# =============================================================================
# build_gap_panel.R
#
# Builds the fund x period model frame for the human-vs-machine alpha-gap
# regressions (see DESIGN_NOTE.md). Standalone: never sourced by plots.R, and
# never reads anything plots.R writes.
#
#   build_gap_panel(ml_index)  ->  one row per (code, period)
#   build_state_panel(...)     ->  one row per period
#
# -----------------------------------------------------------------------------
# VARIABLE TAXONOMY -- four groups, because each enters the econometrics
# differently. Getting this wrong is the main way this design fails.
#
#   A. SCALE       controls, but ENDOGENOUS to past performance (Berk-Green).
#                  fund PL, gestora PL, scaled capacity, flow volatility.
#   B. COSTS       the cleanest controls available: near-exogenous to period-t
#                  skill, mechanically subtractive. Better still, difference
#                  them out via the gross/net decomposition.
#   C. ENVIRONMENT market-level, COMMON across funds at each t. Main effects are
#                  collinear with time FE; INTERACTIONS with fund traits are not
#                  and are far better powered.
#   D. REVEALED STRATEGY  what the manager chose: idiosyncratic risk, tracking
#                  error, alignment with the machine. These are MEDIATORS.
#                  Skill acts THROUGH them, so putting them on the RHS of the
#                  main gap regression destroys the effect being measured.
#                  Describe them, decompose through them, never control on them.
#
# Two specifications follow from this (DESIGN_NOTE.md S7):
#   Spec 1 (headline)   no time FE, two-way clustered SEs. The machine stays in
#                       the DV; state main effects identified off ~102 periods.
#   Spec 2 (robustness) time FE. State main effects drop out BY CONSTRUCTION --
#                       time FE absorb ra_ml(t) entirely -- but interactions and
#                       fund-trait coefficients survive and are immune to any
#                       common shock.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readxl)
  library(tibble); library(here); library(rlang); library(zoo)
})

DATA <- here::here("data")
OUT  <- here::here("data", "derived")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

ROLL_WINDOW <- 36L   # matches min_fund_months (plots.R:170)
MAX_LEVEL_AGE_DAYS <- 15L

# Benchmark scheme for the DV. One of ML_SCHEMES, or "average" to equal-weight
# all eight construction schemes. "average" is the safer headline: it cannot be
# read as an optimiser chosen after seeing the results, and it matches the claim
# the paper actually wants to make -- about systematic investing, not about one
# optimiser. Run mvo_box (and the rest) as the H7 complexity/construction ladder.
ML_BENCHMARK_DEFAULT <- "average"

# ---- Data screens ----------------------------------------------------------
# Each constant below exists because the unscreened build produced values that
# are impossible rather than merely extreme. Counts are from the 2026-08-04 run
# and are reported by `screen_report()` on every build so drift is visible.

# flow_rate = net_flow / lag(aum) exploded to 3.4e10 because 414 fund-periods
# had lagged AUM below R$1k and 16 had exactly zero. A shell fund's flow is not
# economically comparable to a real one's, so below the floor the RATE is NA
# (the level, net_flow, is untouched).
MIN_AUM_FOR_FLOW  <- 100      # R$ thousands
FLOW_RATE_CAP     <- 5        # +/- 500% per window; 50 obs exceeded |10|

# Brazilian equity funds do not charge 750% p.a. Median is 1.5. Values above
# the cap are a units/entry error, not a fee, so they become NA rather than
# being winsorised into a plausible-looking number.
MAX_EXPENSE_RATIO <- 10       # percent p.a.

# HHI <= 1 by construction for a book whose weights sum to <= 100%. 21,040
# fund-periods breached it, far more than the 18 malformed snapshots already
# excluded, because leveraged/short books were never gated. hhi_top15 and
# eff_bets_max now take the same NAV gate as resid_weight and n_pos_min.
# (Gate value is TOPX_NAV_TOLERANCE_PCT, defined with the other topX constants.)


# =============================================================================
# S1. Economatica readers
# =============================================================================
# Verified layout (2026-08-03): metadata rows 1-3, header row 4, "-" for
# missing, 1,943 fund columns, POSIXct date column. The ANBIMA code is the last
# newline-separated token of each header cell:
#   "Patrimônio\nEm moeda orig\nem milhares\n603511"  ->  "603511"
#
# Codes are NOT all numeric (~160 are alphanumeric, e.g. C0001368958), so never
# filter on ^[0-9]+$.

DAILY_FILES <- tibble::tribble(
  ~key,          ~file,                       ~unit,
  "ret",         "bases_fundos_diarios.xlsx", "percent",
  "aum",         "aum_diarios.xlsx",          "BRL thousands",
  "net_flow",    "flow_diarios.xlsx",         "BRL thousands",
  "n_investors", "n_investors_diarios.xlsx",  "count"
)

# The evaluation dates must be the actual ML backtest endpoints—not dates
# inferred from the fund export—so human and machine returns share precisely
# the same 15th-to-15th windows. This is the pinned paper-run input described
# in data/meta_port_ind_signals/data.txt.
ML_BACKTEST_FILE <- file.path(
  DATA, "meta_port_ind_signals",
  "gold_2024_05_comdinheiro_meta_port_backtest_results_bundles.qs"
)

extract_code <- function(x) sub("^.*\n", "", x)

read_economatica_daily <- function(file) {
  path   <- file.path(DATA, file)
  header <- readxl::read_excel(path, skip = 3, n_max = 0, .name_repair = "minimal")

  raw <- readxl::read_excel(
    path, skip = 3, na = "-",
    col_types = c("date", rep("numeric", ncol(header) - 1L)),
    .name_repair = "minimal"
  )

  codes <- extract_code(names(header)[-1])
  if (anyDuplicated(codes)) {
    rlang::abort(paste0(file, ": duplicate fund codes."))
  }

  dates <- as.Date(raw[[1]])
  if (anyNA(dates) || anyDuplicated(dates) || is.unsorted(dates, strictly = TRUE)) {
    rlang::abort(paste0(file, ": dates must be non-missing, unique, and sorted."))
  }

  values <- as.matrix(raw[, -1, drop = FALSE])
  if (any(!is.finite(values) & !is.na(values))) {
    rlang::abort(paste0(file, ": values may be missing but not infinite."))
  }
  colnames(values) <- codes
  list(dates = dates, values = values, codes = codes)
}

# Read all four once, keyed by name.
read_all_daily <- function() {
  purrr::set_names(DAILY_FILES$file, DAILY_FILES$key) %>%
    purrr::map(read_economatica_daily)
}

# Read the ML evaluation index without relying on plots.R having been sourced.
# `qs` stored the xts date index numerically in some paper-run artefacts, so
# restore it explicitly before validating the calendar convention below.
read_ml_index <- function(path = ML_BACKTEST_FILE) {
  if (!file.exists(path)) {
    rlang::abort(sprintf("Pinned ML backtest file does not exist: %s", path))
  }
  if (!requireNamespace("qs", quietly = TRUE) ||
      !requireNamespace("factoRverse", quietly = TRUE)) {
    rlang::abort("read_ml_index() requires the qs and factoRverse packages.")
  }

  meta_result <- qs::qread(path)
  backtest <- meta_result$backtest_meta_portfolios_results
  if (is.null(backtest) || is.null(backtest$ibov_port_backtest_cohort)) {
    rlang::abort("Pinned ML backtest is missing ibov_port_backtest_cohort.")
  }
  cohort <- backtest$ibov_port_backtest_cohort
  if (!base::isS4(cohort) ||
      !("port_returns_m_xts_list" %in% methods::slotNames(cohort))) {
    rlang::abort("ibov_port_backtest_cohort has an unexpected structure.")
  }
  net_returns <- cohort@port_returns_m_xts_list$net_returns_m_xts
  if (!base::isS4(net_returns) ||
      !("data" %in% methods::slotNames(net_returns))) {
    rlang::abort("ML net-return series has an unexpected structure.")
  }

  index <- zoo::index(net_returns@data)
  if (is.numeric(index)) {
    index <- as.Date(
      as.POSIXct(as.numeric(index), origin = "1970-01-01", tz = "UTC")
    )
  } else {
    index <- as.Date(index)
  }
  validate_ml_index(index)
}


# =============================================================================
# S2. Code reconciliation
# =============================================================================
# *** The returns panel and the AUM/flow/investor panels DO NOT share a fund
# *** order, and their fund sets differ slightly. Measured 2026-08-03:
# ***   - 848 of 1,943 positions differ between returns and aum
# ***   - 5 codes appear only in returns, 5 only in aum
# ***   - aum / flow / n_investors agree with each other exactly
# ***
# *** Positional joining would therefore have corrupted ~44% of the panel
# *** SILENTLY. Always join on `code`. This function makes that explicit and
# *** reports the intersection actually available for analysis.

reconcile_codes <- function(daily) {
  code_sets <- purrr::map(daily, "codes")
  common    <- purrr::reduce(code_sets, intersect)

  purrr::iwalk(code_sets, function(cd, nm) {
    rlang::inform(sprintf("  %-12s %4d codes, %4d dropped by intersection",
                          nm, length(cd), length(setdiff(cd, common))))
  })
  rlang::inform(sprintf("  -> %d funds usable across all four panels", length(common)))

  sort(common)
}


# =============================================================================
# S3. Cadastral attributes  (verified against the real file)
# =============================================================================
# dados_cadastrais.xlsx: 1,945 funds x 38 usable columns (39-93 are empty).
# The sheet's own column names are blank and the REAL header sits in row 1 of
# the data, so read it then drop that row.

CAD_COLS <- c(fund_name = 2L, status = 6L, code = 8L, anbima_class = 9L, efpc = 10L,
              multigestor = 11L, fundo_cotas = 12L, perf_fee = 13L,
              investor_type = 14L, leveraged = 15L, redemption = 17L,
              gestora = 18L, administradora = 19L, taxa_adm = 22L,
              inception = 25L)

# Curated, owner-supplied validation set. These are known Brazilian systematic
# or factor-based funds, resolved against the 2026-08-04 cadastral snapshot by
# ANBIMA code. This is a diagnostic label, never a classifier for the paper's
# main estimand; the code check fails loudly if a future source snapshot drops
# one of these ANBIMA codes.
QUANT_VALIDATION_CODES <- c(
  "156019", "155942", "546534", "520845", "521280", "227102",
  "599603", "263435", "C0001245902", "579491", "579572", "596531",
  "C0001244914", "668117", "667501", "668125", "528404", "410322",
  "732079", "506060"
)

BANK_PATTERN <- paste0(
  "ita.|bradesco|banco do brasil|santander|caixa|safra|btg|banrisul|",
  "votorantim|daycoval|bs2|inter\\b|sicredi|sicoob"
)

parse_redemption <- function(x) {            # "D+003" -> 3   (64 distinct values)
  suppressWarnings(as.numeric(sub("^D\\+", "", trimws(as.character(x)))))
}

parse_excel_date <- function(x) {            # Excel serials stored as TEXT
  as.Date(suppressWarnings(as.numeric(as.character(x))), origin = "1899-12-30")
}

add_quant_validation_label <- function(static_df) {
  if (!all(c("code", "fund_name") %in% names(static_df)) || anyDuplicated(static_df$code)) {
    rlang::abort("Static fund data must have unique code and fund_name columns.")
  }
  static_df %>%
    dplyr::mutate(quant_validation_label = .data$code %in% QUANT_VALIDATION_CODES)
}

assert_quant_validation_labels <- function(static_df) {
  found <- static_df$code[static_df$quant_validation_label]
  missing <- setdiff(QUANT_VALIDATION_CODES, found)
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "Curated quant validation code(s) missing from cadastral data: ",
      paste(missing, collapse = ", ")
    ))
  }
  if (sum(static_df$quant_validation_label) != length(QUANT_VALIDATION_CODES)) {
    rlang::abort("Curated quant validation labels are not one-to-one with ANBIMA codes.")
  }
  invisible(TRUE)
}

build_static <- function() {
  raw <- readxl::read_excel(file.path(DATA, "dados_cadastrais.xlsx"),
                            .name_repair = "minimal")

  static <- CAD_COLS %>%
    purrr::map(~ trimws(as.character(raw[[.x]]))) %>%
    tibble::as_tibble() %>%
    dplyr::slice(-1) %>%                              # drop header-as-data row
    dplyr::filter(!is.na(code), code != "") %>%
    dplyr::transmute(
      code,
      fund_name,
      anbima_class,
      gestora,
      administradora,
      status          = tolower(status),              # ativo | cancelado
      efpc            = efpc          %in% c("Sim", "S"),
      perf_fee        = perf_fee      %in% c("Sim", "S"),
      leveraged       = leveraged     %in% c("S", "Sim"),
      investor_type   = investor_type,                # Geral|Qualificado|Profissional
      redemption_days = parse_redemption(redemption),
      # SCREEN 2: 306 funds report a "fee" above 10% p.a., topping out at 750.
      # That is a units or data-entry fault, not a fee schedule, so it becomes
      # NA. Winsorising instead would silently invent a plausible number.
      expense_ratio   = {
        v <- suppressWarnings(as.numeric(taxa_adm))
        dplyr::if_else(!is.na(v) & v >= 0 & v <= MAX_EXPENSE_RATIO, v, NA_real_)
      },
      inception       = parse_excel_date(inception),

      # Feeder / mirror is DIRECTLY observed -- no name heuristic.
      # Real counts: Feeder 573, Espelho 161, Multigestor 29, N/A 1001, "-" 180.
      is_feeder = multigestor %in% c("Feeder", "Espelho"),

      # Owner's rule. Caps out at 29 funds, so any "average human fund-of-funds"
      # benchmark rests on a very thin cross-section -- report N or drop it.
      is_fof = fundo_cotas == "Sim" & multigestor == "Multigestor",

      # Bank affiliation is an asset-manager characteristic. Administrators
      # are excluded: custody/administration does not identify the manager's
      # investment process or captive-management-company channel.
      bank_affiliated = grepl(BANK_PATTERN, paste(gestora),
                              ignore.case = TRUE),

      # Diagnostic only (DESIGN_NOTE.md S9). A keyword flag is deliberately
      # broad and non-authoritative; `quant_validation_label` below is the
      # curated owner-supplied set used solely to validate this diagnostic.
      name_quant_keyword = grepl("quantitativ|sistem.tic|multifator",
                                 paste(fund_name, gestora, anbima_class), ignore.case = TRUE)
    ) %>%
    add_quant_validation_label() %>%
    dplyr::mutate(
      name_quant = quant_validation_label | name_quant_keyword,
      anbima_class_ext = dplyr::case_when(
        name_quant ~ "Quantitativo",
        is_fof     ~ "Multifundos",
        TRUE       ~ anbima_class
      )
    )

  assert_quant_validation_labels(static)
  static
}

build_gestora_static <- function(static_df) {
  static_df %>%
    dplyr::group_by(gestora) %>%
    dplyr::summarise(
      gestora_inception = suppressWarnings(min(inception, na.rm = TRUE)),
      gestora_n_funds   = dplyr::n(),          # in-sample count, not the true count
      .groups = "drop"
    )
}


# =============================================================================
# S4. Duplicate-strategy screen
# =============================================================================
# 734 of 1,945 funds (38%) are Feeder or Espelho vehicles. Masters were already
# excluded upstream, so master/feeder double-counting is not the risk. The real
# risk is that SEVERAL FEEDERS INTO THE SAME MASTER produce near-identical
# return series that look like independent funds -- inflating the cross-section
# and understating standard errors.
#
# No master ID exists in the cadastral file, so detect duplicates from returns:
# near-unit correlation over a decent overlap.  Clusters are connected
# components, not just direct matches to the first fund encountered: if A~B
# and B~C, all three belong to one cluster even when A~C falls below the cut.
# This function RETAINS every legal fund identity.  Collapse to one return
# representative only for a strategy-level performance estimand; retain and
# aggregate all vehicles for AUM and flow analyses.

find_duplicate_clusters <- function(ret_mat, min_overlap = 24L, rho_cut = 0.999) {
  if (!is.matrix(ret_mat) || !is.numeric(ret_mat)) {
    rlang::abort("ret_mat must be a numeric matrix.")
  }
  codes <- colnames(ret_mat)
  n     <- length(codes)
  if (n == 0L || anyNA(codes) || any(!nzchar(codes)) || anyDuplicated(codes)) {
    rlang::abort("ret_mat must have non-empty, unique column names.")
  }
  if (length(min_overlap) != 1L || !is.finite(min_overlap) ||
      min_overlap < 2L || min_overlap != as.integer(min_overlap)) {
    rlang::abort("min_overlap must be an integer of at least 2.")
  }
  if (length(rho_cut) != 1L || !is.finite(rho_cut) || rho_cut < -1 || rho_cut > 1) {
    rlang::abort("rho_cut must lie between -1 and 1.")
  }

  # `component` implements union-by-relabeling.  For this ~2,000-fund use
  # case it is transparent, and unlike one-pass owner assignment it is
  # transitive.
  component <- seq_len(n)

  if (n > 1L) for (i in seq_len(n - 1L)) {
    xi <- ret_mat[, i]
    for (j in (i + 1L):n) {
      if (component[i] == component[j]) next
      ok <- is.finite(xi) & is.finite(ret_mat[, j])
      if (sum(ok) < min_overlap) next
      r <- suppressWarnings(stats::cor(xi[ok], ret_mat[ok, j]))
      if (is.finite(r) && r >= rho_cut) {
        component[component == component[j]] <- component[i]
      }
    }
  }

  # A lexicographic representative makes cluster labels independent of the
  # order in which the source columns happened to arrive.
  cluster_label <- vapply(
    split(codes, component),
    min,
    character(1)
  )
  dup_cluster <- unname(cluster_label[as.character(component)])
  cluster_size <- as.integer(table(dup_cluster)[dup_cluster])

  tibble::tibble(
    code = codes,
    dup_cluster,
    cluster_size,
    is_duplicate = cluster_size > 1L
  )
}

# Optional strategy-level return representative: longest history, then cheapest.
# Do not apply this selection to legal-fund AUM or flow aggregates.
pick_cluster_representative <- function(dup_df, static_df, obs_count) {
  dup_df %>%
    dplyr::left_join(tibble::enframe(obs_count, "code", "n_obs"), by = "code") %>%
    dplyr::left_join(dplyr::select(static_df, code, expense_ratio), by = "code") %>%
    dplyr::group_by(dup_cluster) %>%
    dplyr::arrange(dplyr::desc(n_obs), expense_ratio, .by_group = TRUE) %>%
    dplyr::mutate(is_representative = dplyr::row_number() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(code, dup_cluster, is_representative)
}


# =============================================================================
# S5. Fund-level time-varying covariates  (GROUP A)
# =============================================================================
# `ml_index` labels the END of each return window.  For example, the return
# labelled 2016-01-15 covers (2015-12-15, 2016-01-15].  Therefore AUM and
# investor counts for that row are sampled at 2015-12-15, the window OPENING:
# sampling them on 2016-01-15 would embed the period's realised return and
# flows.  Daily data lets this timing be exact rather than relying on a
# month-end proxy.
#
# Flows need care. Net flow DURING a window is caused by return during that
# window (performance chasing), so only LAGGED window flow is a predictor.
# Contemporaneous flow is an outcome and needs its own identification argument.

# Level as of each window opening: last observation at or before the date.
# A daily feed should be no more than a few calendar days stale around a market
# holiday.  Abort rather than quietly treating an old level as opening AUM.
sample_at <- function(obj, when, codes, max_age_days = MAX_LEVEL_AGE_DAYS) {
  when <- as.Date(when)
  if (anyNA(when) || anyDuplicated(when) || is.unsorted(when, strictly = TRUE)) {
    rlang::abort("when must contain non-missing, unique, sorted dates.")
  }
  if (length(max_age_days) != 1L || !is.finite(max_age_days) ||
      max_age_days < 0 || max_age_days != as.integer(max_age_days)) {
    rlang::abort("max_age_days must be a non-negative integer.")
  }
  if (is.null(obj$dates) || anyNA(obj$dates) || anyDuplicated(obj$dates) ||
      is.unsorted(obj$dates, strictly = TRUE)) {
    rlang::abort("Daily-panel dates must be non-missing, unique, and sorted.")
  }

  idx <- findInterval(when, obj$dates)
  out <- matrix(NA_real_, length(when), length(codes),
                dimnames = list(NULL, codes))
  hit <- idx > 0
  age_days <- as.integer(when[hit] - obj$dates[idx[hit]])
  if (any(age_days > max_age_days)) {
    first <- which(hit)[which(age_days > max_age_days)[1L]]
    rlang::abort(sprintf(
      "Latest daily level is %d days old at opening date %s (maximum allowed: %d).",
      age_days[which(age_days > max_age_days)[1L]], as.character(when[first]), max_age_days
    ))
  }
  out[hit, ] <- obj$values[idx[hit], codes, drop = FALSE]
  out
}

# Validate the exact calendar convention used by the ML backtest and the
# 15th-to-15th fund-return windows.
validate_ml_index <- function(period_ends) {
  period_ends <- as.Date(period_ends)
  if (length(period_ends) == 0L || anyNA(period_ends) ||
      anyDuplicated(period_ends) || is.unsorted(period_ends, strictly = TRUE) ||
      any(format(period_ends, "%d") != "15")) {
    rlang::abort("period_ends must be non-missing, consecutive calendar 15ths.")
  }
  expected <- seq.Date(min(period_ends), max(period_ends), by = "month")
  if (length(period_ends) != length(expected) || any(period_ends != expected)) {
    rlang::abort("period_ends must be non-missing, consecutive calendar 15ths.")
  }
  period_ends
}

# Return-window opening dates for periods labelled by their closing 15th.
window_openings <- function(period_ends) {
  period_ends <- validate_ml_index(period_ends)
  c(as.Date(format(period_ends[[1]] - 20, "%Y-%m-15")),
    period_ends[-length(period_ends)])
}

# Flow summed over (window opening, window closing]. This is contemporaneous
# with the labelled return; only its lag is eligible as a predictor.
sum_over_window <- function(obj, period_ends, codes) {
  openings <- window_openings(period_ends)
  purrr::map2(openings, period_ends, function(from, to) {
    sel <- obj$dates > from & obj$dates <= to
    if (!any(sel)) return(rep(NA_real_, length(codes)))
    window_values <- obj$values[sel, codes, drop = FALSE]
    out <- colSums(window_values, na.rm = TRUE)
    # Keep an unreported fund-window missing. Only the aggregate functions
    # deliberately convert an all-missing gestora or industry period to zero.
    out[colSums(!is.na(window_values)) == 0L] <- NA_real_
    out
  }) %>%
    purrr::reduce(rbind) %>%
    matrix(nrow = length(period_ends), dimnames = list(NULL, codes))
}

matrix_to_long <- function(mat, when, value_name) {
  tibble::as_tibble(mat) %>%
    dplyr::mutate(period = when) %>%
    tidyr::pivot_longer(-period, names_to = "code", values_to = value_name)
}

# Make the missing-data policy explicit.  A gestora or period with no reported
# values has zero observed AUM/flow in this constructed panel, rather than an
# accidental zero produced only as a side effect of `sum(..., na.rm = TRUE)`.
sum_or_zero <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(0)
  sum(x, na.rm = TRUE)
}

build_fund_timevarying <- function(daily, ml_index, codes) {
 
  openings <- window_openings(ml_index)

  levels_long <- list(aum = "aum", n_investors = "n_investors") %>%
    purrr::imap(~ sample_at(daily[[.x]], openings, codes) %>%
                  matrix_to_long(ml_index, .y))

  flow_long <- sum_over_window(daily$net_flow, ml_index, codes) %>%
    matrix_to_long(ml_index, "net_flow")

  purrr::reduce(c(levels_long, list(flow_long)),
                dplyr::full_join, by = c("period", "code")) %>%
    dplyr::arrange(code, period) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(
      # AUM is already the period-opening value, so it is the economically
      # correct denominator for the current window's flow rate.
      # SCREEN 1: a shell fund's flow rate is not comparable to a real fund's.
      # Below the AUM floor the RATE is undefined; the LEVEL (net_flow) stays.
      flow_rate     = dplyr::if_else(aum >= MIN_AUM_FOR_FLOW,
                                     net_flow / aum, NA_real_),
      flow_rate     = pmin(pmax(flow_rate, -FLOW_RATE_CAP), FLOW_RATE_CAP),
      flow_rate_lag = dplyr::lag(flow_rate),          # the usable predictor
      flow_vol_12   = zoo::rollapplyr(flow_rate_lag, 12, stats::sd,
                                       fill = NA, na.rm = TRUE),
      # log() of a zero-AUM fund is not -18.42, it is undefined. The old pmax
      # floor manufactured a hard outlier at every dormant fund-period.
      log_pl_fundo  = dplyr::if_else(aum > 0, log(aum), NA_real_),
      log_investors = dplyr::if_else(n_investors > 0, log(n_investors), NA_real_)
    ) %>%
    dplyr::ungroup()
}

build_gestora_timevarying <- function(fund_tv, static_df) {
  fund_tv %>%
    dplyr::left_join(dplyr::select(static_df, code, gestora), by = "code") %>%
    dplyr::filter(!is.na(gestora)) %>%
    dplyr::group_by(gestora, period) %>%
    dplyr::summarise(
      pl_gestora        = sum_or_zero(aum),
      gestora_net_flow  = sum_or_zero(net_flow),
      n_funds_reporting = sum(!is.na(aum)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      log_pl_gestora = dplyr::if_else(pl_gestora > 0, log(pl_gestora), NA_real_)
    )
}


# =============================================================================
# S5b. Portfolio concentration from topX_w  (GROUP D -- mediators)
# =============================================================================
# top1_w .. top15_w hold the weight of the k-TH LARGEST position individually
# ("6.Maior Investimento"), NOT the cumulative top-k. Monthly, 103 periods,
# 1,944 funds, values in percent, dates as Portuguese month labels ("Nov-2015").
#
# This is partial holdings data, which upgrades the Fundamental Law's breadth
# term from "unmeasurable from returns" to "partially measurable" -- see
# DESIGN_NOTE.md S4.1.
#
# WHAT IS SAFE TO COMPUTE
#   w_cum_k      net cumulative weight of the k largest positions
#   hhi_top15    sum of squared signed top-15 NAV weights; a lower bound on
#                the partial-portfolio HHI even when the fund has shorts
#   resid_weight 100 - long_exposure_top15, only for a non-short,
#                NAV-normalised book
#   n_pos_min    HARD lower bound on the number of positions, no distributional
#                assumption: every unseen position is at most w_(15), so
#                  n_pos_min = 15 + ceiling(resid_weight / w_15)
#                This is the referee-proof breadth measure. Prefer it.
#   decay_slope  OLS slope beta in log w_(k) = a + beta log(k): a descriptive
#                power-law shape of the positive top-15 weights. More-negative
#                beta means weight declines faster with rank (more top-heavy);
#                beta near zero is flatter. It is not turnover, a raw adjacent
#                weight-change ratio, or a definitive quant classifier.
#
# WHAT IS **NOT** RECOVERABLE: TURNOVER.
#   Changes in w_(k) over time are dominated by price drift, not trading: a top
#   holding that outperforms gains weight with zero transactions. Worse, the
#   IDENTITY of position k changes between months, so a difference compares
#   different stocks. Worst of all, the bias correlates with the outcome -- a
#   fund whose top pick soars shows both large "turnover" and high alpha, so any
#   turnover-performance relation built this way is spurious by construction.
#   Do not build a turnover proxy from these files.
#
#   The salvageable cousin is PROFILE STABILITY: the time-series volatility of
#   decay_slope. That measures how much the portfolio's shape shifts, which is
#   closer to strategy stability than to turnover, and it is not mechanically
#   tied to performance.
#
# INTERPRETATION OF decay_slope
#   Under an exact power law, w_b / w_a = (b / a)^beta; it is therefore an
#   elasticity across ranks, not a single adjacent-rank ratio. It is useful as
#   a descriptive mediator/proxy for the shape of the long side of a portfolio.
#   Any association with systematic management is an empirical hypothesis, not
#   a classification rule; control for mandate and fund size in that analysis.

TOPX_MAX <- 15L
TOPX_NAV_TOLERANCE_PCT <- 2

# Materiality floor for calling a negative weight a SHORT.
#
# Economatica's field is "Val Inv (det atv)" -- an asset detail line, not a
# position book. Treating every negative entry as a short flagged 62.2% of all
# fund-periods, including 986 funds whose cadastral `Alavancado` flag is "N" and
# 3,992 fund-months of `Acoes Indexados` (index funds do not short). The median
# flagged weight is -0.27% and the 75th percentile is -0.15%: these are
# bookkeeping lines (negative cash, margin, payables), not investment positions.
#
# Because has_short gates hhi_top15, resid_weight, n_pos_min and eff_bets_max,
# the unfiltered flag was withholding most of the concentration data over
# entries too small to move any of those bounds. Only negatives at or beyond
# this threshold count as shorts; smaller ones are treated as immaterial.
SHORT_MATERIALITY_PCT <- 1

# Ceiling for the minimum-position bound. B3 lists a few hundred tradable names
# and a Brazilian equity fund cannot hold more than the universe, so an implied
# count above this is a division-by-a-rounding-error artefact, not a portfolio.
MAX_PLAUSIBLE_POSITIONS <- 400
TOPX_SOFT_CUM_WEIGHT_PCT <- 110
# Gross exposure can exceed NAV for levered portfolios, but a four-digit
# single-position percentage is not a credible holdings weight. It indicates
# a corrupted source cell, not leverage.
TOPX_MAX_ABS_WEIGHT_PCT <- 1000

PT_MONTHS <- c(Jan = 1, Fev = 2, Mar = 3, Abr = 4,  Mai = 5,  Jun = 6,
               Jul = 7, Ago = 8, Set = 9, Out = 10, Nov = 11, Dez = 12)

# Vectorised month arithmetic without a lubridate dependency.
shift_month <- function(d, n = 1L) {
  k <- as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m")) - 1L + n
  as.Date(sprintf("%d-%02d-01", k %/% 12L, k %% 12L + 1L))
}

parse_pt_month <- function(x) {              # "Nov-2015" -> 2015-11-01
  parts <- strsplit(as.character(x), "-", fixed = TRUE)
  mon <- vapply(parts, `[`, character(1), 1L)
  yr  <- vapply(parts, `[`, character(1), 2L)
  m   <- PT_MONTHS[mon]
  if (anyNA(m)) {
    rlang::abort(paste0("Unrecognised month(s): ",
                        paste(unique(mon[is.na(m)]), collapse = ", ")))
  }
  as.Date(sprintf("%s-%02d-01", yr, m))
}

# Economatica repeats S0001198076 in ranks 2--15 of the current extract, but
# both copies are entirely blank. Retain a single populated copy if there is
# one; fail on two populated copies because choosing one would corrupt the
# fund-to-holdings alignment.
resolve_topx_duplicate_columns <- function(raw, codes, rank) {
  browser()
  if (ncol(raw) != length(codes) + 1L || anyNA(codes) || any(!nzchar(codes))) {
    rlang::abort("Top-X workbook has malformed fund-code headers.")
  }
  duplicate_codes <- unique(codes[duplicated(codes)])
  if (length(duplicate_codes) == 0L) {
    return(list(raw = raw, codes = codes))
  }

  keep <- rep(TRUE, length(codes))
  for (code in duplicate_codes) {
    columns <- which(codes == code)
    populated <- vapply(columns, function(i) any(!is.na(raw[[i + 1L]])), logical(1))
    if (sum(populated) > 1L) {
      rlang::abort(sprintf(
        "top%d_w.xlsx has multiple populated columns for duplicated code %s.",
        rank, code
      ))
    }
    keep[columns] <- FALSE
    if (sum(populated) == 1L) keep[columns[populated]] <- TRUE
  }

  list(raw = raw[, c(TRUE, keep), drop = FALSE], codes = codes[keep])
}

read_topx_rank <- function(k) {
  
  path   <- file.path(DATA, sprintf("top%d_w.xlsx", k))
  header <- readxl::read_excel(path, skip = 3, n_max = 0, .name_repair = "minimal")
  raw    <- readxl::read_excel(path, skip = 3, na = "-",
                               col_types = c("text", rep("numeric", ncol(header) - 1L)),
                               .name_repair = "minimal")

  resolved <- resolve_topx_duplicate_columns(raw, extract_code(names(header)[-1]), k)
  raw <- resolved$raw
  values <- as.matrix(raw[, -1, drop = FALSE])
  colnames(values) <- resolved$codes

  tibble::as_tibble(values) %>%
    dplyr::mutate(month = parse_pt_month(raw[[1]])) %>%
    tidyr::pivot_longer(-month, names_to = "code", values_to = "w") %>%
    dplyr::mutate(rank = k)
}

# Long frame: one row per (code, month, rank).
read_topx_all <- function(max_rank = TOPX_MAX) {
  seq_len(max_rank) %>%
    purrr::map(read_topx_rank) %>%
    purrr::list_rbind() %>%
    dplyr::filter(!is.na(w))
}

# Validate the partial-holdings input without treating modest NAV overages as
# impossible. A leveraged fund can legitimately have gross long exposure above
# 100%; the 2pp tolerance also absorbs rounded reported weights. Above that
# tolerance, lower-bound position counts are not identified from these fields.
validate_concentration_input <- function(topx_long,
                                         soft_cum_weight = TOPX_SOFT_CUM_WEIGHT_PCT,
                                         max_abs_weight = TOPX_MAX_ABS_WEIGHT_PCT,
                                         invalid_snapshot = c("error", "exclude")) {
  invalid_snapshot <- match.arg(invalid_snapshot)
  required <- c("code", "month", "rank", "w")
  if (!is.data.frame(topx_long) || !all(required %in% names(topx_long)) ||
      nrow(topx_long) == 0L) {
    rlang::abort("topx_long must be a non-empty data frame with code, month, rank, and w.")
  }
  if (anyNA(topx_long$code) || any(!nzchar(topx_long$code)) ||
      anyNA(topx_long$month) || any(!is.finite(topx_long$rank)) ||
      any(!is.finite(topx_long$w)) || any(topx_long$rank < 1L) ||
      any(topx_long$rank > TOPX_MAX) || any(topx_long$rank != as.integer(topx_long$rank)) ||
      anyDuplicated(topx_long[c("code", "month", "rank")])) {
    rlang::abort("Top-X holdings must have unique code-month-rank keys, ranks 1..TOPX_MAX, and finite weights.")
  }
  if (length(soft_cum_weight) != 1L || !is.finite(soft_cum_weight) ||
      soft_cum_weight <= 100) {
    rlang::abort("soft_cum_weight must be a finite percentage above 100.")
  }
  if (length(max_abs_weight) != 1L || !is.finite(max_abs_weight) ||
      max_abs_weight <= 100) {
    rlang::abort("max_abs_weight must be a finite percentage above 100.")
  }

  diagnostics <- topx_long %>%
    dplyr::arrange(code, month, rank) %>%
    dplyr::group_by(code, month) %>%
    dplyr::summarise(
      ranks_contiguous = all(rank == seq_len(dplyr::n())),
      weights_descend  = all(diff(w) <= 1e-8),
      long_exposure    = sum(pmax(w, 0)),
      .groups = "drop"
    )

  extreme_keys <- topx_long %>%
    dplyr::filter(abs(w) > max_abs_weight) %>%
    dplyr::distinct(code, month)
  malformed_keys <- diagnostics %>%
    dplyr::filter(!ranks_contiguous | !weights_descend) %>%
    dplyr::select(code, month)
  invalid_keys <- dplyr::bind_rows(extreme_keys, malformed_keys) %>%
    dplyr::distinct(code, month)

  if (nrow(invalid_keys) > 0L) {
    message <- sprintf(
      "%d top-X fund-month snapshot(s) are malformed (%d with |weight| above %.0f%%; %d with invalid rank/order).",
      nrow(invalid_keys), nrow(extreme_keys), max_abs_weight, nrow(malformed_keys)
    )
    if (identical(invalid_snapshot, "error")) rlang::abort(message)
    rlang::warn(paste(message, "Excluding their concentration measures."))
    topx_long <- dplyr::anti_join(topx_long, invalid_keys, by = c("code", "month"))
    diagnostics <- dplyr::anti_join(diagnostics, invalid_keys, by = c("code", "month"))
  }

  high_weight <- dplyr::filter(diagnostics, long_exposure > soft_cum_weight)
  if (nrow(high_weight) > 0L) {
    rlang::warn(sprintf(
      "%d fund-month(s) have positive top-%d exposure above %.0f%% of NAV (first: %s, %s). Review leverage or source units.",
      nrow(high_weight), TOPX_MAX, soft_cum_weight,
      high_weight$code[1L], as.character(high_weight$month[1L])
    ))
  }
  topx_long
}

# Fit log w_(k) ~ log k. Needs >= 4 ranks to be meaningful.
fit_decay_slope <- function(rank, w) {
  ok <- is.finite(w) & w > 0 & is.finite(rank) & rank > 0
  if (sum(ok) < 4L) return(NA_real_)
  unname(stats::coef(stats::lm(log(w[ok]) ~ log(rank[ok])))[2])
}

summarise_concentration <- function(topx_long,
                                    invalid_snapshot = c("error", "exclude")) {
  topx_long <- validate_concentration_input(
    topx_long,
    invalid_snapshot = match.arg(invalid_snapshot)
  )

  topx_long %>%
    dplyr::arrange(code, month, rank) %>%
    dplyr::group_by(code, month) %>%
    dplyr::summarise(
      n_ranks     = dplyr::n(),
      w_top1      = w[rank == 1][1],
      w_top5      = sum(w[rank <= 5]),
      w_top10     = sum(w[rank <= 10]),
      w_cum_15    = sum(w),
      long_exposure_top15  = sum(pmax(w, 0)),
      gross_exposure_top15 = sum(abs(w)),
      # Any negative line at all -- kept for diagnostics, NOT used as a gate.
      has_negative_line = any(w < 0),
      # A short big enough to matter. See SHORT_MATERIALITY_PCT.
      has_short   = any(w <= -SHORT_MATERIALITY_PCT),
      min_weight  = suppressWarnings(min(w)),
      # Smallest POSITIVE observed weight. This, not the highest-ranked weight,
      # is the correct denominator for the minimum-position bound: an immaterial
      # negative bookkeeping line at the last rank would otherwise void it.
      w_min_positive = suppressWarnings(min(w[w > 0])),
      w_last      = w[which.max(rank)],
      hhi_raw     = sum((w / 100)^2),
      decay_slope = fit_decay_slope(rank, w),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # A residual-weight / minimum-position bound assumes net long weights
      # measured against NAV. Preserve the inference under minor rounding, but
      # leave it unavailable when gross top-15 exposure materially exceeds NAV.
      weight_over_nav = long_exposure_top15 > 100 + TOPX_NAV_TOLERANCE_PCT,

      # SCREEN 3: HHI <= 1 is an identity for a long book whose weights sum to
      # at most NAV. Ungated, 21,040 fund-periods breached it -- levered and
      # short books, far more than the malformed snapshots already excluded.
      # Same NAV gate as resid_weight / n_pos_min so the three bounds agree on
      # when they are identified.
      hhi_top15 = dplyr::if_else(!weight_over_nav & !has_short, hhi_raw, NA_real_),
      resid_weight = dplyr::if_else(
        weight_over_nav | has_short, NA_real_, pmax(100 - long_exposure_top15, 0)
      ),
      # Hard lower bound, no distributional assumption: every unseen position
      # is at most as large as the smallest observed one. Not identified for a
      # materially levered (top-15 > NAV) book.
      n_pos_min = dplyr::if_else(
        !weight_over_nav & !has_short &
          is.finite(w_min_positive) & w_min_positive > 0,
        n_ranks + ceiling(resid_weight / w_min_positive),
        NA_real_
      ),
      # The bound divides by the smallest observed positive weight, so a
      # near-zero one sends it to absurdity: the raw series had a mean of 6.8e4
      # against a median of 20. Beyond a plausible investable universe the bound
      # is still technically valid but carries no information, so it is dropped
      # rather than winsorised into a number someone might read as a holding count.
      n_pos_min = dplyr::if_else(n_pos_min <= MAX_PLAUSIBLE_POSITIONS,
                                 n_pos_min, NA_real_),
      eff_bets_max = dplyr::if_else(
        !is.na(hhi_top15) & hhi_top15 > 0, 1 / hhi_top15, NA_real_
      )
    ) %>%
    dplyr::select(-hhi_raw)
}

# `period` labels the CLOSING 15th of a return window. A month-end holdings
# snapshot is attached to the first *full* 15th-to-15th return window that
# starts after the snapshot is observable. For example, a November month-end
# snapshot maps to the return labelled 15 January, not 15 December: the latter
# window began on 15 November and would partially precede the snapshot.
build_concentration_panel <- function(ml_index, max_rank = TOPX_MAX) {
  conc <- read_topx_all(max_rank) %>%
    summarise_concentration(invalid_snapshot = "exclude")

  window_for_month <- tibble::tibble(period = ml_index) %>%
    dplyr::mutate(month_key = as.Date(format(period, "%Y-%m-01")))

  conc %>%
    dplyr::mutate(month_key = shift_month(month, 2L)) %>%
    dplyr::inner_join(window_for_month, by = "month_key") %>%
    dplyr::select(-month_key) %>%
    dplyr::arrange(code, period) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(
      # Profile stability -- the defensible cousin of turnover.
      slope_vol_12 = zoo::rollapplyr(decay_slope, 12, stats::sd,
                                     fill = NA, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}


# =============================================================================
# S6. Industry size, built from individual AUMs  (GROUP C)
# =============================================================================
# ANBIMA publishes only year-end aggregates, so the industry series is summed
# from the fund panel instead. That raises two problems, both handled here.
#
# (1) MECHANICAL RETURN CONTAMINATION -- the important one.
#     AUM(t) = AUM(t-1)*(1+r(t)) + flow(t), so summed AUM embeds the cumulative
#     returns of the very funds whose performance is in the DV. Regressing the
#     performance gap on industry AUM is partly regressing returns on cumulative
#     past returns.
#
#     Fix: separate the FLOW component from the RETURN component. Cumulative net
#     flow is the economically meaningful object anyway -- it is capital
#     entering to compete for alpha, which is exactly what Berk-Green and
#     Pastor-Stambaugh-Taylor are about -- and it is far less mechanically tied
#     to the DV. PREFER `industry_flow_cum` as the regressor; keep
#     `industry_pl` for description only.
#
# (2) COVERAGE DRIFT -- the fund count changes as funds enter and exit, so a raw
#     sum grows partly for mechanical reasons. `n_reporting` is carried
#     alongside so coverage can be controlled or a balanced version built.
#
# Summed over ALL funds present, before eligibility screening: the object of
# interest is the size of the industry, not of the estimation sample.

build_industry_panel <- function(fund_tv) {
  # SCREEN 4: coverage drift is not a detail here. Between 2015-12 and 2024-05
  # `n_reporting` rose 605 -> 1,190 (+97%) while `industry_pl` rose +353%, so a
  # large share of apparent industry growth is simply more funds being covered.
  # The balanced series below holds the fund set fixed: only funds reporting AUM
  # in EVERY period contribute, so its movements are size, not coverage. Use it
  # for anything testing the Pastor-Stambaugh-Taylor industry-scale channel.
  balanced_codes <- fund_tv %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(n_obs = sum(!is.na(aum)), .groups = "drop") %>%
    dplyr::filter(n_obs == dplyr::n_distinct(fund_tv$period)) %>%
    dplyr::pull(code)

  balanced <- fund_tv %>%
    dplyr::filter(code %in% balanced_codes) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      industry_pl_balanced = sum_or_zero(aum),
      n_balanced           = dplyr::n(),
      .groups = "drop"
    )

  fund_tv %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      industry_pl       = sum_or_zero(aum),
      industry_net_flow = sum_or_zero(net_flow),
      n_reporting       = sum(!is.na(aum)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(balanced, by = "period") %>%
    dplyr::arrange(period) %>%
    dplyr::mutate(
      # Counterfactual asset base with market appreciation stripped out.
      industry_flow_cum = cumsum(dplyr::coalesce(industry_net_flow, 0)),

      # Flow scaled by lagged size: the standard flow-rate object, and the
      # cleanest of the three for use as a regressor.
      industry_flow_rate = industry_net_flow / dplyr::lag(industry_pl),

      # Coverage-neutral alternative: average fund size. Moves with the size of
      # the typical fund rather than with how many funds Economatica covers.
      industry_pl_per_fund = dplyr::if_else(n_reporting > 0,
                                            industry_pl / n_reporting, NA_real_),

      # Description only -- return-contaminated AND coverage-driven, see above.
      industry_pl_growth = industry_pl / dplyr::lag(industry_pl) - 1,

      log_industry_pl = dplyr::if_else(industry_pl > 0, log(industry_pl), NA_real_),
      log_industry_pl_balanced = dplyr::if_else(
        !is.na(industry_pl_balanced) & industry_pl_balanced > 0,
        log(industry_pl_balanced), NA_real_
      )
    )
}


# =============================================================================
# S7. Environment panel  (GROUP C, period level)
# =============================================================================
# PRE-SPECIFIED (DESIGN_NOTE.md S5): industry size (flow-based) + cross-sectional
# dispersion. Dispersion is the one state variable that is NOT another reading
# of the same macro cycle -- it can be high in booms and busts alike -- so it
# adds independent variation.
#
# Recession is a VALIDATION cut, not an inference variable: CODACE gives very
# few in-sample episodes and clustered SEs cannot rescue a treatment with one
# or two clusters. Count episodes before designing around it.
#
# Selic and market P/E stay in the robustness appendix: both are highly
# persistent, and ~102 periods of persistent regressors invites Stambaugh bias
# and the Ferson-Sarkissian-Simin critique.
#
# Stock-level inputs come from the PINNED gold features -- per
# provenance/README.md use gold_..._features/...-e7a36 (105 signals), NOT
# data/gold_data (111-signal May 2026 gold: silent six-column mismatch).

CODACE_RECESSIONS <- tibble::tribble(
  ~start, ~end
  # TODO(owner): fill from CODACE (FGV) and cite.
)

build_state_panel <- function(ml_index, industry_panel) {
  tibble::tibble(period = ml_index) %>%
    dplyr::left_join(industry_panel, by = "period") %>%
    dplyr::mutate(
      recession   = purrr::map_lgl(period, ~ any(
        .x >= CODACE_RECESSIONS$start & .x <= CODACE_RECESSIONS$end
      )),
      dispersion  = NA_real_,   # TODO from pinned gold
      avg_corr    = NA_real_,
      breadth     = NA_real_,
      adtv_market = NA_real_,
      mkt_vol     = NA_real_,
      selic       = NA_real_    # TODO from CDI.xlsx (robustness only)
    )
}

# =============================================================================
# S8. Dependent variable and mediators  (GROUP D)
# =============================================================================
# The DV does not exist yet. regress_flat() (plots.R:1035) runs ONE full-sample
# regression per fund -- a single alpha, not a fund x period panel.
#
# For fund i in period t:
#   1. six-factor loadings on the trailing ROLL_WINDOW ending at t-1
#      (strictly trailing -- no period-t information);
#   2. ra(i,t) = r(i,t) - rf(t) - b(i,t-1)' f(t)        <- realised residual;
#   3. same for the ML portfolio                        -> ra_ml(t);
#   4. gap(i,t) = ra_ml(t) - ra(i,t).
#
# Realised residuals, NOT rolling alpha estimates: a rolling alpha is a window
# average, so consecutive observations overlap heavily, the effective sample
# size collapses, and every standard error in the paper becomes wrong.

# Factor basis, taken verbatim from plots.R:1031 so the DV is defined on exactly
# the same factors as the published attribution tables.
FF_PREDICTORS <- c("mkt_e", "hml_n", "mom_n", "rmw_n", "smb_n", "cma_n")

# Align a factor_mat (as built at plots.R:1009) to a vector of period labels.
# Returns the design pieces as plain matrices: everything downstream is linear
# algebra, and lm() per fund-period would be ~200k model-frame constructions.
align_factors <- function(ret_ym, factor_mat) {
  browser()
  required <- c("ym", "rf", FF_PREDICTORS)
  if (!is.data.frame(factor_mat) || !all(required %in% names(factor_mat))) {
    rlang::abort(sprintf("factor_mat must contain: %s.",
                         paste(required, collapse = ", ")))
  }
  factor_ym <- as.character(factor_mat$ym)
  if (anyNA(factor_ym) || any(!nzchar(factor_ym)) || anyDuplicated(factor_ym)) {
    rlang::abort("factor_mat$ym must be non-missing and unique.")
  }
  if (!all(vapply(factor_mat[required[-1L]], is.numeric, logical(1)))) {
    rlang::abort("factor_mat$rf and all factor columns must be numeric.")
  }

  ret_ym <- as.character(ret_ym)
  if (anyNA(ret_ym) || any(!nzchar(ret_ym)) || anyDuplicated(ret_ym) ||
      is.unsorted(ret_ym, strictly = TRUE)) {
    rlang::abort("ret_ym must be non-missing, unique, and strictly sorted.")
  }

  idx <- match(ret_ym, factor_ym)
  if (anyNA(idx)) {
    rlang::abort(sprintf("factor_mat is missing %d of %d periods (first: %s).",
                         sum(is.na(idx)), length(idx), ret_ym[which(is.na(idx))[1]]))
  }
  F <- as.matrix(factor_mat[idx, FF_PREDICTORS, drop = FALSE])
  rf <- as.numeric(factor_mat$rf[idx])
  if (any(!is.finite(F)) || any(!is.finite(rf))) {
    rlang::abort("factor_mat has non-finite factor or risk-free values in the requested periods.")
  }
  list(
    F  = F,
    rf = rf
  )
}

# -----------------------------------------------------------------------------
# rolling_risk_adjusted()
#
# For every column of `ret_mat` (periods x series, PERCENT) and every period t:
#
#   1. fit  y_e ~ 1 + F  on the trailing `window` periods ENDING AT t-1;
#   2. return the realised residual at t,
#          ra(t) = y_e(t) - b_hat' F(t)
#
# THE INTERCEPT IS ESTIMATED BUT NOT SUBTRACTED. Two separate jobs: including it
# in the fit keeps the slope estimates unbiased when mean active return is
# non-zero; excluding it from the prediction keeps the fund's PAST alpha out of
# its PRESENT risk-adjusted return. Subtracting it would net out exactly the
# skill the paper is trying to measure.
#
# Strictly trailing -- period t contributes nothing to its own betas -- so `ra`
# carries no look-ahead. The realised period-t outcomes themselves do not
# overlap, unlike rolling-alpha estimates. Their trailing beta fits do share
# history, however, so residuals can still be serially dependent; downstream
# inference must retain fund and period clustering.
# -----------------------------------------------------------------------------
rolling_risk_adjusted <- function(ret_mat, ret_ym, factor_mat,
                                  window = ROLL_WINDOW, min_obs = ROLL_WINDOW) {
  browser()
  if (!is.matrix(ret_mat) || !is.numeric(ret_mat) || ncol(ret_mat) == 0L ||
      nrow(ret_mat) != length(ret_ym)) {
    rlang::abort("ret_mat must be a non-empty numeric matrix with one row per ret_ym period.")
  }
  if (any(!is.finite(ret_mat) & !is.na(ret_mat))) {
    rlang::abort("ret_mat may contain missing values but not infinite values.")
  }
  if (is.null(colnames(ret_mat)) || anyNA(colnames(ret_mat)) ||
      any(!nzchar(colnames(ret_mat))) || anyDuplicated(colnames(ret_mat))) {
    rlang::abort("ret_mat must have non-missing, unique column names.")
  }
  if (length(window) != 1L || !is.finite(window) || window < 1L ||
      window != as.integer(window)) {
    rlang::abort("window must be a positive integer.")
  }
  if (length(min_obs) != 1L || !is.finite(min_obs) || min_obs < 1L ||
      min_obs != as.integer(min_obs) || min_obs > window) {
    rlang::abort("min_obs must be a positive integer no larger than window.")
  }
  if (min_obs <= length(FF_PREDICTORS) + 1L) {
    rlang::abort("min_obs must exceed the number of coefficients (7).")
  }
  if (nrow(ret_mat) <= window) {
    rlang::abort(sprintf("ret_mat needs more than %d periods for a %d-period trailing fit.",
                         window, window))
  }

  fac    <- align_factors(ret_ym, factor_mat)
  y_e    <- sweep(ret_mat, 1L, fac$rf, "-")     # excess over CDI, percent
  n_per  <- nrow(ret_mat)
  series <- colnames(ret_mat)

  ra <- matrix(NA_real_, n_per, ncol(ret_mat), dimnames = list(NULL, series))
  nb <- matrix(NA_integer_, n_per, ncol(ret_mat), dimnames = list(NULL, series))

  for (t in seq.int(window + 1L, n_per)) {
    rows <- seq.int(t - window, t - 1L)
    X    <- cbind(1, fac$F[rows, , drop = FALSE])
    qr_x <- qr(X)
    if (qr_x$rank < ncol(X)) {
      rlang::abort(sprintf("Factor design is rank deficient in the trailing window ending at period %s.",
                           as.character(ret_ym[t - 1L])))
    }
    Yw   <- y_e[rows, , drop = FALSE]
    ft   <- fac$F[t, ]
    yt   <- y_e[t, ]

    n_ok    <- colSums(!is.na(Yw))
    usable  <- n_ok >= min_obs & !is.na(yt)
    if (!any(usable)) next

    # Fast path: columns with a complete window share one design matrix, so all
    # their betas come from a single solve.
    full <- usable & n_ok == window
    if (any(full)) {
      B <- qr.coef(qr_x, Yw[, full, drop = FALSE])             # (1+k) x n_full
      ra[t, full] <- yt[full] - as.numeric(ft %*% B[-1L, , drop = FALSE])
      nb[t, full] <- window
    }

    # Slow path: gappy columns need their own complete-case design.
    for (j in which(usable & !full)) {
      keep <- !is.na(Yw[, j])
      qr_j <- qr(X[keep, , drop = FALSE])
      if (qr_j$rank < ncol(X)) next
      coef_j <- qr.coef(qr_j, Yw[keep, j])
      if (anyNA(coef_j)) next
      ra[t, j] <- yt[j] - sum(ft * coef_j[-1L])
      nb[t, j] <- sum(keep)
    }
  }

  list(ra = ra, n_obs = nb)
}

# -----------------------------------------------------------------------------
# build_dv()
#
#   gap(i,t) = ra_ml(t) - ra_i(t)
#
# Positive gap = the machine won that period. `ml_col` selects which ML
# portfolio is the benchmark; run the whole thing across the complexity ladder
# (OLS ... NN3) to test H7 -- if the gap against OLS matches the gap against
# NN3, the story is systematisation, not machine learning.
# -----------------------------------------------------------------------------
build_dv <- function(fund_ret_mat, ml_ret_mat, period, factor_mat,
                     ml_col,
                     factor_ym = if (inherits(period, "Date")) {
                       format(period, "%Y-%m")
                     } else {
                       as.character(period)
                     },
                     window = ROLL_WINDOW, min_obs = ROLL_WINDOW) {
  if (length(period) != nrow(fund_ret_mat) || anyNA(period) ||
      anyDuplicated(period) || is.unsorted(period, strictly = TRUE)) {
    rlang::abort("period must be non-missing, unique, sorted, and match fund_ret_mat rows.")
  }
  if (!is.matrix(ml_ret_mat) || !is.numeric(ml_ret_mat) ||
      nrow(ml_ret_mat) != length(period)) {
    rlang::abort("ml_ret_mat must be a numeric matrix with one row per period.")
  }
  if (!ml_col %in% colnames(ml_ret_mat)) {
    rlang::abort(sprintf("'%s' is not a column of ml_ret_mat.", ml_col))
  }
  
  browser()

  fund_ra <- rolling_risk_adjusted(fund_ret_mat, factor_ym, factor_mat, window, min_obs)
  ml_ra   <- rolling_risk_adjusted(ml_ret_mat[, ml_col, drop = FALSE],
                                   factor_ym, factor_mat, window, min_obs)

  ml_series <- tibble::tibble(period = period,
                              ra_ml      = as.numeric(ml_ra$ra[, 1L]))

  tibble::as_tibble(fund_ra$ra) %>%
    dplyr::mutate(period = period) %>%
    tidyr::pivot_longer(-period, names_to = "code", values_to = "ra_fund") %>%
    dplyr::left_join(
      tibble::as_tibble(fund_ra$n_obs) %>%
        dplyr::mutate(period = period) %>%
        tidyr::pivot_longer(-period, names_to = "code", values_to = "n_obs_beta"),
      by = c("period", "code")
    ) %>%
    dplyr::left_join(ml_series, by = "period") %>%
    dplyr::mutate(gap = ra_ml - ra_fund, ml_benchmark = ml_col) %>%
    dplyr::filter(!is.na(gap))
}

# -----------------------------------------------------------------------------
# Inputs the DV needs, loaded standalone (plots.R is never sourced here).
# Every definition below mirrors plots.R so the gap is measured against exactly
# the factor basis and return convention used in the published tables.
# -----------------------------------------------------------------------------

SIGNAL_PORTS_FILE <- file.path(
  DATA, "signal_ports", "gold_2024_05_comdinheiro_signal_port_backtest_cohorts.qs"
)

# Restore a serialised meta_xts whose index was stored numerically (plots.R:14).
restore_xts_dates <- function(obj) {
  idx <- zoo::index(obj@data)
  if (inherits(idx, "Date")) return(obj)
  if (!is.numeric(idx)) rlang::abort("meta_xts index is neither Date nor numeric.")
  d <- as.Date(as.POSIXct(as.numeric(idx), origin = "1970-01-01", tz = "UTC"))
  if (anyNA(d) || anyDuplicated(d) || is.unsorted(d, strictly = TRUE)) {
    rlang::abort("meta_xts index could not be restored to unique sorted dates.")
  }
  obj@data <- xts::xts(zoo::coredata(obj@data), order.by = d)
  obj
}

# Geometric compounding of daily percent returns over (previous 15th, 15th].
# Identical convention to compound_daily_returns_15th() at plots.R:99.
compound_daily_to_windows <- function(obj, period_ends, codes = colnames(obj$values)) {
  period_ends <- validate_ml_index(period_ends)
  openings    <- window_openings(period_ends)

  out <- matrix(NA_real_, length(period_ends), length(codes),
                dimnames = list(NULL, codes))
  for (i in seq_along(period_ends)) {
    sel <- obj$dates > openings[[i]] & obj$dates <= period_ends[[i]]
    if (!any(sel)) {
      rlang::abort(paste0("No daily rows in the window ending ", period_ends[[i]], "."))
    }
    block <- obj$values[sel, codes, drop = FALSE]
    out[i, ] <- apply(block, 2L, function(x) {
      x <- x[is.finite(x)]
      if (!length(x)) return(NA_real_)
      (prod(1 + x / 100) - 1) * 100
    })
  }
  out
}

# The eight meta-portfolio construction schemes, in the order they appear in the
# pinned backtest. `selected_bench_return` is the IBOV benchmark, not a scheme.
ML_SCHEMES <- c("rp", "rp_box", "hrp", "cs", "sw", "ew", "mvo", "mvo_box")

# Resolve a short scheme name ("mvo_box") or the literal "average" to a return
# series. "average" equal-weights all eight construction schemes, which is the
# right benchmark when the question is about systematic investing in general
# rather than about one optimiser's quirks -- it also removes the reader's
# suspicion that a single scheme was chosen after seeing the results.
resolve_ml_benchmark <- function(ml_ret_mat, scheme) {
  scheme_col <- function(s) {
    hit <- grep(sprintf("^c__meta_%s_ibov_s__", s), colnames(ml_ret_mat), value = TRUE)
    if (length(hit) != 1L) {
      rlang::abort(sprintf("Scheme '%s' matched %d columns.", s, length(hit)))
    }
    hit
  }

  if (identical(scheme, "average")) {
    cols <- vapply(ML_SCHEMES, scheme_col, character(1))
    avg  <- rowMeans(ml_ret_mat[, cols, drop = FALSE], na.rm = TRUE)
    out  <- cbind(ml_ret_mat, average = avg)
    return(list(mat = out, col = "average"))
  }

  list(mat = ml_ret_mat, col = scheme_col(scheme))
}

# ML meta-portfolio net returns, on the same index as read_ml_index().
read_ml_returns <- function(path = ML_BACKTEST_FILE) {
  cohort <- qs::qread(path)$backtest_meta_portfolios_results$ibov_port_backtest_cohort
  obj    <- restore_xts_dates(cohort@port_returns_m_xts_list$net_returns_m_xts)
  m      <- as.matrix(zoo::coredata(obj@data))
  rownames(m) <- format(as.Date(zoo::index(obj@data)))
  m
}

# Six-factor basis exactly as plots.R:1009 builds it: MKT as benchmark excess
# over CDI, every style factor market-neutralised so long-only portfolios do not
# collapse into collinearity.
build_factor_mat <- function(period_ends, cdi_daily) {
  sp <- restore_xts_dates(
    qs::qread(SIGNAL_PORTS_FILE)$signal_port_backtest_cohort_ind_signals@
      port_returns_m_xts_list$net_returns_m_xts
  )
  sp_xts <- sp@data
  sp_ym  <- format(as.Date(zoo::index(sp_xts)), "%Y-%m")

  fcol <- function(sig) {
    hit <- grep(paste0("^c__cs_", sig, "_(long|short)_s__"),
                colnames(sp_xts), value = TRUE)
    if (length(hit) != 1L) {
      rlang::abort(sprintf("Signal '%s' matched %d columns, expected 1.", sig, length(hit)))
    }
    hit
  }

  rf <- as.numeric(compound_daily_to_windows(cdi_daily, period_ends,
                                             codes = colnames(cdi_daily$values)[1])[, 1])

  tibble::tibble(
    ym  = sp_ym,
    mkt = as.numeric(sp_xts[, "selected_bench_return"]),
    hml = as.numeric(sp_xts[, fcol("be_me")]),
    mom = as.numeric(sp_xts[, fcol("prc_ret_12m_1m")]),
    rmw = as.numeric(sp_xts[, fcol("gp_at_12m")]),
    smb = as.numeric(sp_xts[, fcol("log_mkt_cap")]),
    cma = as.numeric(sp_xts[, fcol("assets_gr1y_tr")])
  ) %>%
    dplyr::left_join(tibble::tibble(ym = format(period_ends, "%Y-%m"), rf = rf),
                     by = "ym") %>%
    dplyr::transmute(
      ym, rf,
      mkt_e = mkt - rf,
      hml_n = hml - mkt,
      mom_n = mom - mkt,
      rmw_n = rmw - mkt,
      smb_n = smb - mkt,
      cma_n = cma - mkt
    ) %>%
    dplyr::arrange(ym)
}


# -----------------------------------------------------------------------------
# rolling_rho_machine()
#
# Returns-based transfer-coefficient proxy. Both details below are load-bearing:
#
#   * computed on RESIDUAL active returns (the `ra` series above), never raw
#     active returns -- raw correlation conflates weight alignment with shared
#     factor exposure, so two funds holding nothing in common but tilted the
#     same way would look aligned;
#   * TRAILING window, so the value at t uses only information through t-1. The
#     ML portfolio performed well in sample, so a contemporaneous correlation
#     with it is mechanically tied to performance -- the same maximisation bias
#     already handled in the oracle/FUNDS_Q75 analysis. Used contemporaneously
#     this variable would manufacture its own result.
# -----------------------------------------------------------------------------
rolling_rho_machine <- function(fund_ra, ml_ra, window = ROLL_WINDOW,
                                min_obs = 24L) {
  if (!is.matrix(fund_ra) || !is.numeric(fund_ra) || ncol(fund_ra) == 0L ||
      length(ml_ra) != nrow(fund_ra)) {
    rlang::abort("fund_ra must be a non-empty numeric matrix and ml_ra must match its rows.")
  }
  if (length(window) != 1L || !is.finite(window) || window < 1L ||
      window != as.integer(window) || nrow(fund_ra) <= window) {
    rlang::abort("window must be a positive integer shorter than the residual series.")
  }
  if (length(min_obs) != 1L || !is.finite(min_obs) || min_obs < 2L ||
      min_obs != as.integer(min_obs) || min_obs > window) {
    rlang::abort("min_obs must be an integer between 2 and window.")
  }
  if (is.null(colnames(fund_ra)) || anyNA(colnames(fund_ra)) ||
      any(!nzchar(colnames(fund_ra))) || anyDuplicated(colnames(fund_ra))) {
    rlang::abort("fund_ra must have non-missing, unique column names.")
  }
  if (any(!is.finite(fund_ra) & !is.na(fund_ra)) ||
      any(!is.finite(ml_ra) & !is.na(ml_ra))) {
    rlang::abort("Residual inputs may contain missing values but not infinite values.")
  }
  n_per <- nrow(fund_ra)
  out   <- matrix(NA_real_, n_per, ncol(fund_ra),
                  dimnames = list(NULL, colnames(fund_ra)))

  for (t in seq.int(window + 1L, n_per)) {
    rows <- seq.int(t - window, t - 1L)       # strictly trailing
    m    <- ml_ra[rows]
    out[t, ] <- apply(fund_ra[rows, , drop = FALSE], 2L, function(f) {
      ok <- is.finite(f) & is.finite(m)
      if (sum(ok) < min_obs) return(NA_real_)
      if (stats::sd(f[ok]) == 0 || stats::sd(m[ok]) == 0) return(NA_real_)
      stats::cor(f[ok], m[ok])
    })
  }
  out
}

# idio_vol: residual sd from the six-factor fit -> breadth proxy (Abis: quants
# hold 225 names vs 117, up to 15% lower idiosyncratic vol).
# Live tension worth exploiting: Amihud-Goyenko read LOW R-squared as
# selectivity predicting HIGHER alpha; Abis reads HIGH R-squared as diversified
# systematic breadth. Same statistic, opposite stories -- and our machine, with
# known high breadth, is the reference point that separates them.


# =============================================================================
# S9. Assembly
# =============================================================================
build_gap_panel <- function(ml_index, ml_benchmark = ML_BENCHMARK_DEFAULT,
                            screen_duplicates = TRUE) {
  
  browser()
  rlang::inform("Reading Economatica daily panels ...")
  daily <- read_all_daily()

  rlang::inform("Reconciling fund codes across panels ...")
  codes <- reconcile_codes(daily)

  static   <- build_static()
  gest_s   <- build_gestora_static(static)
  fund_tv  <- build_fund_timevarying(daily, ml_index, codes)
  gest_tv  <- build_gestora_timevarying(fund_tv, static)
  industry <- build_industry_panel(fund_tv)
  conc     <- build_concentration_panel(ml_index)

  rlang::inform("Building the dependent variable ...")
  dv <- build_gap_dv(daily, ml_index, codes, ml_benchmark)

  # Same-master feeders produce near-identical return series that look like
  # independent funds, inflating the cross-section and understating standard
  # errors. The cluster id and representative flag are ATTACHED, never filtered:
  # every legal vehicle must stay in the panel for AUM and flow aggregation, but
  # any strategy-level regression should restrict to `is_representative`.
  dup <- if (isTRUE(screen_duplicates)) {
    rlang::inform("Screening duplicate strategies ...")
    fund_m <- compound_daily_to_windows(daily$ret, ml_index, codes)
    obs_n  <- colSums(!is.na(fund_m))
    find_duplicate_clusters(fund_m) %>%
      pick_cluster_representative(static, obs_n)
  } else {
    tibble::tibble(code = codes, dup_cluster = codes, is_representative = TRUE)
  }

  panel <- fund_tv %>%
    dplyr::left_join(dv$gap, by = c("code", "period")) %>%
    dplyr::left_join(dup, by = "code") %>%
    dplyr::left_join(static, by = "code") %>%
    dplyr::left_join(gest_s, by = "gestora") %>%
    dplyr::left_join(dplyr::select(gest_tv, gestora, period,
                                   pl_gestora, log_pl_gestora, gestora_net_flow,
                                   n_funds_reporting),
                     by = c("gestora", "period")) %>%
    dplyr::left_join(conc, by = c("code", "period")) %>%
    dplyr::mutate(
      fund_age_yrs    = as.numeric(period - inception) / 365.25,
      gestora_age_yrs = as.numeric(period - gestora_inception) / 365.25,
      # Main sample: stock pickers only. Feeders and funds-of-funds are
      # different estimands and must not be silently pooled.
      in_main_sample  = !is_feeder & !is_fof & !is.na(aum)
    )

  assert_panel(panel)
  list(panel = panel, industry = industry, static = static,
       ml_benchmark = dv$ml_benchmark, factor_mat = dv$factor_mat)
}

# Assemble the DV: compound fund and ML returns onto the 15th-to-15th index,
# build the factor basis, then difference the realised risk-adjusted returns.
# `rho_machine` comes free once both residual series exist.
build_gap_dv <- function(daily, ml_index, codes,
                         ml_benchmark = ML_BENCHMARK_DEFAULT) {
  
  browser()
  cdi_daily <- read_economatica_daily("CDI_diarios.xlsx")

  fund_m <- compound_daily_to_windows(daily$ret, ml_index, codes)
  ml_raw <- read_ml_returns()

  if (!identical(rownames(ml_raw), format(ml_index))) {
    rlang::abort("ML return index does not match the validated ml_index.")
  }

  resolved     <- resolve_ml_benchmark(ml_raw, ml_benchmark)
  ml_m         <- resolved$mat
  ml_benchmark <- resolved$col
  rlang::inform(sprintf("  ML benchmark column: '%s'.", ml_benchmark))

  factor_mat <- build_factor_mat(ml_index, cdi_daily)

  gap <- build_dv(fund_m, ml_m, ml_index, factor_mat, ml_col = ml_benchmark)

  # Mediator, not a control (see the taxonomy at the top of this file).
  fund_ra <- rolling_risk_adjusted(fund_m, format(ml_index, "%Y-%m"), factor_mat)
  ml_ra   <- rolling_risk_adjusted(ml_m[, ml_benchmark, drop = FALSE],
                                   format(ml_index, "%Y-%m"), factor_mat)
  rho <- rolling_rho_machine(fund_ra$ra, as.numeric(ml_ra$ra[, 1L]))

  gap <- gap %>%
    dplyr::left_join(
      matrix_to_long(rho, ml_index, "rho_machine"),
      by = c("code", "period")
    )

  list(gap = gap, ml_benchmark = ml_benchmark, factor_mat = factor_mat)
}

assert_panel <- function(panel) {
  if (anyDuplicated(panel[, c("code", "period")])) {
    rlang::abort("gap_panel: duplicate (code, period) rows.")
  }
  rlang::inform(paste0(
    sprintf("gap_panel: %d fund-periods, %d funds, %d in main sample.\n",
            nrow(panel), dplyr::n_distinct(panel$code),
            sum(panel$in_main_sample, na.rm = TRUE)),
    "REMINDERS\n",
    "  * log_pl_fundo is ENDOGENOUS to past performance (Berk-Green). Naive OLS\n",
    "    of alpha on size is biased -- use Pastor-Stambaugh-Taylor recursive\n",
    "    demeaning, or instrument size.\n",
    "  * Group D (idio_vol, rho_machine, tracking error) are MEDIATORS. Keep\n",
    "    them off the RHS of the main gap regression.\n",
    "  * Prefer industry_flow_cum / industry_flow_rate over industry_pl:\n",
    "    summed AUM embeds the cumulative returns of the panel's own funds.\n",
    "  * Run find_duplicate_clusters() before estimating -- 38% of the universe\n",
    "    is feeder/mirror vehicles."))
  invisible(TRUE)
}

# The writes must stay INSIDE the guard. Tests source this file with
# gap_panel.run = FALSE to load helpers without triggering the full build; with
# the qsave() calls outside, `res` does not exist and the whole suite aborts
# before the first test runs.
if (isTRUE(getOption("gap_panel.run", TRUE))) {
  ml_index <- read_ml_index()
  res <- build_gap_panel(ml_index)
  qs::qsave(res$panel,    file.path(OUT, "gap_panel.qs"))
  qs::qsave(res$industry, file.path(OUT, "industry_panel.qs"))
}

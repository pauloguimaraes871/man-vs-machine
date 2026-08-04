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

  values <- as.matrix(raw[, -1, drop = FALSE])
  colnames(values) <- codes
  list(dates = as.Date(raw[[1]]), values = values, codes = codes)
}

# Read all four once, keyed by name.
read_all_daily <- function() {
  purrr::set_names(DAILY_FILES$file, DAILY_FILES$key) %>%
    purrr::map(read_economatica_daily)
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

CAD_COLS <- c(status = 6L, code = 8L, anbima_class = 9L, efpc = 10L,
              multigestor = 11L, fundo_cotas = 12L, perf_fee = 13L,
              investor_type = 14L, leveraged = 15L, redemption = 17L,
              gestora = 18L, administradora = 19L, taxa_adm = 22L,
              inception = 25L)

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

build_static <- function() {
  raw <- readxl::read_excel(file.path(DATA, "dados_cadastrais.xlsx"),
                            .name_repair = "minimal")

  CAD_COLS %>%
    purrr::map(~ trimws(as.character(raw[[.x]]))) %>%
    tibble::as_tibble() %>%
    dplyr::slice(-1) %>%                              # drop header-as-data row
    dplyr::filter(!is.na(code), code != "") %>%
    dplyr::transmute(
      code,
      anbima_class,
      gestora,
      administradora,
      status          = tolower(status),              # ativo | cancelado
      efpc            = efpc          %in% c("Sim", "S"),
      perf_fee        = perf_fee      %in% c("Sim", "S"),
      leveraged       = leveraged     %in% c("S", "Sim"),
      investor_type   = investor_type,                # Geral|Qualificado|Profissional
      redemption_days = parse_redemption(redemption),
      expense_ratio   = suppressWarnings(as.numeric(taxa_adm)),
      inception       = parse_excel_date(inception),

      # Feeder / mirror is DIRECTLY observed -- no name heuristic.
      # Real counts: Feeder 573, Espelho 161, Multigestor 29, N/A 1001, "-" 180.
      is_feeder = multigestor %in% c("Feeder", "Espelho"),

      # Owner's rule. Caps out at 29 funds, so any "average human fund-of-funds"
      # benchmark rests on a very thin cross-section -- report N or drop it.
      is_fof = fundo_cotas == "Sim" & multigestor == "Multigestor",

      # Administradora matters as much as gestora: the administrator is the
      # distribution channel, which is what the captive-flow argument is about.
      bank_affiliated = grepl(BANK_PATTERN, paste(gestora, administradora),
                              ignore.case = TRUE),

      # Diagnostic only (DESIGN_NOTE.md S9). Miguel & Chen (2025) show the label
      # does not identify the behaviour. Never a headline split.
      name_quant = grepl("quantitativ|sistem.tic|multifator",
                         paste(gestora, anbima_class), ignore.case = TRUE)
    ) %>%
    dplyr::mutate(
      anbima_class_ext = dplyr::case_when(
        name_quant ~ "Quantitativo",
        is_fof     ~ "Multifundos",
        TRUE       ~ anbima_class
      )
    )
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
# near-unit correlation over a decent overlap. O(n^2) -- run once and cache.

find_duplicate_clusters <- function(ret_mat, min_overlap = 24L, rho_cut = 0.999) {
  codes <- colnames(ret_mat)
  n     <- length(codes)
  owner <- seq_len(n)

  for (i in seq_len(n - 1L)) {
    if (owner[i] != i) next
    xi <- ret_mat[, i]
    for (j in (i + 1L):n) {
      if (owner[j] != j) next
      ok <- is.finite(xi) & is.finite(ret_mat[, j])
      if (sum(ok) < min_overlap) next
      r <- suppressWarnings(stats::cor(xi[ok], ret_mat[ok, j]))
      if (is.finite(r) && r >= rho_cut) owner[j] <- i
    }
  }

  tibble::tibble(code = codes, dup_cluster = codes[owner])
}

# Keep one representative per cluster: longest history, then cheapest.
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
# Daily data removes the month-end/15th mismatch: sample levels AT the 15th that
# OPENS each window (predetermined, therefore a clean regressor) and SUM flows
# over the preceding window.
#
# Flows need care. Net flow DURING a window is caused by return during that
# window (performance chasing), so only LAGGED window flow is a predictor.
# Contemporaneous flow is an outcome and needs its own identification argument.

# Level as of each window opening: last observation at or before the date.
sample_at <- function(obj, when, codes) {
  idx <- findInterval(when, obj$dates)
  out <- matrix(NA_real_, length(when), length(codes),
                dimnames = list(NULL, codes))
  hit <- idx > 0
  out[hit, ] <- obj$values[idx[hit], codes, drop = FALSE]
  out
}

# Flow summed over (previous window open, this window open].
sum_over_window <- function(obj, when, codes) {
  prev <- c(as.Date(NA), utils::head(when, -1))
  purrr::map2(prev, when, function(from, to) {
    sel <- !is.na(from) & obj$dates > from & obj$dates <= to
    if (!any(sel)) return(rep(NA_real_, length(codes)))
    colSums(obj$values[sel, codes, drop = FALSE], na.rm = TRUE)
  }) %>%
    purrr::reduce(rbind) %>%
    matrix(nrow = length(when), dimnames = list(NULL, codes))
}

matrix_to_long <- function(mat, when, value_name) {
  tibble::as_tibble(mat) %>%
    dplyr::mutate(period = when) %>%
    tidyr::pivot_longer(-period, names_to = "code", values_to = value_name)
}

build_fund_timevarying <- function(daily, ml_index, codes) {
  levels_long <- list(aum = "aum", n_investors = "n_investors") %>%
    purrr::imap(~ sample_at(daily[[.x]], ml_index, codes) %>%
                  matrix_to_long(ml_index, .y))

  flow_long <- sum_over_window(daily$net_flow, ml_index, codes) %>%
    matrix_to_long(ml_index, "net_flow")

  purrr::reduce(c(levels_long, list(flow_long)),
                dplyr::full_join, by = c("period", "code")) %>%
    dplyr::arrange(code, period) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(
      flow_rate     = net_flow / dplyr::lag(aum),
      flow_rate_lag = dplyr::lag(flow_rate),          # the usable predictor
      flow_vol_12   = zoo::rollapplyr(flow_rate, 12, stats::sd,
                                      fill = NA, na.rm = TRUE),
      log_pl_fundo  = log(pmax(aum, 1e-8)),
      log_investors = log(pmax(n_investors, 1))
    ) %>%
    dplyr::ungroup()
}

build_gestora_timevarying <- function(fund_tv, static_df) {
  fund_tv %>%
    dplyr::left_join(dplyr::select(static_df, code, gestora), by = "code") %>%
    dplyr::filter(!is.na(gestora)) %>%
    dplyr::group_by(gestora, period) %>%
    dplyr::summarise(
      pl_gestora        = sum(aum, na.rm = TRUE),
      n_funds_reporting = sum(!is.na(aum)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(log_pl_gestora = log(pmax(pl_gestora, 1e-8)))
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
#   w_cum_k      cumulative weight of the k largest positions
#   hhi_top15    sum of squared top-15 weights = a LOWER BOUND on true HHI
#   resid_weight 100 - w_cum_15 = weight held outside the top 15
#   n_pos_min    HARD lower bound on the number of positions, no distributional
#                assumption: every unseen position is at most w_(15), so
#                  n_pos_min = 15 + ceiling(resid_weight / w_15)
#                This is the referee-proof breadth measure. Prefer it.
#   decay_slope  OLS slope of log w_(k) on log k -- the shape of the weight
#                curve. Steep = concentrated stock picker, flat = diversified
#                systematic. This is the DISCRIMINATOR (see below).
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
# WHY decay_slope IS THE CLASSIFIER WE COULD NOT BUILD BEFORE
#   Abis: quantitative funds hold ~225 names, discretionary ~117. A systematic
#   book has a flat, long weight curve; a discretionary book is steep and short.
#   Unlike the rejected designs in DESIGN_NOTE.md S9, this is observable,
#   holdings-based, and INDEPENDENT of the ML portfolios -- so it carries none
#   of the circularity that killed "closeness to ML" as a classifier.
#   Control for ANBIMA class and fund size: sectorial mandates are concentrated
#   by design, and large funds cannot concentrate in small caps.

TOPX_MAX <- 15L

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

read_topx_rank <- function(k) {
  path   <- file.path(DATA, sprintf("top%d_w.xlsx", k))
  header <- readxl::read_excel(path, skip = 3, n_max = 0, .name_repair = "minimal")
  raw    <- readxl::read_excel(path, skip = 3, na = "-",
                               col_types = c("text", rep("numeric", ncol(header) - 1L)),
                               .name_repair = "minimal")

  values <- as.matrix(raw[, -1, drop = FALSE])
  colnames(values) <- extract_code(names(header)[-1])

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

# Fit log w_(k) ~ log k. Needs >= 4 ranks to be meaningful.
fit_decay_slope <- function(rank, w) {
  ok <- is.finite(w) & w > 0 & is.finite(rank) & rank > 0
  if (sum(ok) < 4L) return(NA_real_)
  unname(stats::coef(stats::lm(log(w[ok]) ~ log(rank[ok])))[2])
}

summarise_concentration <- function(topx_long) {
  topx_long %>%
    dplyr::arrange(code, month, rank) %>%
    dplyr::group_by(code, month) %>%
    dplyr::summarise(
      n_ranks     = dplyr::n(),
      w_top1      = w[rank == 1][1],
      w_top5      = sum(w[rank <= 5]),
      w_top10     = sum(w[rank <= 10]),
      w_cum_15    = sum(w),
      w_last      = w[which.max(rank)],
      hhi_top15   = sum((w / 100)^2),                 # LOWER bound on true HHI
      decay_slope = fit_decay_slope(rank, w),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      resid_weight = pmax(100 - w_cum_15, 0),
      # Hard lower bound, no distributional assumption: every unseen position
      # is at most as large as the smallest observed one.
      n_pos_min = dplyr::if_else(
        is.finite(w_last) & w_last > 0,
        n_ranks + ceiling(resid_weight / w_last),
        NA_real_
      ),
      eff_bets_max = dplyr::if_else(hhi_top15 > 0, 1 / hhi_top15, NA_real_)
    )
}

# Attach a month-end holdings snapshot to the window it can legitimately
# explain: the one OPENING after that month ends. Same predetermination logic
# as the AUM lag in S5.
build_concentration_panel <- function(ml_index, max_rank = TOPX_MAX) {
  conc <- read_topx_all(max_rank) %>% summarise_concentration()

  window_for_month <- tibble::tibble(period = ml_index) %>%
    dplyr::mutate(month_key = as.Date(format(period, "%Y-%m-01")))

  conc %>%
    dplyr::mutate(month_key = shift_month(month, 1L)) %>%
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
  fund_tv %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      industry_pl       = sum(aum, na.rm = TRUE),
      industry_net_flow = sum(net_flow, na.rm = TRUE),
      n_reporting       = sum(!is.na(aum)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(period) %>%
    dplyr::mutate(
      # Counterfactual asset base with market appreciation stripped out.
      industry_flow_cum = cumsum(dplyr::coalesce(industry_net_flow, 0)),

      # Flow scaled by lagged size: the standard flow-rate object, and the
      # cleanest of the three for use as a regressor.
      industry_flow_rate = industry_net_flow / dplyr::lag(industry_pl),

      # Description only -- return-contaminated, see (1) above.
      industry_pl_growth = industry_pl / dplyr::lag(industry_pl) - 1,

      log_industry_pl = log(pmax(industry_pl, 1e-8))
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
  idx <- match(ret_ym, factor_mat$ym)
  if (anyNA(idx)) {
    rlang::abort(sprintf("factor_mat is missing %d of %d periods (first: %s).",
                         sum(is.na(idx)), length(idx), ret_ym[which(is.na(idx))[1]]))
  }
  list(
    F  = as.matrix(factor_mat[idx, FF_PREDICTORS, drop = FALSE]),
    rf = as.numeric(factor_mat$rf[idx])
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
# carries no look-ahead and consecutive observations do not overlap. That is the
# whole reason for using realised residuals instead of rolling alpha estimates:
# a rolling alpha is a window average, so its successive values share ~35 of 36
# observations, the effective sample size collapses, and every standard error
# downstream becomes wrong.
# -----------------------------------------------------------------------------
rolling_risk_adjusted <- function(ret_mat, ret_ym, factor_mat,
                                  window = ROLL_WINDOW, min_obs = 24L) {
  stopifnot(is.matrix(ret_mat), nrow(ret_mat) == length(ret_ym))
  if (min_obs <= length(FF_PREDICTORS) + 1L) {
    rlang::abort("min_obs must exceed the number of coefficients (7).")
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
      B <- qr.solve(X, Yw[, full, drop = FALSE])              # (1+k) x n_full
      ra[t, full] <- yt[full] - as.numeric(ft %*% B[-1L, , drop = FALSE])
      nb[t, full] <- window
    }

    # Slow path: gappy columns need their own complete-case design.
    for (j in which(usable & !full)) {
      keep <- !is.na(Yw[, j])
      fit  <- tryCatch(.lm.fit(X[keep, , drop = FALSE], Yw[keep, j]),
                       error = function(e) NULL)
      if (is.null(fit) || anyNA(fit$coefficients)) next
      ra[t, j] <- yt[j] - sum(ft * fit$coefficients[-1L])
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
build_dv <- function(fund_ret_mat, ml_ret_mat, ret_ym, factor_mat,
                     ml_col, window = ROLL_WINDOW, min_obs = 24L) {
  if (!ml_col %in% colnames(ml_ret_mat)) {
    rlang::abort(sprintf("'%s' is not a column of ml_ret_mat.", ml_col))
  }

  fund_ra <- rolling_risk_adjusted(fund_ret_mat, ret_ym, factor_mat, window, min_obs)
  ml_ra   <- rolling_risk_adjusted(ml_ret_mat[, ml_col, drop = FALSE],
                                   ret_ym, factor_mat, window, min_obs)

  ml_series <- tibble::tibble(period_idx = seq_along(ret_ym),
                              ra_ml      = as.numeric(ml_ra$ra[, 1L]))

  tibble::as_tibble(fund_ra$ra) %>%
    dplyr::mutate(period_idx = dplyr::row_number()) %>%
    tidyr::pivot_longer(-period_idx, names_to = "code", values_to = "ra_fund") %>%
    dplyr::left_join(
      tibble::as_tibble(fund_ra$n_obs) %>%
        dplyr::mutate(period_idx = dplyr::row_number()) %>%
        tidyr::pivot_longer(-period_idx, names_to = "code", values_to = "n_obs_beta"),
      by = c("period_idx", "code")
    ) %>%
    dplyr::left_join(ml_series, by = "period_idx") %>%
    dplyr::mutate(gap = ra_ml - ra_fund, ml_benchmark = ml_col) %>%
    dplyr::filter(!is.na(gap))
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
build_gap_panel <- function(ml_index) {
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

  panel <- fund_tv %>%
    dplyr::left_join(static, by = "code") %>%
    dplyr::left_join(gest_s, by = "gestora") %>%
    dplyr::left_join(dplyr::select(gest_tv, gestora, period,
                                   pl_gestora, log_pl_gestora, n_funds_reporting),
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
  list(panel = panel, industry = industry, static = static)
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

# res <- build_gap_panel(ml_index)
# qs::qsave(res$panel,    file.path(OUT, "gap_panel.qs"))
# qs::qsave(res$industry, file.path(OUT, "industry_panel.qs"))

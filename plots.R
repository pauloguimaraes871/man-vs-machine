#Load datasets
library("dplyr")
library("factoRverse")
if (!requireNamespace("qs", quietly = TRUE)) {
  # qs was archived from CRAN on 2026-01-17; install the last binary from a
  # pre-archival Posit Package Manager snapshot instead of the "latest" repo.
  install.packages(
    "qs",
    repos = "https://packagemanager.posit.co/cran/2026-01-10"
  )
  renv::record("qs@0.27.3")
}

restore_meta_xts_dates <- function(x) {
  idx <- zoo::index(x@data)
  if (inherits(idx, "Date")) return(x)
  if (!is.numeric(idx)) {
    rlang::abort("The stored meta_xts index is neither Date nor numeric.")
  }

  restored_dates <- as.Date(
    as.POSIXct(as.numeric(idx), origin = "1970-01-01", tz = "UTC")
  )
  if (anyNA(restored_dates) || anyDuplicated(restored_dates) ||
      is.unsorted(restored_dates, strictly = TRUE)) {
    rlang::abort("The stored meta_xts index could not be restored to unique, sorted dates.")
  }

  x@data <- xts::xts(zoo::coredata(x@data), order.by = restored_dates)
  x
}

# ---- Machine-run bundles ---------------------------------------------------
# This reads the LOCAL paper run (run_paper_machine.R, grid arm), not the pinned
# pre-compiled bundles.
#
# The previous loader walked every folder under data/ and assigned each one's
# single .qs to a variable named after the folder. That layout no longer exists:
# the pre-compiled bundles moved under data/pre_compiled_results/, data/derived/
# now holds two .qs files rather than one, and data/checkpoints/ holds none, so
# the walk fed qs::qread() a length-0 and a length-2 path and died at the first
# folder. Name the three objects this script actually needs instead.
#
#   meta_grid  meta-portfolios: $cohorts$<mandate> is a port_backtest_cohort,
#              $results the 32 individual (8 scheme x 4 mandate) backtests
#   mb_grid    the ML models: an sb_metabacktest_results S4 carrying the meta
#              learner, the 7 base learners and their OOS predictions
#   learners   per-base-learner portfolios (cs and rp), built by the `learners`
#              stage; $cohort is the port_backtest_cohort
#   screen     signal portfolios, under $signal_port_cohorts
MACHINE_RUN <- here::here("data", "derived", "machine_run")

read_run_bundle <- function(file) {
  path <- file.path(MACHINE_RUN, file)
  if (!file.exists(path)) {
    rlang::abort(paste0(
      "Machine-run bundle not found: ", path,
      ". Run run_paper_machine.R (STOP_AFTER = 'learners') first."
    ))
  }
  qs::qread(path, nthreads = 4)
}

meta_grid <- read_run_bundle("meta_grid.qs")
mb_grid   <- read_run_bundle("mb_grid.qs")
learners  <- read_run_bundle("learners_grid.qs")
screen    <- read_run_bundle("screen.qs")
ss        <- read_run_bundle("ss.qs")

# ---- Consolidated OOS prediction metrics (Table 1) -------------------------
# Build the table directly from each current grid-search base learner. The
# consolidated `cons_oos` column evaluates the complete learner-specific OOS
# prediction vector, rather than averaging annual evaluation statistics.
predictive_model_label <- function(name) {
  toupper(sub("_(grid|bayes)$", "", sub("_f__.*$", "", sub("^c__", "", name))))
}

predictive_model_order <- c("OLS", "GLMNET", "RF", "XGB", "NN1", "NN2", "NN3")
predictive_metric_order <- c(
  "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb"
)
predictive_sb_list <- mb_grid@base_sb_backtest_results_list
if (!setequal(vapply(names(predictive_sb_list), predictive_model_label, character(1)),
              predictive_model_order)) {
  rlang::abort("The grid bundle does not contain exactly the seven expected base learners.")
}

predictive_metrics_long <- purrr::imap_dfr(
  predictive_sb_list,
  function(result, name) {
    metrics <- result@consolidated_eval_metrics
    required_columns <- c("metric", "cons_oos")
    if (!all(required_columns %in% names(metrics))) {
      rlang::abort(paste0("Missing consolidated OOS metrics for ", name, "."))
    }
    metrics |>
      dplyr::filter(.data$metric %in% predictive_metric_order) |>
      dplyr::transmute(
        model = predictive_model_label(name),
        metric = .data$metric,
        value = as.numeric(.data$cons_oos)
      )
  }
)
if (anyDuplicated(predictive_metrics_long[c("model", "metric")]) ||
    any(!is.finite(predictive_metrics_long$value)) ||
    nrow(predictive_metrics_long) !=
      length(predictive_model_order) * length(predictive_metric_order)) {
  rlang::abort("The consolidated OOS metric panel is incomplete, duplicated, or non-finite.")
}

predictive_metrics <- predictive_metrics_long |>
  tidyr::pivot_wider(names_from = .data$metric, values_from = .data$value) |>
  dplyr::mutate(
    model = factor(.data$model, levels = predictive_model_order)
  ) |>
  dplyr::arrange(.data$model)

predictive_digits <- c(
  rss = 4L, cp = 3L, rmse = 3L, mae = 3L, mphe = 3L,
  mpe = 3L, mape = 3L, hr = 3L, mb = 3L
)
predictive_preference <- c(
  rss = "high", cp = "high", rmse = "low", mae = "low", mphe = "low",
  mpe = "low", mape = "low", hr = "high", mb = "zero"
)

format_predictive_metric <- function(metric, value) {
  values <- predictive_metrics[[metric]]
  score <- switch(
    predictive_preference[[metric]],
    high = values,
    low = -values,
    zero = -abs(values)
  )
  formatted <- sprintf(paste0("%.", predictive_digits[[metric]], "f"), value)
  row_index <- which(values == value)
  if (length(row_index) != 1L) {
    rlang::abort(paste0("A unique model value was not found for metric ", metric, "."))
  }
  if (row_index == which.max(score)) {
    paste0("\\textbf{", formatted, "}")
  } else if (row_index == which.min(score)) {
    paste0("\\underline{", formatted, "}")
  } else {
    formatted
  }
}

predictive_display_names <- c(
  OLS = "OLS", GLMNET = "ENET", RF = "RF", XGB = "XGB",
  NN1 = "NN1", NN2 = "NN2", NN3 = "NN3"
)
predictive_table_rows <- vapply(seq_len(nrow(predictive_metrics)), function(i) {
  model <- as.character(predictive_metrics$model[[i]])
  cells <- vapply(predictive_metric_order, function(metric) {
    format_predictive_metric(metric, predictive_metrics[[metric]][[i]])
  }, character(1))
  paste0(
    predictive_display_names[[model]], " & ", paste(cells, collapse = " & "),
    " ", intToUtf8(92), intToUtf8(92)
  )
}, character(1))

predictive_table_tex <- c(
  "\\begin{table}[t]",
  "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  "\\renewcommand{\\arraystretch}{1.15}",
  paste0(
    "\\caption[Out-of-sample predictive performance]",
    "{\\footnotesize \\textbf{: Out-of-sample predictive performance.} ",
    "The table reports the \\texttt{cons\\_oos} metrics stored for each base learner ",
    "over June 2015--July 2026. OOS $R^2$ is $1-\\sum e^2/\\sum y^2$; CP is the ",
    "mean forecast--target cross-product; RMSE, MAE, MPHE and MPE are root mean ",
    "squared, absolute, pseudo-Huber and median pinball errors; MAPE is mean absolute ",
    "percentage error; HR is the directional hit rate; and MB is mean bias under ",
    "$e=y-\\widehat{y}$. Bold denotes the best value and underlining the worst: ",
    "error metrics and absolute MB are minimized, whereas OOS $R^2$, CP and HR are maximized.}"
  ),
  "\\footnotesize",
  "\\setlength{\\tabcolsep}{1.6pt}",
  paste0(
    "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{1.1cm}",
    "*{9}{>{\\raggedleft\\arraybackslash}X}}"
  ),
  "\\hline",
  paste0(
    "Model & OOS $R^2$ & CP & RMSE & MAE & MPHE & MPE & MAPE & HR & MB ",
    intToUtf8(92), intToUtf8(92)
  ),
  "\\hline",
  predictive_table_rows,
  "\\hline",
  "\\end{tabularx}",
  "\\label{summarystats}",
  "\\end{table}"
)
dir.create(here::here("tables"), showWarnings = FALSE)
writeLines(predictive_table_tex, here::here("tables", "predictive_performance.tex"))

# The mandate whose portfolios every exhibit below is built from. The machine
# ran four; the paper's figures are the broad-market contest.
RUN_MANDATE <- "ibov"
meta_cohort <- meta_grid$cohorts[[RUN_MANDATE]]
if (is.null(meta_cohort)) {
  rlang::abort(sprintf("meta_grid has no '%s' cohort. Present: %s.",
                       RUN_MANDATE, paste(names(meta_grid$cohorts), collapse = ", ")))
}

# ---- Human-manager fund benchmarks ----------------------------------------
# Economatica daily exports have metadata in rows 1--3, headers in row 4,
# percentage simple returns thereafter, and "-" for missing observations.
read_economatica_daily_returns <- function(path) {
  header <- readxl::read_excel(
    path,
    skip = 3,
    n_max = 0,
    .name_repair = "minimal"
  )
  if (ncol(header) < 2L) {
    rlang::abort(paste0("No daily return panel found in ", path, "."))
  }

  raw <- readxl::read_excel(
    path,
    skip = 3,
    na = "-",
    col_types = c("date", rep("numeric", ncol(header) - 1L)),
    .name_repair = "minimal"
  )
  if (ncol(raw) < 2L || nrow(raw) == 0L) {
    rlang::abort(paste0("No daily return panel found in ", path, "."))
  }

  dates <- raw[[1]]
  if (is.numeric(dates)) {
    dates <- as.Date(dates, origin = "1899-12-30")
  } else {
    dates <- as.Date(dates)
  }
  if (anyNA(dates) || anyDuplicated(dates) || is.unsorted(dates, strictly = TRUE)) {
    rlang::abort(paste0("Daily dates in ", path, " must be unique, non-missing, and sorted."))
  }

  values <- vapply(raw[-1], function(x) {
    numeric_x <- suppressWarnings(as.numeric(x))
    if (any(!is.na(x) & is.na(numeric_x))) {
      rlang::abort(paste0("A non-numeric daily return was found in ", path, "."))
    }
    numeric_x
  }, numeric(nrow(raw)))

  header_parts <- strsplit(names(raw)[-1], "\n", fixed = TRUE)
  series_ids <- vapply(header_parts, function(x) trimws(x[[length(x)]]), character(1))
  if (any(!nzchar(series_ids)) || anyDuplicated(series_ids)) {
    rlang::abort(paste0("Series identifiers in ", path, " must be non-empty and unique."))
  }
  colnames(values) <- series_ids

  finite_values <- values[is.finite(values)]
  if (length(finite_values) == 0L || any(finite_values < -100)) {
    rlang::abort(paste0("Invalid percentage returns found in ", path, "."))
  }

  list(dates = dates, returns_pct = values)
}

compound_daily_returns_15th <- function(daily_data, endpoints) {
  endpoints <- as.Date(endpoints)
  expected_endpoints <- seq.Date(min(endpoints), max(endpoints), by = "month")
  if (length(endpoints) != length(expected_endpoints) ||
      any(endpoints != expected_endpoints) ||
      any(format(endpoints, "%d") != "15")) {
    rlang::abort("Monthly endpoints must be consecutive calendar 15ths.")
  }

  prior_endpoints <- c(
    as.Date(format(endpoints[[1]] - 20, "%Y-%m-15")),
    endpoints[-length(endpoints)]
  )
  out <- matrix(
    NA_real_,
    nrow = length(endpoints),
    ncol = ncol(daily_data$returns_pct),
    dimnames = list(as.character(endpoints), colnames(daily_data$returns_pct))
  )

  for (i in seq_along(endpoints)) {
    in_period <- daily_data$dates > prior_endpoints[[i]] &
      daily_data$dates <= endpoints[[i]]
    if (!any(in_period)) {
      rlang::abort(paste0("No daily rows found for the period ending ", endpoints[[i]], "."))
    }
    period_returns <- daily_data$returns_pct[in_period, , drop = FALSE]
    out[i, ] <- apply(period_returns, 2, function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0L) return(NA_real_)
      (prod(1 + x / 100) - 1) * 100
    })
  }

  out
}

# REFINED SAMPLE. The `_refined` exports are the revised universe with
# feeder/mirror vehicles removed at source, and they are the only ones that
# reach the local run's evaluation window: the original exports stop at
# 2024-05-15, while the grid run's index runs to 2026-07-15. Mixing vintages
# would leave every period after 2024-05 missing and trip the anyNA() check
# below.
funds_daily <- read_economatica_daily_returns(
  here::here("data", "bases_fundos_diarios_refined.xlsx")
)
ibov_daily <- read_economatica_daily_returns(
  here::here("data", "ibov_diarios_refined.xlsx")
)
cdi_daily <- read_economatica_daily_returns(
  here::here("data", "CDI_diarios_refined.xlsx")
)

# ---- ML backtest window ----------------------------------------------------
# Established before the fund benchmarks so every daily series can be compounded
# to the exact monthly endpoints used by the ML backtest.
meta_cohort@port_returns_m_xts_list$net_returns_m_xts <-
  restore_meta_xts_dates(meta_cohort@port_returns_m_xts_list$net_returns_m_xts)

ml_index <- meta_cohort@port_returns_m_xts_list$net_returns_m_xts@data %>%
  zoo::index()

# Geometric monthly returns in percent over (previous 15th, current 15th].
fund_ret <- compound_daily_returns_15th(funds_daily, ml_index)
ibov_ret <- as.numeric(compound_daily_returns_15th(ibov_daily, ml_index)[, 1])
cdi_ret  <- as.numeric(compound_daily_returns_15th(cdi_daily, ml_index)[, 1])
if (anyNA(ibov_ret) || anyNA(cdi_ret)) {
  rlang::abort("IBOV and CDI must cover every 15th-to-15th evaluation period.")
}

# Use one common eligibility universe for every human-manager estimand. Requiring
# 36 monthly observations excludes short-history ranking artifacts and quota
# events that straddle a 15th endpoint, while still allowing dead funds to
# contribute during the periods in which they reported.
min_fund_months <- 36L
fund_obs <- colSums(!is.na(fund_ret))
reliable_funds <- names(fund_obs)[fund_obs >= min_fund_months]
fund_ret_window <- fund_ret[, reliable_funds, drop = FALSE]

# Benchmark 1 - cross-sectional average return per period.
funds_mean <- rowMeans(fund_ret_window, na.rm = TRUE)

# Benchmark 2 - Best-in-Class: rank funds by their mean return WITHIN THE
# EVALUATION WINDOW, keep the top quartile, then take their cross-sectional
# average per period. Selection remains ex-post (not identifiable in advance),
# using only the ML evaluation period and the common eligible-fund universe.
fund_avg  <- colMeans(fund_ret_window, na.rm = TRUE)
q75       <- stats::quantile(fund_avg, 0.75, na.rm = TRUE)
top_cols  <- !is.na(fund_avg) & fund_avg >= q75
top_funds <- colnames(fund_ret_window)[top_cols]
reliable_top_funds <- top_funds
funds_q75 <- rowMeans(fund_ret_window[, top_funds, drop = FALSE], na.rm = TRUE)

# Two caveats on the ranking statistic, quantified here so the captions can cite
# measured numbers rather than assertions.
#   (i) The score is the ARITHMETIC mean of monthly returns, which rewards
#       volatility relative to a compounded (geometric) score. Overlap with a
#       geometric ranking measures how much that matters.
#  (ii) Funds enter with different history lengths, so a fund observed over a
#       short favourable stretch competes with one observed throughout.
fund_geo_score <- apply(fund_ret_window, 2, function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  (prod(1 + x / 100)^(1 / length(x)) - 1) * 100
})
geo_top_funds <- colnames(fund_ret_window)[
  !is.na(fund_geo_score) &
    fund_geo_score >= stats::quantile(fund_geo_score, 0.75, na.rm = TRUE)
]
rank_overlap_pct <- 100 * length(intersect(reliable_top_funds, geo_top_funds)) /
  length(reliable_top_funds)
rank_partial_history_pct <- 100 *
  sum(fund_obs[reliable_top_funds] < length(ml_index)) / length(reliable_top_funds)

funds_benchmarks <- tibble::tibble(
  date              = ml_index,
  funds_mean        = funds_mean,
  funds_q75         = funds_q75,
  ibov              = ibov_ret,
  funds_mean_active = funds_mean - ibov_ret,
  funds_q75_active  = funds_q75  - ibov_ret
)

# ---- Active performance of the human-manager benchmarks -------------------
# summarize_performance() consumes the market proxy positionally, so both xts
# objects share the exact ML index in the same order.
funds_window <- funds_benchmarks

# Raw (not active) returns in PERCENT: summarize_performance() rescales by 100
# and derives geometric active returns itself when active_returns = TRUE.
funds_bench_m_xts <- xts::xts(
  as.matrix(funds_window[, c("funds_mean", "funds_q75")]),
  order.by = ml_index
)
ibov_m_xts <- xts::xts(funds_window$ibov, order.by = ml_index)

funds_performance <- factoRverse:::summarize_performance(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref = funds_bench_m_xts,
  selected_market_factor_proxy_m_xts_upd_ref = ibov_m_xts,
  model_structure = "no_pooled",
  model_spec_theme_level = NULL,
  lmer_control = FALSE,
  selected_signal_themes_m_d_ref = NULL,
  active_returns = TRUE
)

# ---- Representative single-fund human benchmarks (median of the metrics) ----
# funds_performance above computes each metric ON an aggregated fund series (a
# portfolio holding all / all top-quartile funds equally), which banks the
# cross-fund diversification no single-fund investor gets. The benchmarks below
# instead compute every metric PER FUND and then take the median across funds,
# so they describe the experience of picking one fund at random. The median
# (rather than the mean) is robust to ratios whose denominator can approach zero
# -- e.g. the Martin ratio explodes for funds with near-zero active drawdown.
# summarize_performance() is vectorized over columns, so the whole eligible
# fund panel goes through in one call.

fund_window_m_xts <- xts::xts(fund_ret_window, order.by = ml_index)

per_fund_performance <- factoRverse:::summarize_performance(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref = fund_window_m_xts,
  selected_market_factor_proxy_m_xts_upd_ref = ibov_m_xts,
  model_structure = "no_pooled",
  model_spec_theme_level = NULL,
  lmer_control = FALSE,
  selected_signal_themes_m_d_ref = NULL,
  active_returns = TRUE
)$signal_universe_m_d_ref

# Cross-fund median of each table metric (matched by fund name so the
# top-quartile subset stays aligned). These are cross-fund medians of fund-level
# statistics, not portfolio statistics: the reported ratios and t-statistic
# describe the median fund, not a tradable portfolio.
med_fund_row <- function(fund_names, label) {
  m <- per_fund_performance %>% dplyr::filter(tickers %in% fund_names)
  tibble::tibble(
    portfolio = label,
    ann_ret = stats::median(m$act_ann_ret, na.rm = TRUE) / 100,
    te      = stats::median(m$ann_track_err, na.rm = TRUE) / 100,
    ir      = stats::median(m$ann_info_ratio, na.rm = TRUE),
    aes     = stats::median(m$act_exp_short, na.rm = TRUE) / 100,
    ulcer   = stats::median(m$act_ulcer, na.rm = TRUE) / 100,
    mr      = stats::median(m$act_ann_martin_ratio, na.rm = TRUE),
    alpha   = stats::median(m$alpha, na.rm = TRUE),
    alpha_t = stats::median(m$alpha_t_stat, na.rm = TRUE)
  )
}

med_fund_rows <- dplyr::bind_rows(
  med_fund_row(reliable_funds, "MED"),
  med_fund_row(reliable_top_funds, "MED_Q75")
)

# ---- ML meta-portfolio performance, net of a fictitious management fee -----
# A 1% p.a. fee is monthlised and removed geometrically, putting the machine
# portfolios on the same net-of-fee footing as the human managers.
mgmt_fee_annual  <- 0.01
mgmt_fee_monthly <- (1 + mgmt_fee_annual)^(1 / 12) - 1

deduct_management_fee <- function(returns_pct, monthly_fee = mgmt_fee_monthly) {
  ((1 + returns_pct / 100) / (1 + monthly_fee) - 1) * 100
}

ml_returns_m_xts <- meta_cohort@port_returns_m_xts_list$net_returns_m_xts@data

# The benchmark column carries no fee; every other column is a portfolio.
bench_col <- "selected_bench_return"
port_cols <- setdiff(colnames(ml_returns_m_xts), bench_col)

# This fee-adjusted object is the only ML ensemble return input used below.
ml_ports_net_m_xts <- deduct_management_fee(ml_returns_m_xts[, port_cols])
ml_bench_m_xts <- ml_returns_m_xts[, bench_col]
if (max(abs(as.numeric(ml_bench_m_xts) - ibov_ret), na.rm = TRUE) > 0.01) {
  rlang::abort("Daily-compounded IBOV does not reconcile to the ML benchmark within 1 bp.")
}

ml_performance <- factoRverse:::summarize_performance(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref = ml_ports_net_m_xts,
  selected_market_factor_proxy_m_xts_upd_ref = ml_bench_m_xts,
  model_structure = "no_pooled",
  model_spec_theme_level = NULL,
  lmer_control = FALSE,
  selected_signal_themes_m_d_ref = NULL,
  active_returns = TRUE
)

# ---- Shared helpers for the active-performance LaTeX tables ----------------
# summarize_performance() reports returns/risk in percent; the tables follow
# the paper's convention of fractions, so the percent columns are divided by 100.
format_active_rows <- function(perf_df, label_fun) {
  perf_df %>%
    dplyr::transmute(
      portfolio = label_fun(tickers),
      ann_ret   = act_ann_ret / 100,
      te        = ann_track_err / 100,
      ir        = ann_info_ratio,
      aes       = act_exp_short / 100,
      ulcer     = act_ulcer / 100,
      mr        = act_ann_martin_ratio,
      alpha     = alpha,                     # percent per month
      alpha_t   = alpha_t_stat
    )
}

tex_metric_rows <- function(tbl) {
  sprintf(
    "%s & %.3f & %.3f & %.2f & %.3f & %.3f & %.2f & %.3f & %.2f \\\\",
    gsub("_", "\\\\_", tbl$portfolio),
    tbl$ann_ret, tbl$te, tbl$ir, tbl$aes, tbl$ulcer, tbl$mr,
    tbl$alpha, tbl$alpha_t
  )
}

tex_panel_header <- function(label) {
  paste0("\\multicolumn{9}{l}{\\textit{", label, "}} \\\\")
}

active_col_preamble <- paste0(
  "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{2.7cm}",
  "*{8}{>{\\raggedleft\\arraybackslash}X}}"
)
active_col_header <- paste(
  "Portfolio & Ret. & TE & IR & AES & Ulcer & MR &",
  "$\\alpha$ & $t(\\alpha)$ \\\\"
)

# ---- Table 3: ML Ensemble vs human-manager benchmarks ---------------------
# Strip the backtest naming convention down to the portfolio label, e.g.
# "c__meta_rp_box_ibov_s__comdinheiro_pregold_f__comdinheiro_target" -> "RP_BOX".
pretty_port_label <- function(x) {
  x <- sub("^c__meta_", "", x)
  x <- sub("_ibov_s__.*$", "", x)
  toupper(x)
}

ml_rows   <- format_active_rows(ml_performance$signal_universe_m_d_ref, pretty_port_label)
fund_rows <- format_active_rows(funds_performance$signal_universe_m_d_ref, pretty_port_label)

active_table <- dplyr::bind_rows(ml_rows, fund_rows, med_fund_rows)  # console inspection

ensemble_table_tex <- c(
  "\\begin{table}[h]",
  "\\captionsetup{",
  "  justification=raggedright,",
  "  labelsep=none,",
  "  singlelinecheck=false,",
  "  labelformat=boldlabel,",
  "}",
  "\\renewcommand{\\arraystretch}{1.25}",
  paste0(
    "\\caption[Out-of-sample net active summary statistics of the ML Ensemble]",
    "{\\footnotesize \\textbf{: Out-of-sample ",
    "net active summary statistics of the ML Ensemble.} This table reports ",
    "out-of-sample net active summary statistics for the ML Ensemble portfolios and ",
    "the human-manager benchmarks. The ML Ensemble averages the return predictions ",
    "across all ML models (OLS, Elastic Net, Random Forest, XGBoost and three neural ",
    "networks) before portfolio construction; the ML rows differ only by the ",
    "portfolio-construction scheme. Annualized Active Geometric Return ",
    "(Ret.), Tracking Error (TE), Information Ratio (IR), Active ",
    "Expected Shortfall (AES), Active Ulcer Index (Ulcer), Annualized Active Martin ",
    "Ratio (MR), CAPM alpha ($\\alpha$, in percent per month) and its t-statistic ",
    "($t(\\alpha)$) are displayed. IR here is the geometrically compounded ",
    "annualized active return divided by the tracking error; the combination and ",
    "fund-picking tables use the arithmetic-mean convention and therefore report ",
    "systematically lower values for the same portfolios. ",
    "ML portfolio returns are net of transaction costs ",
    "and of a 1\\% p.a. management fee. The human-manager benchmarks come in two ",
    "forms. FUNDS\\_MEAN and FUNDS\\_Q75 apply the metrics to a single aggregated ",
    "return series --- an equally weighted portfolio of, respectively, all eligible ",
    "funds and the top-quartile eligible funds (ranked by mean return over the ",
    "evaluation window) --- ",
    "and therefore ",
    "embed the diversification of holding many funds at once. MED and ",
    "MED\\_Q75 instead compute each metric for every fund individually and then ",
    "report the median across funds, describing the experience of an investor who ",
    "picks a single (respectively, any or top-quartile) fund. Every human-manager ",
    "benchmark uses the same universe of funds with at least 36 monthly observations. ",
    "Because that screen admits funds with partial histories, top-quartile ",
    "membership is assigned over differing sample lengths; ",
    "Figure~\\ref{fig:picker_quantile} quantifies how far the classification and ",
    "the resulting Information Ratios depend on that choice. The median (rather ",
    "than the mean) is used because averaging non-linear ratios across funds is ",
    "dominated by small-denominator outliers; the reported ratios and $t(\\alpha)$ ",
    "are therefore cross-fund medians of fund-level quantities, not statistics of a ",
    "tradable portfolio.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  active_col_preamble,
  "\\hline",
  active_col_header,
  "\\hline",
  tex_panel_header("ML Ensemble --- average across all ML models"),
  tex_metric_rows(ml_rows),
  "\\hline",
  tex_panel_header("Benchmarks --- fund portfolios (metric of the average)"),
  tex_metric_rows(fund_rows),
  "\\hline",
  tex_panel_header("Benchmarks --- representative single fund (median across funds)"),
  tex_metric_rows(med_fund_rows),
  "\\hline",
  "\\end{tabularx}",
  "\\label{actret}",
  "\\end{table}"
)

dir.create(here::here("tables"), showWarnings = FALSE)
writeLines(ensemble_table_tex, here::here("tables", "active_performance.tex"))

# ---- Table 4: active performance across the individual ML models -----------
# Same workflow as the ensemble: restore the real dates on the backtest object
# (mirroring the meta-portfolio treatment above), remove the 1% p.a. fee, then
# summarize against the run's own benchmark column.
#
# Source is the `learners` stage of run_paper_machine.R, which turns each base
# learner into a cs and an rp portfolio under the ibov mandate using the same
# paper_meta_port_config() the meta portfolios use -- so these columns are
# directly comparable to meta_cs_ibov / meta_rp_ibov above. Columns are named
# learner_<cs|rp>_<config>.
mb_net_returns_obj <- restore_meta_xts_dates(
  learners$cohort@port_returns_m_xts_list$net_returns_m_xts
)
mb_returns_m_xts <- mb_net_returns_obj@data

# Each learner portfolio starts one period into the base-learner OOS window
# (initial_buffer_period = 1), which reaches further back than the ensemble's:
# 133 periods here against ml_index's 128. Trim to ml_index so both tables
# report the same out-of-sample period.
stopifnot(all(ml_index %in% zoo::index(mb_returns_m_xts)))
mb_returns_m_xts <- mb_returns_m_xts[zoo::index(mb_returns_m_xts) %in% ml_index, ]
stopifnot(nrow(mb_returns_m_xts) == length(ml_index),
          all(zoo::index(mb_returns_m_xts) == ml_index))

mb_port_cols <- setdiff(colnames(mb_returns_m_xts), bench_col)
mb_ports_net_m_xts <- deduct_management_fee(mb_returns_m_xts[, mb_port_cols])
mb_bench_m_xts <- mb_returns_m_xts[, bench_col]

mb_performance <- factoRverse:::summarize_performance(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref = mb_ports_net_m_xts,
  selected_market_factor_proxy_m_xts_upd_ref = mb_bench_m_xts,
  model_structure = "no_pooled",
  model_spec_theme_level = NULL,
  lmer_control = FALSE,
  selected_signal_themes_m_d_ref = NULL,
  active_returns = TRUE
)

# Column names carry both the model and the construction scheme, e.g.
# "c__learner_rp_glmnet_grid_ind_signals_backtest_results_s__..." -> model
# GLMNET, scheme RP. The config name is set by the `learners` stage; the tuning
# arm in the middle is `grid` or `bayes` depending on ARM, so strip either.
model_from_col <- function(x) {
  x <- sub("^c__learner_(?:cs|rp)_", "", x)          # drop the portfolio wrapper
  x <- sub("_s__.*$", "", x)                          # drop signals/features tail
  x <- sub("_ind_signals_backtest_results$", "", x)   # drop the dataset suffix
  toupper(sub("_(grid|bayes)$", "", x))               # drop the tuning arm
}
scheme_from_col <- function(x) toupper(sub("^c__learner_(cs|rp)_.*$", "\\1", x))

model_perf   <- mb_performance$signal_universe_m_d_ref
model_tbl    <- format_active_rows(model_perf, model_from_col)
model_tbl$scheme <- scheme_from_col(model_perf$tickers)

model_order <- c("OLS", "GLMNET", "RF", "XGB", "NN1", "NN2", "NN3")
order_by_model <- function(tbl) tbl[order(factor(tbl$portfolio, levels = model_order)), ]
cs_tbl <- order_by_model(model_tbl[model_tbl$scheme == "CS", ])
rp_tbl <- order_by_model(model_tbl[model_tbl$scheme == "RP", ])

model_table_tex <- c(
  "\\begin{table}[h]",
  "\\captionsetup{",
  "  justification=raggedright,",
  "  labelsep=none,",
  "  singlelinecheck=false,",
  "  labelformat=boldlabel,",
  "}",
  "\\renewcommand{\\arraystretch}{1.25}",
  paste0(
    "\\caption[Out-of-sample net active summary statistics across ML models]",
    "{\\footnotesize \\textbf{: Out-of-sample ",
    "net active summary statistics across ML models.} This table reports ",
    "out-of-sample net active summary statistics for portfolios built from each ",
    "individual ML model, under Cross-Sectional (CS) and Risk-Parity (RP) ",
    "construction. Models are OLS, Elastic Net (GLMNET), Random Forest (RF), ",
    "XGBoost (XGB) and three neural networks (NN1--NN3). Annualized Active ",
    "Geometric Return (Ret.), Tracking Error (TE), Information ",
    "Ratio (IR), Active Expected Shortfall (AES), Active Ulcer Index (Ulcer), ",
    "Annualized Active Martin Ratio (MR), CAPM alpha ($\\alpha$, in percent per ",
    "month) and its t-statistic ($t(\\alpha)$) are displayed. Portfolio returns are ",
    "net of transaction costs and of a 1\\% p.a. management fee.}"
  ),
  "\\footnotesize",
  active_col_preamble,
  "\\hline",
  active_col_header,
  "\\hline",
  tex_panel_header("Cross-Sectional weighting (CS)"),
  tex_metric_rows(cs_tbl),
  "\\hline",
  tex_panel_header("Risk Parity (RP)"),
  tex_metric_rows(rp_tbl),
  "\\hline",
  "\\end{tabularx}",
  "\\label{modelret}",
  "\\end{table}"
)

writeLines(model_table_tex, here::here("tables", "model_performance.tex"))

# ---- Shared plotting / LaTeX helpers ---------------------------------------
# Journal-theme styling shared by the composition and feature-importance grids.
plot_base_size <- 19

journal_area_theme <- function(base_size = plot_base_size) {
  jp <- factoRverse:::.journal_palette()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = jp$background, color = NA),
      panel.background = ggplot2::element_rect(fill = jp$background, color = NA),
      panel.grid.major = ggplot2::element_line(color = jp$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text    = ggplot2::element_text(color = jp$text),
      axis.title   = ggplot2::element_text(color = jp$text),
      plot.title   = ggplot2::element_text(color = jp$text, face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(color = jp$text, size = base_size),
      strip.text   = ggplot2::element_text(color = jp$text, face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = jp$text),
      legend.text  = ggplot2::element_text(color = jp$text)
    )
}

# Paste-ready LaTeX figure block. Requires \usepackage{graphicx} and
# \usepackage{caption}. The image path is relative to the manuscript .tex;
# adjust `fig_dir` if figures/ sits elsewhere.
# No float number is emitted: LaTeX auto-numbers the float, and a hard-coded
# number in the optional (list-of-figures) argument silently goes stale whenever
# floats are reordered. The short caption is the title instead.
latex_figure <- function(file, bold_title, body, label, fig_dir = "figures",
                         include_options = "width=\\linewidth") {
  c(
    "\\begin{figure}[htbp]",
    "\\centering",
    "\\captionsetup{",
    "  justification=raggedright,",
    "  labelsep=none,",
    "  singlelinecheck=false,",
    "  labelformat=boldlabel,",
    "}",
    sprintf("\\caption[%s]{\\footnotesize \\textbf{: %s} %s}",
            bold_title, bold_title, body),
    sprintf("\\includegraphics[%s]{%s/%s}", include_options, fig_dir, file),
    sprintf("\\label{%s}", label),
    "\\end{figure}"
  )
}

dir.create(here::here("figures"), showWarnings = FALSE)

# ---- Benjamini--Hochberg signal eligibility through time -------------------
# Replicate the ``Eligibility by Theme'' view of the stored individual-signal
# frequentist BH result, while passing all tickers and dates explicitly to
# avoid the interactive selections in plot.meta_dataframe().
bh_ss_results <- ss$ss_backtest_results_lists$freq_ss_backtest_results_lists$
  ind_signals$freq_np_BH
if (is.null(bh_ss_results)) {
  rlang::abort("The individual-signal freq_np_BH result is absent from ss.qs.")
}

bh_signal_universe <- bh_ss_results@signal_universe_m_df
bh_signal_universe@data <- bh_signal_universe@data %>%
  dplyr::filter(.data$theme != "forced") %>%
  dplyr::arrange(.data$theme, .data$tickers, .data$dates) %>%
  dplyr::mutate(
    # "tickers" is the package's storage name for characteristic identifiers.
    # Use a display variable so no figure label describes them as stock tickers.
    signals = factor(.data$tickers, levels = unique(.data$tickers)),
    eligibility = dplyr::if_else(
      .data$is_eligible == 1, "Elected", "Not elected"
    )
  )

bh_eligibility_plot <- ggplot2::ggplot(
  bh_signal_universe@data,
  ggplot2::aes(x = .data$dates, y = .data$signals, fill = .data$eligibility)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.08) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(.data$theme),
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::scale_fill_manual(
    values = c("Elected" = "#C51B33", "Not elected" = "#1F4E79")
  ) +
  ggplot2::scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::labs(
    title = "Benjamini--Hochberg signal eligibility",
    subtitle = "All signals and annual screening dates",
    x = "Screening date",
    y = "Signals",
    fill = "Eligibility"
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 10),
    axis.text.x = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_text(size = 11),
    legend.text = ggplot2::element_text(size = 9),
    legend.title = ggplot2::element_text(size = 10),
    plot.title = ggplot2::element_text(size = 16),
    plot.subtitle = ggplot2::element_text(size = 10),
    panel.grid = ggplot2::element_blank(),
    panel.spacing.y = grid::unit(1.5, "mm"),
    strip.text.y = ggplot2::element_text(angle = 0, size = 8),
    strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(4, 4, 4, 2)
  )

ggplot2::ggsave(
  here::here("figures", "bh_eligibility_heatmap.png"),
  bh_eligibility_plot,
  width = 10, height = 18, dpi = 300, bg = "white"
)

bh_eligibility_figure_tex <- latex_figure(
  "bh_eligibility_heatmap.png",
  "Signal eligibility under the Benjamini--Hochberg adjustment.",
  paste(
    "Each row is an individual non-sector signal and each column is an",
    "annual screening date. Tiles identify characteristics selected by the 5\\%",
    "Benjamini--Hochberg procedure using an expanding 81-month initial estimation",
    "window. All available characteristics and screening dates are shown."
  ),
  "fig:bh_eligibility",
  include_options = "width=0.70\\linewidth,height=0.85\\textheight,keepaspectratio"
)
writeLines(
  bh_eligibility_figure_tex,
  here::here("figures", "bh_eligibility_figure.tex")
)

# ---- Feature importance across ML models -----------------------------------
# For each model in the individual-signals run, factoRverse stores a
# feature_importance_m_df: a global OLS surrogate of the model output on the
# same standardized signals. Its `importance` column is the signed surrogate
# coefficient. Do not aggregate `normalized_importance` here: factoRverse
# z-scores the coefficient vector cross-sectionally, so an exact zero becomes
# negative whenever the mean coefficient is positive. That transformation is
# useful for ranking individual coefficients, but it creates spurious theme
# exposure and invalid net/gross totals.
#
# Aggregate raw coefficients first and then divide both net and gross theme
# loadings by the model-date total gross loading. Gross shares sum to one within
# model-date; the signed net share shows what remains after cancellation.
sb_list <- mb_grid@base_sb_backtest_results_list

# Compact model label from the backtest element name:
# "c__glmnet_grid_f__..._ind_signals" -> "GLMNET"; "c__ols_f__..." -> "OLS".
# The tuning arm embedded in the name follows ARM, so accept `grid` as well as
# `bayes` -- matching only `_bayes$` left the grid run labelled "GLMNET_GRID",
# which no longer matched fi_model_order and silently emptied those facets.
fi_model_label <- function(nm) {
  toupper(sub("_(grid|bayes)$", "", sub("_f__.*$", "", sub("^c__", "", nm))))
}
fi_model_order <- c("OLS", "GLMNET", "RF", "XGB", "NN1", "NN2", "NN3")

# Long feature-importance table across all models, themes only (drop the
# intercept and any untagged rows).
fi_all <- purrr::imap_dfr(sb_list, function(sbr, nm) {
  d <- sbr@feature_importance_m_df@data %>%
    dplyr::filter(!is.na(theme)) %>%
    dplyr::select(dates, theme, importance)
  d$model <- fi_model_label(nm)
  d
})
fi_all$model <- factor(fi_all$model, levels = intersect(fi_model_order, unique(fi_all$model)))

# Display relabelling of theme values (e.g. "forced" -> "industries").
fi_all$theme <- dplyr::recode(fi_all$theme, forced = "industries")

# Order themes by aggregate absolute raw importance so all figures are aligned.
theme_order <- fi_all %>%
  dplyr::group_by(theme) %>%
  dplyr::summarise(m = sum(abs(importance), na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(m) %>%
  dplyr::pull(theme)
fi_all$theme <- factor(fi_all$theme, levels = theme_order)

jp_fi <- factoRverse:::.journal_palette()

# Diverging bars: net (signed sum) importance per theme, last period, by model,
# drawn INSIDE a grey +/- gross envelope.
#
# Net alone cannot distinguish no loading from large offsetting loadings. Gross
# is sum |beta| within the theme: the largest net the theme could show if every
# signal pointed the same way. Dividing by model total gross makes the envelopes
# comparable across learners while preserving cancellation.
fi_bar <- fi_all %>%
  dplyr::group_by(model) %>%
  dplyr::filter(dates == max(dates)) %>%
  dplyr::group_by(model, theme) %>%
  dplyr::summarise(net = sum(importance, na.rm = TRUE),
                   gross = sum(abs(importance), na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(total_gross = sum(gross),
                net_share = net / total_gross,
                gross_share = gross / total_gross,
                alignment = sum(abs(net)) / total_gross,
                sign = ifelse(net >= 0, "Positive", "Negative")) %>%
  dplyr::ungroup()

fi_bar_grid <- ggplot2::ggplot(fi_bar, ggplot2::aes(x = theme)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = -gross_share, ymax = gross_share),
                          colour = "grey85", linewidth = 3.4) +
  ggplot2::geom_col(ggplot2::aes(y = net_share, fill = sign), width = 0.55) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.25) +
  ggplot2::facet_wrap(~model, ncol = 3) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = c(Positive = jp_fi$primary, Negative = jp_fi$negative),
                             guide = "none") +
  ggplot2::labs(x = NULL,
                y = "Share of model gross loading: net = coloured bar, gross = grey envelope",
                title = "Net vs gross feature importance by theme across ML models (last period)",
                subtitle = "Raw GSM coefficients aggregated by theme, then scaled by each model's total gross loading") +
  journal_area_theme()

# Tile heatmap of net (summed) importance per theme over time, faceted by model.
fi_heat <- fi_all %>%
  dplyr::group_by(model, dates, theme) %>%
  dplyr::summarise(net = sum(importance, na.rm = TRUE),
                   gross = sum(abs(importance), na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::group_by(model, dates) %>%
  dplyr::mutate(total_gross = sum(gross),
                net_share = net / total_gross,
                gross_share = gross / total_gross) %>%
  dplyr::ungroup()

fi_heat_base_size <- 16
fi_heat_grid <- ggplot2::ggplot(fi_heat, ggplot2::aes(x = dates, y = theme, fill = net_share)) +
  ggplot2::geom_tile() +
  ggplot2::facet_wrap(~model, ncol = 2) +
  ggplot2::scale_fill_gradient2(low = jp_fi$negative, mid = "#EBEEF1", high = jp_fi$primary,
                                midpoint = 0, name = "Net share") +
  ggplot2::guides(fill = ggplot2::guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = grid::unit(8, "cm"),
    barheight = grid::unit(0.45, "cm")
  )) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Net feature importance by theme over time across ML models") +
  journal_area_theme(base_size = fi_heat_base_size) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = fi_heat_base_size - 2),
    strip.text = ggplot2::element_text(size = fi_heat_base_size),
    legend.text = ggplot2::element_text(size = fi_heat_base_size - 4),
    legend.title = ggplot2::element_text(size = fi_heat_base_size - 2)
  )

# Companion gross-share heatmap. The net map cannot separate no loading from
# offsetting loadings; the gross share shows where coefficient mass is located
# without allowing signs to cancel.
fi_heat_gross_grid <- ggplot2::ggplot(fi_heat,
                                      ggplot2::aes(x = dates, y = theme, fill = gross_share)) +
  ggplot2::geom_tile() +
  ggplot2::facet_wrap(~model, ncol = 2) +
  ggplot2::scale_fill_gradient(low = "#EBEEF1", high = jp_fi$primary,
                               limits = c(0, 1), name = "Gross share") +
  ggplot2::guides(fill = ggplot2::guide_colorbar(
    title.position = "top", title.hjust = 0.5,
    barwidth = grid::unit(8, "cm"), barheight = grid::unit(0.45, "cm")
  )) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Gross feature importance by theme over time across ML models",
                subtitle = "Theme sum of absolute GSM coefficients as a share of model-date total gross loading") +
  journal_area_theme(base_size = fi_heat_base_size) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = fi_heat_base_size - 2),
    strip.text = ggplot2::element_text(size = fi_heat_base_size),
    legend.text = ggplot2::element_text(size = fi_heat_base_size - 4),
    legend.title = ggplot2::element_text(size = fi_heat_base_size - 2)
  )

ggplot2::ggsave(here::here("figures", "feature_importance_bar.png"),
                fi_bar_grid, width = 14, height = 11, dpi = 300, bg = "white")
ggplot2::ggsave(here::here("figures", "feature_importance_heatmap.png"),
                fi_heat_grid, width = 14, height = 12, dpi = 300, bg = "white")
ggplot2::ggsave(here::here("figures", "feature_importance_heatmap_gross.png"),
                fi_heat_gross_grid, width = 14, height = 12, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the feature-importance figures ------------------
feature_importance_figures_tex <- c(
  latex_figure(
    "feature_importance_bar.png",
    "Net and gross feature importance by theme across ML models (last period).",
    paste(
      "Feature importance is a global OLS surrogate of each model's output on",
      "the same standardized signals, so the raw loading is a signed regression",
      "coefficient. Coefficients are first aggregated by theme and then divided",
      "by each model's total gross loading. The \\emph{coloured bar} is the net",
      "signed loading; the \\emph{grey envelope} is the gross loading",
      "$\\sum|\\beta|$. Gross shares sum to one within a model. A coloured bar",
      "that fills its envelope indicates aligned signals, whereas a short bar",
      "inside a wide envelope indicates offsetting coefficients. At the final",
      "refit the ratio of total absolute net theme loading to total gross loading",
      "is 1.00 for GLMNET, 0.59 for NN2, 0.56 for NN1, 0.50 for RF, 0.26 for",
      "NN3, 0.25 for XGB and 0.08 for OLS. The OLS result reflects large",
      "opposing coefficients on correlated momentum signals, rather than a lack",
      "of momentum exposure. Themes are ordered by aggregate gross importance."
    ),
    "fig:fi_bar"
  ),
  "",
  latex_figure(
    "feature_importance_heatmap.png",
    "Net feature importance by theme over time across ML models.",
    paste(
      "At each refit, raw GSM coefficients are summed within theme and divided",
      "by that model-date's total gross loading. Colour therefore encodes the",
      "signed net theme share on a common diverging scale. A stable colour block",
      "indicates persistence in the sign and relative importance of a theme.",
      "Near-white cells remain ambiguous between low exposure and offsetting",
      "signals and should be read with Figure~\\ref{fig:fi_heatmap_gross}."
    ),
    "fig:fi_heatmap"
  ),
  "",
  latex_figure(
    "feature_importance_heatmap_gross.png",
    "Gross feature importance by theme over time across ML models.",
    paste(
      "For each model and refit, the sum of absolute raw GSM coefficients within",
      "a theme is divided by total gross loading, so theme shares sum to one.",
      "The figure measures where each surrogate allocates coefficient mass",
      "without allowing signs to cancel. Read with Figure~\\ref{fig:fi_heatmap}:",
      "a dark gross cell paired with a pale net cell indicates offsetting",
      "signals within that theme. This distinction is especially important for",
      "the unpenalized OLS surrogate and the deeper nonlinear learners."
    ),
    "fig:fi_heatmap_gross"
  )
)

writeLines(feature_importance_figures_tex,
           here::here("figures", "feature_importance_figures.tex"))

# ---- Composition of a portfolio quantity by group over time ----------------
# Unlike the built-in "composition" plot (which counts holdings in the
# universe), this sums a portfolio quantity (portfolio weight, or relative risk
# contribution) within each group at every rebalance date and shows the mix
# over time, styled with the journal theme. Everything is read from the
# stock_universe_m_df, which already carries weights, rel_risk_contr and the
# grouping columns side by side.

# Tidy per-date share of `value_col` within each group of `variable`. Numeric
# grouping variables are binned into cross-sectional quantiles per date. `data`
# overrides the default source (pbr@stock_universe_m_df@data) when supplied.
composition_df <- function(pbr, variable, value_col = "weights",
                           numeric_bins = 4L, data = NULL, group_relabel = NULL) {
  df0 <- if (is.null(data)) pbr@stock_universe_m_df@data else data

  for (col in c("tickers", "dates", variable, value_col)) {
    if (!col %in% colnames(df0)) {
      stop(sprintf("'%s' is not a column of the data. Available columns: %s",
                   col, paste(colnames(df0), collapse = ", ")))
    }
  }

  df <- tibble::tibble(
    dates = df0$dates,
    value = df0[[value_col]],
    gv    = df0[[variable]]
  ) %>%
    dplyr::filter(!is.na(gv), !is.na(value))

  if (is.numeric(df$gv)) {
    lbls <- sprintf("Q%d", seq_len(numeric_bins))
    df <- df %>%
      dplyr::group_by(dates) %>%
      dplyr::mutate(group = as.character(factor(dplyr::ntile(gv, numeric_bins),
                                                levels = seq_len(numeric_bins), labels = lbls))) %>%
      dplyr::ungroup()
  } else {
    df <- df %>% dplyr::mutate(group = as.character(gv))
  }

  # Optional display relabelling of group values, e.g. c(forced = "industries").
  if (!is.null(group_relabel)) {
    df$group <- dplyr::recode(df$group, !!!group_relabel)
  }

  comp <- df %>%
    dplyr::group_by(dates, group) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # position = "fill" assumes non-negative contributions; warn if a scheme goes
  # short so distorted panels are not read as real composition.
  if (any(comp$value < 0)) {
    warning(sprintf("'%s' has negative values; the filled composition may be misleading.",
                    value_col))
  }
  comp
}

# Single-portfolio composition.
plot_composition <- function(pbr, variable, value_col = "weights",
                             numeric_bins = 4L, fill_lab = variable,
                             y_lab = "Share of portfolio") {
  jp   <- factoRverse:::.journal_palette()
  comp <- composition_df(pbr, variable, value_col, numeric_bins)
  comp$group <- factor(comp$group, levels = sort(unique(comp$group)))
  cols <- rep(jp$categorical, length.out = nlevels(comp$group))

  ggplot2::ggplot(comp, ggplot2::aes(x = dates, y = value, fill = group)) +
    ggplot2::geom_area(position = "fill", color = jp$background, linewidth = 0.1) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = y_lab, fill = fill_lab) +
    journal_area_theme()
}

# Strip the cohort naming down to a compact portfolio label:
# "meta_rp_box_ibov" -> "RP_BOX".
pretty_meta_label <- function(x) toupper(sub("_ibov$", "", sub("^meta_", "", x)))

# Grid of compositions faceted by portfolio, sharing one fill scale. Optionally
# appends a benchmark panel (e.g. IBOV via `bench_value_col = "ibov_bench_weights"`)
# computed from the first portfolio's universe. `group_levels` fixes the legend/
# stacking order (else groups are sorted alphabetically).
plot_composition_grid <- function(pbr_list, variable, value_col = "weights",
                                  numeric_bins = 4L, fill_lab = variable,
                                  y_lab = "Share of portfolio",
                                  title = NULL, ncol = 2L, group_levels = NULL,
                                  bench_value_col = NULL, bench_label = "IBOV",
                                  group_relabel = NULL) {
  jp <- factoRverse:::.journal_palette()

  comp_all <- purrr::imap_dfr(pbr_list, function(pbr, nm) {
    d <- composition_df(pbr, variable, value_col, numeric_bins, group_relabel = group_relabel)
    d$portfolio <- pretty_meta_label(nm)
    d
  })
  port_levels <- pretty_meta_label(names(pbr_list))

  # Benchmark panel: same grouping, but weighted by the benchmark's own weights.
  if (!is.null(bench_value_col)) {
    b <- composition_df(pbr_list[[1]], variable, bench_value_col, numeric_bins,
                        group_relabel = group_relabel)
    b$portfolio <- bench_label
    comp_all <- dplyr::bind_rows(comp_all, b)
    port_levels <- c(port_levels, bench_label)
  }

  grp_lv <- if (is.null(group_levels)) sort(unique(comp_all$group)) else
    union(group_levels, sort(unique(comp_all$group)))
  comp_all$group     <- factor(comp_all$group, levels = grp_lv)
  comp_all$portfolio <- factor(comp_all$portfolio, levels = port_levels)
  cols <- rep(jp$categorical, length.out = nlevels(comp_all$group))

  ggplot2::ggplot(comp_all, ggplot2::aes(x = dates, y = value, fill = group)) +
    ggplot2::geom_area(position = "fill", color = jp$background, linewidth = 0.1) +
    ggplot2::facet_wrap(~portfolio, ncol = ncol) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = y_lab, fill = fill_lab, title = title) +
    journal_area_theme()
}

# Weight composition across ALL ensemble portfolios in the cohort, by sector
# and by liquidity class (the size proxy). Weights are non-negative, so the
# 100%-stacked composition is well defined; both the weights and the grouping
# columns live in the stock universe, so no external join is needed.
pbr_list <- meta_cohort@port_backtest_results_list

# Largest to smallest, so the legend and stacking read top-down by size.
liquidity_levels <- c("mega_caps", "large_caps", "mid_caps", "small_caps", "micro_caps")

sector_grid <- plot_composition_grid(
  pbr_list, "sectors_c2", value_col = "weights", fill_lab = "Sector",
  y_lab = "Share of portfolio weight", ncol = 3,
  bench_value_col = "ibov_bench_weights", bench_label = "IBOV",
  group_relabel = c(forced = "industries"),
  title = "Weight composition by sector across portfolios"
)
liquidity_grid <- plot_composition_grid(
  pbr_list, "liquidity_classification", value_col = "weights", fill_lab = "Liquidity-based size class",
  y_lab = "Share of portfolio weight", ncol = 3, group_levels = liquidity_levels,
  bench_value_col = "ibov_bench_weights", bench_label = "IBOV",
  title = "Weight composition by liquidity-based size class"
)

dir.create(here::here("figures"), showWarnings = FALSE)
ggplot2::ggsave(here::here("figures", "composition_sector_weight.png"),
                sector_grid, width = 14, height = 12, dpi = 300, bg = "white")
ggplot2::ggsave(here::here("figures", "composition_liquidity_weight.png"),
                liquidity_grid, width = 14, height = 12, dpi = 300, bg = "white")


# ---- Paste-ready LaTeX for the composition figures -------------------------
composition_figures_tex <- c(
  latex_figure(
    "composition_sector_weight.png",
    "Portfolio weight composition by sector.",
    paste(
      "Out-of-sample monthly composition of portfolio weight across economic",
      "sectors, for each ML-ensemble portfolio-construction scheme (RP, RP\\_BOX,",
      "HRP, CS, SW, EW, MVO, MVO\\_BOX) and the IBOV benchmark. Each panel stacks",
      "the sum of portfolio weights within each sector to 100\\% at every",
      "rebalance date."
    ),
    "fig:comp_sector"
  ),
  "",
  latex_figure(
    "composition_liquidity_weight.png",
    "Portfolio weight composition by liquidity-based size class.",
    paste(
      "Out-of-sample monthly composition of portfolio weight across",
      "liquidity-based size classes (mega, large, mid, small, micro and nano caps),",
      "for each ML-ensemble portfolio-construction scheme and the IBOV benchmark.",
      "Each panel stacks the sum of portfolio weights within each size class to",
      "100\\% at every rebalance date. The classes are assigned using inflation-adjusted",
      "three-month average daily trading value and trading-day participation; they are",
      "therefore capacity categories rather than direct market-capitalization bins."
    ),
    "fig:comp_mktcap"
  )
)

writeLines(composition_figures_tex, here::here("figures", "composition_figures.tex"))

# ---- Average portfolio characteristics over time ---------------------------
# port_metrics_m_xts holds the weighted-average of each stock characteristic
# (sum(eop_port_weights * metric)); the benchmark's weighted version carries a
# "bench_" prefix and is identical across schemes. One panel per characteristic,
# one line per ML-ensemble portfolio scheme plus the IBOV benchmark.
# One metric per factor family: momentum, value, quality, quality, value,
# low-vol, size, interaction, growth.
#
# The growth slot was `fcf_be_12m_chg3y`, which the current metric set no longer
# produces -- compute_meta_port_metrics() (pipeline R/helpers/
# backtest_meta_portfolios.R) computes no FCF/BE change variant at all. Its
# growth block is roe_12m_gr3y / ocf_at_12m_gr3y / gross_mrg_12m_gr3y, so the
# slot takes roe_12m_gr3y: still a profitability ratio's 3-year change, which
# keeps the family represented. Say so in the paper rather than presenting it as
# the original series.
char_metrics <- c("sharpe_12m", "ni_me", "roe_12m", "gp_at_12m", "div_me_med_36m",
                  "ret_vol_12m", "log_mkt_cap", "qmj", "roe_12m_gr3y")

# Human-readable panel labels (verify against the paper's variable definitions).
char_labels <- c(
  sharpe_12m       = "Sharpe ratio (12m)",
  ni_me            = "Earnings yield (NI/ME)",
  roe_12m          = "Return on equity (12m)",
  gp_at_12m        = "Gross profitability (GP/AT)",
  div_me_med_36m   = "Dividend yield (36m median)",
  ret_vol_12m      = "Return volatility (12m)",
  log_mkt_cap      = "Log market cap (size)",
  qmj              = "Quality minus junk (QMJ)",
  roe_12m_gr3y     = "ROE (3y change)"
)

# The metric set travels with the pipeline, not with this script, so a renamed
# or retired feature shows up here as a bare "subscript out of bounds" from
# inside as.data.frame(). Check the names first and say which ones moved.
local({
  have <- colnames(pbr_list[[1]]@port_metrics_m_xts@data)
  need <- c(char_metrics, paste0("bench_", char_metrics))
  gone <- setdiff(need, have)
  if (length(gone) > 0L) {
    rlang::abort(paste0(
      "port_metrics_m_xts has no column(s): ", paste(gone, collapse = ", "),
      ". The run's metric set changed -- see compute_meta_port_metrics() in the ",
      "pipeline's R/helpers/backtest_meta_portfolios.R for what it now computes, ",
      "and update char_metrics/char_labels to match."
    ))
  }
})

# Portfolio-weighted characteristics, one row block per scheme.
char_port <- purrr::imap_dfr(pbr_list, function(pbr, nm) {
  x <- pbr@port_metrics_m_xts@data
  d <- as.data.frame(x[, char_metrics, drop = FALSE])
  d$date   <- zoo::index(x)
  d$series <- pretty_meta_label(nm)
  d
})

# Benchmark-weighted characteristics (same across schemes): take from the first.
xb <- pbr_list[[1]]@port_metrics_m_xts@data
char_bench <- as.data.frame(xb[, paste0("bench_", char_metrics), drop = FALSE])
colnames(char_bench) <- char_metrics
char_bench$date   <- zoo::index(xb)
char_bench$series <- "IBOV"

char_long <- dplyr::bind_rows(char_port, char_bench) %>%
  tidyr::pivot_longer(dplyr::all_of(char_metrics), names_to = "metric", values_to = "value")
char_long$metric <- factor(char_long$metric, levels = char_metrics)

# Series order/colours: 8 schemes get the categorical palette, IBOV is a
# distinct near-black dashed line so the benchmark stands out.
jp_char       <- factoRverse:::.journal_palette()
scheme_levels <- pretty_meta_label(names(pbr_list))
series_levels <- c(scheme_levels, "IBOV")
char_long$series <- factor(char_long$series, levels = series_levels)

char_cols <- setNames(rep(jp_char$categorical, length.out = length(scheme_levels)), scheme_levels)
char_cols["IBOV"] <- jp_char$text
char_ltys <- setNames(rep("solid", length(series_levels)), series_levels)
char_ltys["IBOV"] <- "dashed"

char_grid <- ggplot2::ggplot(char_long,
                             ggplot2::aes(x = date, y = value, color = series, linetype = series)) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::facet_wrap(~metric, ncol = 3, scales = "free_y",
                      labeller = ggplot2::labeller(metric = char_labels)) +
  ggplot2::scale_color_manual(values = char_cols, name = NULL) +
  ggplot2::scale_linetype_manual(values = char_ltys, name = NULL) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Average portfolio characteristics over time") +
  journal_area_theme()

ggplot2::ggsave(here::here("figures", "portfolio_characteristics.png"),
                char_grid, width = 14, height = 12, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the portfolio-characteristics figure ------------
portfolio_characteristics_figures_tex <- latex_figure(
  "portfolio_characteristics.png",
  "Average portfolio characteristics over time.",
  paste(
    "Weighted-average stock characteristics of each ML-ensemble portfolio",
    "(sum of end-of-period portfolio weights times the characteristic) at every",
    "rebalance date. Each panel is one characteristic: Sharpe ratio (12m),",
    "earnings yield (NI/ME), return on equity (12m), gross profitability",
    "(GP/AT), dividend yield (36m median), return volatility (12m), log market",
    "capitalization, quality minus junk (QMJ) and the three-year change in",
    "return on equity. Each line is one portfolio-construction scheme (RP, RP\\_BOX, HRP,",
    "CS, SW, EW, MVO, MVO\\_BOX); the dashed black line is the IBOV benchmark's",
    "weighted average. Vertical scales differ across panels."
  ),
  "fig:port_char"
)

writeLines(portfolio_characteristics_figures_tex,
           here::here("figures", "portfolio_characteristics_figure.tex"))

# ---- Long-only factor attribution (returns-based style regression) ----------
# We have no fund holdings, so instead of characteristic averages we regress the
# portfolio returns on long-only factor-mimicking portfolios. To keep the market
# loading interpretable and defeat the collinearity of long-only equity
# portfolios (all ~ market + tilt), each style factor is used market-neutralized
# (style return minus the benchmark), i.e. as an active/tilt return; MKT enters
# as the benchmark excess over CDI. This is a returns-based style attribution,
# not a Fama-French risk-premium test: loadings are style tilts and alpha is the
# active return unexplained by the market + style basis. `screen` must be
# loaded in the session (its $signal_port_cohorts holds the cap-scaled long-only
# signal portfolios).

# Daily CDI is compounded on the same 15th-to-15th endpoints as every return
# series in the attribution.
cdi <- tibble::tibble(ym = format(ml_index, "%Y-%m"), rf = cdi_ret)

# Long-only factor-mimicking portfolios (net returns, percent).
sp_returns_obj <- restore_meta_xts_dates(
  screen$signal_port_cohorts$signal_port_backtest_cohort_ind_signals@
    port_returns_m_xts_list$net_returns_m_xts
)
sp_xts   <- sp_returns_obj@data
sp_ym    <- format(as.Date(zoo::index(sp_xts)), "%Y-%m")

fcol <- function(sig) {
  hit <- grep(paste0("^c__cs_", sig, "_(long|short)_s__"), colnames(sp_xts), value = TRUE)
  stopifnot(length(hit) == 1L)
  hit
}

factor_mat <- tibble::tibble(
  ym  = sp_ym,
  mkt = as.numeric(sp_xts[, "selected_bench_return"]),
  hml = as.numeric(sp_xts[, fcol("be_me")]),              # value
  mom = as.numeric(sp_xts[, fcol("prc_ret_12m_1m")]),     # momentum
  rmw = as.numeric(sp_xts[, fcol("gp_at_12m")]),          # profitability
  smb = as.numeric(sp_xts[, fcol("log_mkt_cap")]),        # size (short large caps)
  # `assets_gr1y_tr` is not produced by the current gold features; `assets_gr1y`
  # is the surviving asset-growth signal. Matches build_gap_panel.R's CMA leg.
  cma = as.numeric(sp_xts[, fcol("assets_gr1y")])         # investment (short high growth)
) %>%
  dplyr::left_join(cdi, by = "ym") %>%
  dplyr::transmute(
    ym,
    rf,
    mkt_e = mkt - rf,     # market excess over CDI
    hml_n = hml - mkt,    # style active returns (market-neutralized)
    mom_n = mom - mkt,
    rmw_n = rmw - mkt,
    smb_n = smb - mkt,
    cma_n = cma - mkt
  ) %>%
  dplyr::arrange(ym)

ff_predictors <- c("mkt_e", "hml_n", "mom_n", "rmw_n", "smb_n", "cma_n")
ff_disp       <- c("alpha", "MKT", "HML", "MOM", "RMW", "SMB", "CMA")

# Regress one return series (percent) on the market-neutralized factor basis.
regress_flat <- function(ret, ret_ym) {
  d <- data.frame(ret = as.numeric(ret), ym = ret_ym, stringsAsFactors = FALSE) %>%
    dplyr::inner_join(factor_mat, by = "ym")
  d$y_e <- d$ret - d$rf
  d <- d[stats::complete.cases(d[, c("y_e", ff_predictors)]), ]
  if (nrow(d) < 12L) return(NULL)
  fit <- stats::lm(y_e ~ mkt_e + hml_n + mom_n + rmw_n + smb_n + cma_n, data = d)
  s  <- summary(fit)
  co <- stats::coef(fit)

  terms <- c("(Intercept)", "mkt_e", "hml_n", "mom_n", "rmw_n", "smb_n", "cma_n")
  est   <- co[terms]
  tval  <- s$coefficients[terms, "t value"]

  list(est  = stats::setNames(as.numeric(est), ff_disp),
       tval = stats::setNames(as.numeric(tval), ff_disp),
       r2   = s$r.squared, n = nrow(d))
}

# Per-fund regression, then median loading across funds (mirrors MED_FUND).
regress_median <- function(fund_names) {
  res <- lapply(fund_names, function(fn) regress_flat(fund_window_m_xts[, fn], test_ym))
  res <- res[!vapply(res, is.null, logical(1))]
  ests <- vapply(res, `[[`, numeric(length(ff_disp)), "est")
  tvs  <- vapply(res, `[[`, numeric(length(ff_disp)), "tval")
  list(est  = apply(ests, 1, stats::median, na.rm = TRUE),
       tval = apply(tvs,  1, stats::median, na.rm = TRUE),
       r2   = stats::median(vapply(res, `[[`, numeric(1), "r2"), na.rm = TRUE),
       n    = length(res))
}

# All test series share the ML index, so one year-month vector serves all.
test_ym <- format(ml_index, "%Y-%m")

ff_results <- list()
for (cn in colnames(ml_ports_net_m_xts)) {
  ff_results[[pretty_port_label(cn)]] <- regress_flat(ml_ports_net_m_xts[, cn], test_ym)
}
ff_results[["FUNDS_MEAN"]]   <- regress_flat(funds_bench_m_xts[, "funds_mean"], test_ym)
ff_results[["FUNDS_Q75"]]    <- regress_flat(funds_bench_m_xts[, "funds_q75"], test_ym)
ff_results[["MED_FUND"]]     <- regress_median(reliable_funds)
ff_results[["MED_FUND_Q75"]] <- regress_median(reliable_top_funds)

# Tidy frame for console inspection (estimates only).
factor_loadings <- purrr::imap_dfr(ff_results, function(r, nm) {
  if (is.null(r)) return(NULL)
  tibble::tibble(portfolio = nm, alpha = r$est[["alpha"]], MKT = r$est[["MKT"]],
                 HML = r$est[["HML"]], MOM = r$est[["MOM"]], RMW = r$est[["RMW"]],
                 SMB = r$est[["SMB"]], CMA = r$est[["CMA"]], R2 = r$r2)
})

# ---- LaTeX table: factor attribution ---------------------------------------
# Each portfolio spans two rows: estimates, then t-statistics in parentheses.
ff_est_row <- function(label, r) {
  e <- r$est
  sprintf("%s & %.3f & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f & %.2f \\\\",
          gsub("_", "\\\\_", label), e[["alpha"]], e[["MKT"]], e[["HML"]], e[["MOM"]],
          e[["RMW"]], e[["SMB"]], e[["CMA"]], r$r2)
}
ff_t_row <- function(r) {
  t <- r$tval
  sprintf(" & (%.2f) & (%.2f) & (%.2f) & (%.2f) & (%.2f) & (%.2f) & (%.2f) & \\\\",
          t[["alpha"]], t[["MKT"]], t[["HML"]], t[["MOM"]], t[["RMW"]], t[["SMB"]], t[["CMA"]])
}
ff_block <- function(label, display_label = label) {
  r <- ff_results[[label]]
  if (is.null(r)) {
    return(sprintf(
      "%s & \\multicolumn{8}{c}{--} \\\\",
      gsub("_", "\\\\_", display_label)
    ))
  }
  c(ff_est_row(display_label, r), ff_t_row(r))
}

ml_labels <- pretty_port_label(colnames(ml_ports_net_m_xts))

factor_table_tex <- c(
  "\\begin{table}[h]",
  "\\captionsetup{",
  "  justification=raggedright,",
  "  labelsep=none,",
  "  singlelinecheck=false,",
  "  labelformat=boldlabel,",
  "}",
  "\\renewcommand{\\arraystretch}{1.2}",
  paste0(
    "\\caption[Long-only factor attribution of portfolio returns]",
    "{\\footnotesize \\textbf{: Long-only factor ",
    "attribution of portfolio returns.} Each portfolio's monthly excess return over ",
    "the CDI is regressed on a market factor and five market-neutralized style ",
    "factors built from long-only cap-scaled signal portfolios: MKT is the benchmark ",
    "(IBOV) excess return over CDI, and HML (be\\_me), MOM (prc\\_ret\\_12m\\_1m), RMW ",
    "(gp\\_at\\_12m), SMB (log\\_mkt\\_cap) and CMA (assets\\_gr1y\\_tr) are each the ",
    "long-only signal portfolio minus the benchmark (an active/tilt return). This is ",
    "a returns-based style attribution, not a risk-premium test: loadings are style ",
    "tilts and $\\alpha$ (percent per month) is the active return unexplained by the ",
    "basis. Fund, IBOV and CDI daily returns are geometrically compounded from the ",
    "previous calendar 15th through the current calendar 15th, matching the ML and ",
    "factor-portfolio timing. MKT is therefore the contemporaneous market loading. ",
    "Estimates are shown with t-statistics in parentheses; $R^2$ is the regression ",
    "fit. MED and ",
    "MED\\_Q75 regress each fund individually and report the median loading ",
    "across (all / top-quartile) funds.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  paste0(
    "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{2.3cm}",
    "*{8}{>{\\raggedleft\\arraybackslash}X}}"
  ),
  "\\hline",
  "Portfolio & $\\alpha$ & MKT & HML & MOM & RMW & SMB & CMA & $R^2$ \\\\",
  "\\hline",
  tex_panel_header("ML Ensemble"),
  unlist(lapply(ml_labels, ff_block)),
  "\\hline",
  tex_panel_header("Benchmarks --- fund portfolios"),
  ff_block("FUNDS_MEAN"),
  ff_block("FUNDS_Q75"),
  tex_panel_header("Benchmarks --- representative single fund (median across funds)"),
  ff_block("MED_FUND", "MED"),
  ff_block("MED_FUND_Q75", "MED_Q75"),
  "\\hline",
  "\\end{tabularx}",
  "\\label{factorattr}",
  "\\end{table}"
)

writeLines(factor_table_tex, here::here("tables", "factor_attribution.tex"))

# ---- Global surrogate fit: how linear each ML model's predictions are -------
# final_gsm is the fitted OLS global surrogate (an `lm`) regressing each model's
# out-of-sample predictions on the [-1, 1] cross-sectionally scaled signals.
# Its R^2 is the share of prediction variance explained linearly. Only the final
# refit's surrogate is stored, so this is an end-of-sample snapshot per model
# rather than a time series.
jp_gsm <- factoRverse:::.journal_palette()

gsm_stats <- purrr::imap_dfr(sb_list, function(sbr, nm) {
  g <- sbr@final_gsm
  if (!inherits(g, "lm")) return(NULL)          # GSM must be the OLS (lm) surrogate
  tibble::tibble(
    model     = fi_model_label(nm),
    intercept = stats::coef(g)[["(Intercept)"]],  # ~ mean predicted active return
    r2        = summary(g)$r.squared
  )
})
gsm_stats$model <- factor(gsm_stats$model,
                          levels = intersect(fi_model_order, gsm_stats$model))

gsm_long <- gsm_stats %>%
  tidyr::pivot_longer(c(intercept, r2), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = factor(
    dplyr::recode(metric,
                  intercept = "Surrogate~intercept",
                  r2        = "Surrogate~R^2"),
    levels = c("Surrogate~intercept", "Surrogate~R^2")
  ))

gsm_plot <- ggplot2::ggplot(gsm_long, ggplot2::aes(x = model, y = value, fill = model)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::facet_wrap(
    ~metric,
    ncol = 2,
    scales = "free_y",
    labeller = ggplot2::label_parsed
  ) +
  ggplot2::scale_fill_manual(
    values = rep(jp_gsm$categorical, length.out = nlevels(gsm_stats$model)),
    guide = "none"
  ) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Linear explainability of each ML model's predictions (final refit)") +
  journal_area_theme()

ggplot2::ggsave(here::here("figures", "surrogate_fit.png"),
                gsm_plot, width = 12, height = 6, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the surrogate-fit figure ------------------------
surrogate_fit_figure_tex <- latex_figure(
  "surrogate_fit.png",
  "Linear explainability of each ML model's predictions.",
  paste(
    "For each ML model (OLS, GLMNET, RF, XGB, NN1--NN3), an OLS global surrogate",
    "regresses the model's predictions on the same signals, scaled",
    "cross-sectionally to $[-1,1]$. \\emph{Left:} the surrogate intercept is",
    "approximately the model's mean predicted active return because the scaled",
    "signals are approximately centred. It captures a common level shift across",
    "stocks and therefore does not affect their cross-sectional ranking.",
    "\\emph{Right:} surrogate $R^2$ is the share of prediction variance reproduced",
    "by a linear function of the signals. Values near one indicate an essentially",
    "linear prediction rule, whereas lower values indicate nonlinearities and",
    "interactions. The final-refit $R^2$ is 1.00 for OLS and GLMNET, 0.87 for",
    "NN1 and NN2, 0.71 for NN3, 0.59 for RF, and 0.49 for XGB. Both diagnostics",
    "use the final-refit surrogate from the individual-signals specification."
  ),
  "fig:surrogate_fit"
)

writeLines(surrogate_fit_figure_tex,
           here::here("figures", "surrogate_fit_figure.tex"))

# ---- Selected hyperparameters over time, across models ---------------------
# factoRverse's "Best Hyperparameters Over Time" plot pivots a model's
# best_hyperparameters_m_xts to long and facets by hyperparameter. Here we add a
# model dimension and facet by model x hyperparameter, so every tuned model's
# hyperparameter trajectory shows in one grid. Untuned models (OLS) carry no
# hyperparameters and drop out.
jp_hyper <- factoRverse:::.journal_palette()

hyper_all <- purrr::imap_dfr(sb_list, function(sbr, nm) {
  h <- sbr@best_hyperparameters_m_xts
  if (is.null(h)) return(NULL)
  h <- restore_meta_xts_dates(h)
  d <- tryCatch(as.data.frame(h@data), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0 || ncol(d) == 0) return(NULL)
  d$dates <- as.Date(rownames(d))
  d %>%
    tidyr::pivot_longer(-dates, names_to = "hyper", values_to = "value") %>%
    dplyr::mutate(model = fi_model_label(nm))
})
stopifnot(!all(is.na(hyper_all$dates)))   # rownames must parse as dates

# Drop hyperparameters we do not want to display (e.g. best_lam).
hyper_all <- hyper_all %>% dplyr::filter(!hyper %in% c("best_lam"))

# Group hyperparameters that play the same role across model families into one
# panel (a "concept"), drawn as separate coloured lines per model. Unmapped
# hyperparameters fall back to their own name, which also collapses the shared
# NN1/NN2/NN3 parameters into one panel each.
hyper_concept <- c(
  "num.trees" = "Number of trees", "num_trees" = "Number of trees", "nrounds" = "Number of trees",
  "max.depth" = "Max tree depth", "max_depth" = "Max tree depth",
  "min.bucket" = "Min leaf size", "min.node.size" = "Min leaf size", "min_child_weight" = "Min leaf size",
  "eta" = "Learning rate", "lr" = "Learning rate", "learning_rate" = "Learning rate",
  "best_iteration" = "Early-stop iteration", "best_iter" = "Early-stop iteration",
  "mtry" = "Feature subsampling", "colsample_bytree" = "Feature subsampling",
  "colsample.by.tree" = "Feature subsampling",
  "droprate" = "Dropout rate", "dropout" = "Dropout rate"
)
concept_order <- c("Number of trees", "Max tree depth", "Min leaf size",
                   "Feature subsampling", "Learning rate", "Early-stop iteration",
                   "Dropout rate")

# RF's mtry and XGB's colsample_bytree are both feature-subsampling fractions in
# [0, 1], so they share one panel directly (no rescaling needed).

hyper_all$panel <- ifelse(hyper_all$hyper %in% names(hyper_concept),
                          unname(hyper_concept[hyper_all$hyper]), hyper_all$hyper)
present_panels <- unique(hyper_all$panel)
panel_levels   <- c(intersect(concept_order, present_panels),
                    sort(setdiff(present_panels, concept_order)))
hyper_all$panel <- factor(hyper_all$panel, levels = panel_levels)

present_models  <- intersect(fi_model_order, unique(hyper_all$model))
hyper_all$model <- factor(hyper_all$model, levels = present_models)
hyper_cols <- setNames(rep(jp_hyper$categorical, length.out = length(present_models)),
                       present_models)

hyper_ncol <- 3L
hyper_plot <- ggplot2::ggplot(
  hyper_all, ggplot2::aes(x = dates, y = value, color = model, group = model)
) +
  ggplot2::geom_line(alpha = 0.7, linewidth = 0.4) +
  ggplot2::geom_point(size = 1.1) +
  ggplot2::facet_wrap(~panel, ncol = hyper_ncol, scales = "free_y") +
  ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
  ggplot2::scale_color_manual(values = hyper_cols, name = NULL) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Hyperparameter settings over time, by model") +
  journal_area_theme()

hyper_height <- max(6, ceiling(length(panel_levels) / hyper_ncol) * 3)
ggplot2::ggsave(here::here("figures", "hyperparameters_over_time.png"),
                hyper_plot, width = 16, height = hyper_height,
                dpi = 300, bg = "white", limitsize = FALSE)

# ---- Paste-ready LaTeX for the hyperparameter-over-time figure -------------
hyperparameters_figure_tex <- latex_figure(
  "hyperparameters_over_time.png",
  "Hyperparameter settings over time.",
  paste(
    "Recorded hyperparameter settings at each walk-forward refit for GLMNET, RF,",
    "XGB and NN1--NN3; OLS is untuned and omitted. The recorded objects contain both",
    "searched parameters and fixed training settings. Constant paths therefore",
    "document design choices rather than repeated validation selections.",
    "Parameters that play the same role across models share a panel and are",
    "drawn as separate coloured lines: number of trees (RF num.trees, XGB nrounds),",
    "maximum tree depth, minimum leaf size (RF min.bucket, XGB min\\_child\\_weight),",
    "learning rate (XGB eta, NN lr), early-stopping iteration and feature",
    "subsampling (RF mtry, XGB colsample\\_bytree; both fractions of the features",
    "sampled). Model-specific hyperparameters keep their own panels, and the three",
    "neural networks share each remaining NN panel. Points",
    "are the recorded values at each refit date; vertical scales differ across",
    "panels. Estimated on the individual-signals run."
  ),
  "fig:hyper_time"
)

writeLines(hyperparameters_figure_tex,
           here::here("figures", "hyperparameters_over_time_figure.tex"))

# ---- Cumulative net return: ML Ensemble vs human managers -------------------
# Wealth index (cumprod(1 + r/100)) over the ML backtest window for the 8 ML
# Ensemble schemes (net of transaction costs and the 1% p.a. fee), FUNDS_MEAN,
# FUNDS_Q75 and IBOV, plus a dashed dispersion band showing the 5th-95th
# percentile of eligible individual-fund wealth paths each period.
# cumprod() propagates NA forward once a fund's return series ends, so a fund
# naturally drops out of the band when it exits.
cum_wealth <- function(pct_ret) cumprod(1 + pct_ret / 100)

# --- Line series: ML schemes, FUNDS_MEAN/Q75, IBOV ---
ml_cum <- apply(zoo::coredata(ml_ports_net_m_xts), 2, cum_wealth)
colnames(ml_cum) <- pretty_port_label(colnames(ml_ports_net_m_xts))  # "c__meta_rp_ibov_s__..." -> "RP"

fund_bench_cum <- apply(zoo::coredata(funds_bench_m_xts), 2, cum_wealth)
colnames(fund_bench_cum) <- c("FUNDS_MEAN", "FUNDS_Q75")

bench_cum <- cum_wealth(as.numeric(zoo::coredata(ml_bench_m_xts)))

lines_wide <- cbind(ml_cum, fund_bench_cum, IBOV = bench_cum)
lines_df <- as.data.frame(lines_wide) %>%
  dplyr::mutate(date = ml_index) %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "wealth")

# --- Dispersion band: individual fund wealth paths, hold-from-start cohort ---
# Cohort = funds with a non-NA return in the window's first month; each fund's
# cumulative path (and hence its presence in the band) ends when it exits.
start_alive <- colnames(fund_ret_window)[!is.na(fund_ret_window[1, ])]
fund_cum <- apply(fund_ret_window[, start_alive, drop = FALSE], 2, cum_wealth)

band_from_cohort <- function(cum_mat, cohort, label) {
  m <- cum_mat[, cohort, drop = FALSE]
  qs <- t(apply(m, 1, stats::quantile, probs = c(0.05, 0.95), na.rm = TRUE))
  tibble::tibble(date = ml_index, cohort = label, lower = qs[, 1], upper = qs[, 2])
}

band_df <- band_from_cohort(fund_cum, start_alive, "Eligible funds (5th-95th pct.)")

# --- Colours: 8 ML schemes get the categorical palette; FUNDS_MEAN/Q75 and
# IBOV get distinct, thicker-lined roles so they read as reference series. IBOV
# is dotted (rather than dashed) so it is not confused with the band's dashed
# edges.
jp_cum <- factoRverse:::.journal_palette()
ml_scheme_levels <- pretty_meta_label(names(pbr_list))
series_levels <- c(ml_scheme_levels, "FUNDS_MEAN", "FUNDS_Q75", "IBOV")
lines_df$series <- factor(lines_df$series, levels = series_levels)

line_cols <- c(
  setNames(rep(jp_cum$categorical, length.out = length(ml_scheme_levels)), ml_scheme_levels),
  FUNDS_MEAN = jp_cum$negative,
  FUNDS_Q75  = jp_cum$positive,
  IBOV       = jp_cum$text
)
line_widths <- setNames(c(rep(0.5, length(ml_scheme_levels)), 1.1, 1.1, 1.0), series_levels)
line_types  <- setNames(c(rep("solid", length(ml_scheme_levels)), "solid", "solid", "dotted"),
                        series_levels)

band_cols <- c("Eligible funds (5th-95th pct.)" = jp_cum$neutral)

cumret_plot <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = band_df,
                       ggplot2::aes(x = date, ymin = lower, ymax = upper, fill = cohort),
                       alpha = 0.12, color = NA) +
  ggplot2::geom_line(data = band_df,
                     ggplot2::aes(x = date, y = lower, color = cohort),
                     linetype = "dashed", linewidth = 0.35, alpha = 0.7) +
  ggplot2::geom_line(data = band_df,
                     ggplot2::aes(x = date, y = upper, color = cohort),
                     linetype = "dashed", linewidth = 0.35, alpha = 0.7) +
  ggplot2::geom_line(data = lines_df,
                     ggplot2::aes(x = date, y = wealth, color = series,
                                  linewidth = series, linetype = series)) +
  ggplot2::scale_color_manual(values = c(line_cols, band_cols), name = NULL,
                              breaks = c(series_levels, names(band_cols))) +
  ggplot2::scale_fill_manual(values = band_cols, name = NULL, guide = "none") +
  ggplot2::scale_linewidth_manual(values = line_widths, guide = "none") +
  ggplot2::scale_linetype_manual(values = line_types, guide = "none") +
  ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
  ggplot2::labs(x = NULL, y = "Cumulative wealth (Dec-2015 = start)",
                title = "Cumulative net return: ML Ensemble vs. human managers") +
  journal_area_theme() +
  ggplot2::theme(legend.position = "right")

ggplot2::ggsave(here::here("figures", "cumulative_return.png"),
                cumret_plot, width = 14, height = 8, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the cumulative-return figure --------------------
cumulative_return_figure_tex <- latex_figure(
  "cumulative_return.png",
  "Cumulative net return: ML Ensemble vs. human managers.",
  paste(
    "Cumulative wealth index (compounded net monthly returns) over the",
    "out-of-sample window for the eight ML Ensemble portfolio-construction",
    "schemes (net of transaction costs and a 1\\% p.a. management fee),",
    "FUNDS\\_MEAN, FUNDS\\_Q75 and IBOV (dotted, to distinguish it from the dashed",
    "band). The dashed band shows the 5th--95th percentile of individual fund",
    "wealth paths each month across eligible funds, illustrating the dispersion of",
    "outcomes underlying the FUNDS\\_MEAN average. The cohort holds funds alive at",
    "the start of the window; a fund leaves the cohort (and the band) once it",
    "stops reporting returns."
  ),
  "fig:cumret"
)

writeLines(cumulative_return_figure_tex,
           here::here("figures", "cumulative_return_figure.tex"))

# ---- Correlograms: agreement across models and across portfolios -----------
# Setup for the man-and-machine aggregation question: combining two strategies
# only helps if their returns are imperfectly correlated, so we look at (i) how
# much the base learners' predictions agree and (ii) how correlated the ML
# Ensemble portfolios are with the human-manager benchmarks.
jp_cor <- factoRverse:::.journal_palette()

cor_long <- function(cor_mat, panel_label) {
  lv <- colnames(cor_mat)
  d  <- as.data.frame(as.table(as.matrix(cor_mat)), stringsAsFactors = FALSE)
  colnames(d) <- c("row", "col", "corr")
  d$row   <- factor(d$row, levels = rev(lv))
  d$col   <- factor(d$col, levels = lv)
  d$panel <- panel_label
  d
}

correlogram_plot <- function(long_df, title, ncol = 1L, text_size = 4.4) {
  ggplot2::ggplot(long_df, ggplot2::aes(x = col, y = row, fill = corr)) +
    ggplot2::geom_tile(color = jp_cor$background, linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", corr)),
                       size = text_size, color = jp_cor$text) +
    ggplot2::facet_wrap(~panel, ncol = ncol) +
    ggplot2::scale_fill_gradient2(low = jp_cor$negative, mid = "#EBEEF1",
                                  high = jp_cor$primary, midpoint = 0,
                                  limits = c(-1, 1), name = "Correlation") +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        direction = "vertical",
        barheight = grid::unit(60, "mm"),
        barwidth = grid::unit(7, "mm")
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    journal_area_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# --- (i) Base-learner prediction agreement ---
preds_df <- mb_grid@
  base_learners_oos_predictions_m_df@data
pred_cols <- setdiff(colnames(preds_df), c("id", "tickers", "dates", "target", "error"))
pred_mat  <- as.matrix(preds_df[, pred_cols, drop = FALSE])
colnames(pred_mat) <- fi_model_label(pred_cols)
pred_mat  <- pred_mat[, intersect(fi_model_order, colnames(pred_mat)), drop = FALSE]

# Average of the per-date CROSS-SECTIONAL correlation matrices: this asks
# whether the models rank the same stocks alike, without the common time-series
# variation in prediction levels that a pooled correlation would absorb.
per_date_cor <- lapply(split(seq_len(nrow(pred_mat)), preds_df$dates), function(i) {
  if (length(i) < 3L) return(NULL)
  suppressWarnings(stats::cor(pred_mat[i, , drop = FALSE], use = "pairwise.complete.obs"))
})
per_date_cor <- Filter(Negate(is.null), per_date_cor)
stopifnot(length(per_date_cor) > 0L)
pred_cor <- apply(simplify2array(per_date_cor), 1:2, mean, na.rm = TRUE)

pred_cor_plot <- correlogram_plot(
  cor_long(pred_cor, "Mean cross-sectional correlation of predictions"),
  title = "Agreement across base-learner predictions"
)
ggplot2::ggsave(here::here("figures", "correlogram_predictions.png"),
                pred_cor_plot, width = 10, height = 8.5, dpi = 300, bg = "white")

# --- (ii) Portfolio return agreement, ML Ensemble vs human managers ---
perf_mat <- cbind(zoo::coredata(ml_ports_net_m_xts), zoo::coredata(funds_bench_m_xts))
colnames(perf_mat) <- c(pretty_port_label(colnames(ml_ports_net_m_xts)),
                        "FUNDS_MEAN", "FUNDS_Q75")
fund_mat  <- fund_ret_window[, reliable_funds, drop = FALSE]
bench_vec <- as.numeric(zoo::coredata(ml_bench_m_xts))

# AVG_FUND and AVG_FUND_Q75 are correlation summaries, not return series.
# Each cell averages the focal series' correlations with the corresponding
# individual-fund cohort. Diagonal cells average distinct fund pairs.
perf_cor_with_fund_averages <- function(series_mat, funds_mat, q75_funds_mat) {
  pairwise_cor <- function(x) {
    cm <- stats::cor(x, use = "pairwise.complete.obs")
    mean(cm[upper.tri(cm)], na.rm = TRUE)
  }
  cross_cohort_cor <- function(x, y) {
    cm <- stats::cor(x, y, use = "pairwise.complete.obs")
    same_fund <- outer(colnames(x), colnames(y), FUN = "==")
    mean(cm[!same_fund], na.rm = TRUE)
  }

  base <- stats::cor(series_mat, use = "pairwise.complete.obs")
  avg_all <- rowMeans(
    stats::cor(series_mat, funds_mat, use = "pairwise.complete.obs"),
    na.rm = TRUE
  )
  avg_q75 <- rowMeans(
    stats::cor(series_mat, q75_funds_mat, use = "pairwise.complete.obs"),
    na.rm = TRUE
  )
  summary_cross <- cbind(AVG_FUND = avg_all, AVG_FUND_Q75 = avg_q75)
  summary_block <- matrix(
    c(
      pairwise_cor(funds_mat),
      cross_cohort_cor(funds_mat, q75_funds_mat),
      cross_cohort_cor(funds_mat, q75_funds_mat),
      pairwise_cor(q75_funds_mat)
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(colnames(summary_cross), colnames(summary_cross))
  )

  rbind(cbind(base, summary_cross), cbind(t(summary_cross), summary_block))
}

# Active returns strip the common market exposure that otherwise pushes every
# long-only pair toward 1. ML and fund returns now share 15th-to-15th timing.
bench_fund_vec <- funds_window$ibov
n_ml_cols <- ncol(ml_ports_net_m_xts)

perf_active <- perf_mat
perf_active[,  seq_len(n_ml_cols)] <-
  sweep(perf_mat[,  seq_len(n_ml_cols), drop = FALSE], 1, bench_vec, "-")
perf_active[, -seq_len(n_ml_cols)] <-
  sweep(perf_mat[, -seq_len(n_ml_cols), drop = FALSE], 1, bench_fund_vec, "-")
fund_active <- sweep(fund_mat, 1, bench_fund_vec, "-")
q75_fund_active <- fund_active[, reliable_top_funds, drop = FALSE]

act_cor <- perf_cor_with_fund_averages(
  perf_active,
  fund_active,
  q75_fund_active
)

perf_cor_long <- cor_long(act_cor, "Active returns (vs IBOV)")

perf_cor_plot <- correlogram_plot(
  perf_cor_long,
  title = "Active return correlation: ML Ensemble portfolios vs human managers",
  text_size = 4.5
)
ggplot2::ggsave(here::here("figures", "correlogram_performance.png"),
                perf_cor_plot, width = 12, height = 10.5, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the correlogram figures -------------------------
correlogram_figures_tex <- c(
  latex_figure(
    "correlogram_predictions.png",
    "Agreement across base-learner predictions.",
    paste(
      "Pairwise correlation of the out-of-sample return predictions of the seven",
      "base learners (OLS, GLMNET, RF, XGB, NN1--NN3). Correlations are computed",
      "cross-sectionally within each month and then averaged over months, so they",
      "measure whether the models rank the same stocks alike rather than sharing a",
      "common time-series level. Mean correlation across all 21 model pairs is",
      "0.81. The RF--XGB correlation is 0.82, while mean within-family",
      "correlation is 0.93 among NN1--NN3 and 0.81 between OLS and GLMNET. The",
      "lowest pair is OLS--XGB at 0.66. High within-family correlations indicate",
      "that architecture variants often provide less forecast diversification",
      "than their different labels suggest."
    ),
    "fig:corr_pred"
  ),
  "",
  latex_figure(
    "correlogram_performance.png",
    "Active return correlation: ML Ensemble portfolios vs human managers.",
    paste(
      "Monthly active return correlations (each series measured net of IBOV) among",
      "the eight ML Ensemble portfolio-construction schemes and the human-manager",
      "benchmarks FUNDS\\_MEAN and FUNDS\\_Q75. Active rather than raw returns are",
      "shown because the common market exposure of long-only portfolios",
      "mechanically pushes every raw-return pair toward one. All fund and IBOV",
      "returns are geometrically compounded 15th-to-15th to match the ML backtest.",
      "AVG\\_FUND and AVG\\_FUND\\_Q75 are not return series: their entries average",
      "each portfolio's correlations with, respectively, all reliable funds and",
      "reliable top-quartile funds. Their diagonal entries average distinct",
      "within-cohort fund pairs and therefore are not equal to one. Lower",
      "ML-versus-fund correlations indicate greater diversification potential from",
      "combining machine and human portfolios."
    ),
    "fig:corr_perf"
  )
)

writeLines(correlogram_figures_tex,
           here::here("figures", "correlogram_figures.tex"))

# ---- Man AND machine: combining ML portfolios with human managers -----------
# A two-asset combination (w in the ML portfolio, 1 - w in the human manager)
# has a ONE-dimensional weight space, so the frontier is traced with a
# deterministic grid rather than random draws: the grid is reproducible, hits
# the endpoints exactly and needs no Monte Carlo error. Portfolios are
# rebalanced monthly to constant weights.
#
# Note on the single-fund case: averaging the combined RETURNS across funds is
# degenerate, since mean_f[w*r_ML + (1-w)*r_f] = w*r_ML + (1-w)*mean_f(r_f),
# i.e. exactly the FUNDS_MEAN combination. The informative question -- what an
# investor who pairs the machine with ONE fund gets -- requires computing the
# metrics per fund and then aggregating those. Following the MED_FUND
# convention, we report the MEDIAN across funds (means of ratios are dominated
# by funds with near-zero denominators).
w_grid <- seq(0, 1, by = 0.05)

rf_vec <- cdi_ret

# Annualized metrics computed column-wise, so a whole fund panel is evaluated in
# one pass: Sharpe on returns in excess of the CDI, and active return / active
# risk (tracking error) / Information Ratio against IBOV. Active return and risk
# are annualized so that IR is exactly their ratio, which makes IR the slope of
# a ray from the origin on the active risk-return frontier.
# `combo_total` drives the Sharpe (excess over CDI); `combo_active` is supplied
# separately because it already contains each leg's return net of aligned IBOV.
# Since the weights sum to one, the combination's active return is the weighted
# average of the component active returns.
combo_metrics <- function(combo_total, combo_active, rf) {
  xe   <- sweep(as.matrix(combo_total), 1, rf, "-")   # excess over CDI
  xa   <- as.matrix(combo_active)                     # active vs own benchmark
  sd_e <- apply(xe, 2, stats::sd, na.rm = TRUE)
  act_ret  <- colMeans(xa, na.rm = TRUE) * 12
  act_risk <- apply(xa, 2, stats::sd, na.rm = TRUE) * sqrt(12)
  med_ret  <- stats::median(act_ret, na.rm = TRUE)
  med_risk <- stats::median(act_risk, na.rm = TRUE)
  # median() over a single-column case returns that column's own value.
  tibble::tibble(
    sharpe      = stats::median(colMeans(xe, na.rm = TRUE) / sd_e * sqrt(12), na.rm = TRUE),
    ir          = stats::median(act_ret / act_risk, na.rm = TRUE),
    active_ret  = med_ret,
    active_risk = med_risk,
    # Slope of the plotted coordinate. Equals `ir` only when a single manager
    # series is supplied; across funds median(a/b) != median(a)/median(b), so the
    # frontier figure must use this one to stay internally consistent.
    ir_plot     = med_ret / med_risk
  )
}

ml_ret_mat <- zoo::coredata(ml_ports_net_m_xts)
colnames(ml_ret_mat) <- pretty_port_label(colnames(ml_ports_net_m_xts))

# Blend one ML series into a panel of manager series at weight w. When a manager
# is not reporting -- the fund has closed, or has not launched yet -- the
# investor cannot hold it, so the capital is reallocated to the ML portfolio and
# the month is held at 100% ML rather than dropped. This keeps every combination
# a continuously invested path over the full window, and makes the w = 1
# endpoint exactly the standalone ML portfolio for every manager.
blend <- function(ml_vec, human_mat, w) {
  ml_rep <- matrix(ml_vec, nrow = length(ml_vec), ncol = ncol(human_mat),
                   dimnames = dimnames(human_mat))
  out <- w * ml_rep + (1 - w) * human_mat
  absent <- is.na(human_mat)
  out[absent] <- ml_rep[absent]
  out
}

# Four manager archetypes, forming a 2x2: diversified across funds (rows 1) vs
# picking a single fund (row 2), crossed with the average manager vs the
# top-quartile manager. The single top-quartile fund represents the investor who
# is an exceptional fund picker.
stopifnot(length(reliable_top_funds) > 0L)

human_sets <- list(
  "FUNDS_MEAN"    = perf_mat[, "FUNDS_MEAN", drop = FALSE],
  "FUNDS_Q75"     = perf_mat[, "FUNDS_Q75",  drop = FALSE],
  "MED_FUND"      = fund_mat,
  "MED_FUND_Q75"  = fund_mat[, reliable_top_funds, drop = FALSE]
)

# Active returns of each leg against 15th-to-15th IBOV.
ml_active_mat     <- sweep(ml_ret_mat, 1, bench_vec, "-")
human_active_sets <- lapply(human_sets, function(m) sweep(as.matrix(m), 1, bench_fund_vec, "-"))

combo_df <- purrr::map_dfr(colnames(ml_ret_mat), function(s) {
  purrr::map_dfr(names(human_sets), function(hn) {
    hm  <- human_sets[[hn]]
    hma <- human_active_sets[[hn]]
    purrr::map_dfr(w_grid, function(w) {
      combo_total  <- blend(ml_ret_mat[, s],    hm,  w)
      combo_active <- blend(ml_active_mat[, s], hma, w)
      tibble::tibble(scheme = s, human = hn, w = w) %>%
        dplyr::bind_cols(combo_metrics(combo_total, combo_active, rf_vec))
    })
  })
})

combo_df$scheme <- factor(combo_df$scheme, levels = colnames(ml_ret_mat))
combo_df$human  <- factor(combo_df$human, levels = names(human_sets))

# Optimum per (scheme, human) for each metric, plus the pure-component values.
combo_summary <- combo_df %>%
  dplyr::group_by(scheme, human) %>%
  dplyr::summarise(
    sharpe_human = sharpe[which.min(abs(w - 0))],
    sharpe_ml    = sharpe[which.min(abs(w - 1))],
    sharpe_best  = max(sharpe, na.rm = TRUE),
    w_sharpe     = w[which.max(sharpe)],
    ir_human     = ir[which.min(abs(w - 0))],
    ir_ml        = ir[which.min(abs(w - 1))],
    ir_best      = max(ir, na.rm = TRUE),
    w_ir         = w[which.max(ir)],
    .groups      = "drop"
  )

jp_comb <- factoRverse:::.journal_palette()

# Active risk-return frontier. Each curve traces one (ML scheme, manager) pair
# as w runs from 0 (pure manager) to 1 (pure ML); because active return and
# active risk are annualized consistently, the Information Ratio is the slope of
# a ray from the origin, so a curve bulging up and to the left is a genuine
# diversification gain. Endpoints and the maximum-IR point are marked.
near0 <- function(x) abs(x) < 1e-9

pure_ml_pts    <- combo_df %>% dplyr::filter(near0(w - 1))
# The w = 0 point is NOT scheme-independent: months in which a fund does not
# report are held in that scheme's ML portfolio, so each scheme has its own
# pure-manager endpoint. Keep `scheme` in the key rather than collapsing.
pure_human_pts <- combo_df %>% dplyr::filter(near0(w)) %>%
  dplyr::distinct(human, scheme, active_risk, active_ret)
max_ir_pts <- combo_df %>%
  dplyr::group_by(scheme, human) %>%
  dplyr::slice_max(ir_plot, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

scheme_cols <- setNames(
  rep(jp_comb$categorical, length.out = nlevels(combo_df$scheme)),
  levels(combo_df$scheme)
)

combo_plot <- ggplot2::ggplot(
  combo_df, ggplot2::aes(x = active_risk, y = active_ret, color = scheme)
) +
  ggplot2::geom_path(linewidth = 0.5, alpha = 0.9) +
  ggplot2::geom_point(data = max_ir_pts, shape = 18, size = 2.6) +
  ggplot2::geom_point(data = pure_ml_pts, shape = 17, size = 2.2) +
  ggplot2::geom_point(data = pure_human_pts, shape = 15, size = 3.2) +
  ggplot2::facet_wrap(~human, ncol = 2) +   # 2x2: diversified vs single pick, average vs top quartile
  ggplot2::scale_color_manual(values = scheme_cols, name = NULL) +
  ggplot2::expand_limits(x = 0, y = 0) +   # keeps IR readable as slope from origin
  ggplot2::labs(
    x = "Annualized active risk (tracking error, %)",
    y = "Annualized active return (%)",
    title = "Man and machine: active risk-return frontier",
    subtitle = paste0(
      "w = 0: pure manager (square); w = 1: pure ML (triangle)\n",
      "Diamonds mark the maximum Information Ratio"
    )
  ) +
  journal_area_theme()

ggplot2::ggsave(here::here("figures", "combination_frontier.png"),
                combo_plot, width = 13, height = 11, dpi = 300, bg = "white")

# ---- Table 6: combination frontier optima ----------------------------------
combo_table_human <- dplyr::recode(
  as.character(combo_summary$human),
  MED_FUND = "MED",
  MED_FUND_Q75 = "MED_Q75"
)

combo_table_body <- sprintf(
  "%s & %s & %.2f & %.2f & %.2f & %.0f\\%% & %.2f & %.2f & %.2f & %.0f\\%% \\\\",
  gsub("_", "\\\\_", as.character(combo_summary$scheme)),
  gsub("_", "\\\\_", combo_table_human),
  combo_summary$sharpe_human, combo_summary$sharpe_ml, combo_summary$sharpe_best,
  combo_summary$w_sharpe * 100,
  combo_summary$ir_human, combo_summary$ir_ml, combo_summary$ir_best,
  combo_summary$w_ir * 100
)

combo_table_tex <- c(
  "\\begin{table}[h]",
  "\\captionsetup{",
  "  justification=raggedright,",
  "  labelsep=none,",
  "  singlelinecheck=false,",
  "  labelformat=boldlabel,",
  "}",
  "\\renewcommand{\\arraystretch}{1.05}",
  paste0(
    "\\caption[Man-and-machine combination frontier]",
    "{\\footnotesize \\textbf{: Man-and-machine ",
    "combination frontier.} Each row combines one ML Ensemble portfolio with one ",
    "human-manager benchmark over $w \\in \\{0, 0.05, \\ldots, 1\\}$. Annualized ",
    "Sharpe ratios use the CDI and Information Ratios use IBOV; all returns are ",
    "measured 15th-to-15th. Information Ratios are the annualized arithmetic mean ",
    "active return divided by the tracking error, so they are not directly ",
    "comparable with the geometric convention of Table~\\ref{actret}. ",
    "Human and ML denote $w=0$ and $w=1$; Best and $w^{*}$ ",
    "report the grid optimum. FUNDS\\_MEAN and FUNDS\\_Q75 are diversified eligible-fund ",
    "portfolios. MED and MED\\_Q75 report medians across eligible individual-fund ",
    "combinations. Missing fund periods are allocated entirely to the ML leg.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  paste0(
    "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{2.8cm}",
    ">{\\raggedright\\arraybackslash}m{2.2cm}*{8}{>{\\raggedleft\\arraybackslash}X}}"
  ),
  "\\hline",
  paste("& & \\multicolumn{4}{c}{Sharpe ratio} &",
        "\\multicolumn{4}{c}{Information ratio} \\\\"),
  paste("Portfolio & Manager & Hum. & ML & Best & $w^{*}$ &",
        "Hum. & ML & Best & $w^{*}$ \\\\"),
  "\\hline",
  combo_table_body,
  "\\hline",
  "\\end{tabularx}",
  "\\label{combofront}",
  "\\end{table}"
)

writeLines(combo_table_tex, here::here("tables", "combination_frontier.tex"))

# ---- Paste-ready LaTeX for the combination-frontier figure -----------------
combination_figure_tex <- latex_figure(
  "combination_frontier.png",
  "Man and machine: active risk-return frontier.",
  paste(
    "Annualized active return and risk for monthly rebalanced combinations holding",
    "$w$ in an ML Ensemble portfolio and $1-w$ in a human-manager benchmark.",
    "FUNDS\\_MEAN and FUNDS\\_Q75 are diversified fund portfolios; MED\\_FUND and",
    "MED\\_FUND\\_Q75 report the median result across individual-fund combinations.",
    "In the two diversified panels the Information Ratio is the slope of a ray from",
    "the origin. In the two MED panels the coordinates are cross-fund medians of the",
    "components, so the slope is the median-component ratio and not the median of",
    "the fund-level Information Ratios reported in Table~\\ref{combofront}.",
    "All returns are measured 15th-to-15th against IBOV. Squares, triangles and",
    "diamonds mark the pure manager, pure ML portfolio and maximum-IR combination.",
    "In the MED panels the $w=0$ square differs by ML scheme because months in which",
    "a fund does not report are held in that scheme's ML portfolio."
  ),
  "fig:combo_frontier"
)

writeLines(combination_figure_tex,
           here::here("figures", "combination_frontier_figure.tex"))

# ---- Fund-picking robustness ------------------------------------------------
# The primary specification is fixed before examining these robustness results:
# RP is the paper's most transparent risk-controlled ML construction. Other ML
# schemes remain visible in the oracle-quantile figure as descriptive checks.
primary_ml_scheme <- "RP"
if (!primary_ml_scheme %in% colnames(ml_ret_mat)) {
  rlang::abort("The pre-specified RP portfolio is missing from the ML return matrix.")
}
primary_ml_ret <- ml_ret_mat[, primary_ml_scheme]

annualized_ir <- function(returns_pct, benchmark_pct) {
  active <- returns_pct - benchmark_pct
  active <- active[is.finite(active)]
  if (length(active) < 3L) return(NA_real_)
  active_sd <- stats::sd(active)
  if (!is.finite(active_sd) || active_sd <= 0) return(NA_real_)
  mean(active) / active_sd * sqrt(12)
}

fund_portfolio_return <- function(fund_panel, fund_names) {
  if (length(fund_names) == 0L) {
    rlang::abort("A fund portfolio cannot be formed from an empty selection.")
  }
  out <- rowMeans(fund_panel[, fund_names, drop = FALSE], na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}

rank_funds <- function(fund_panel) {
  scores <- colMeans(fund_panel, na.rm = TRUE)
  valid <- is.finite(scores)
  if (!any(valid)) rlang::abort("No funds have finite returns in the ranking window.")
  names(scores)[valid][order(-scores[valid], names(scores)[valid])]
}

combo_ir_result <- function(ml_returns, human_returns, benchmark, weights = w_grid) {
  if (length(ml_returns) != length(human_returns) ||
      length(ml_returns) != length(benchmark)) {
    rlang::abort("ML, human, and benchmark returns must have identical lengths.")
  }
  ir_path <- vapply(weights, function(w) {
    combined <- w * ml_returns + (1 - w) * human_returns
    combined[!is.finite(human_returns)] <- ml_returns[!is.finite(human_returns)]
    annualized_ir(combined, benchmark)
  }, numeric(1))
  if (all(!is.finite(ir_path))) {
    rlang::abort("No finite Information Ratio was obtained on the weight grid.")
  }
  best_index <- which.max(ir_path)
  tibble::tibble(
    ir_human = ir_path[[which.min(abs(weights))]],
    ir_ml = ir_path[[which.min(abs(weights - 1))]],
    ir_best = ir_path[[best_index]],
    delta_ir = ir_path[[best_index]] -
      max(ir_path[[which.min(abs(weights))]], ir_path[[which.min(abs(weights - 1))]]),
    w_ir = weights[[best_index]]
  )
}

circular_block_indices <- function(n, block_length) {
  if (n < 2L || block_length < 1L || block_length > n) {
    rlang::abort("Invalid sample or block length for the circular bootstrap.")
  }
  starts <- sample.int(n, ceiling(n / block_length), replace = TRUE)
  indices <- unlist(lapply(starts, function(s) {
    ((s - 1L + seq_len(block_length) - 1L) %% n) + 1L
  }), use.names = FALSE)
  indices[seq_len(n)]
}

# Oracle sensitivity: rank over the full evaluation window, then vary the
# retained top share. This is deliberately a performance bound, not a strategy.
oracle_top_shares <- c(0.50, 0.35, 0.25, 0.15, 0.10, 0.05)
oracle_ranked_funds <- rank_funds(fund_ret_window)

oracle_human_returns <- setNames(lapply(oracle_top_shares, function(top_share) {
  n_selected <- max(1L, ceiling(top_share * length(oracle_ranked_funds)))
  fund_portfolio_return(
    fund_ret_window,
    oracle_ranked_funds[seq_len(n_selected)]
  )
}), as.character(oracle_top_shares))

oracle_quantile_df <- purrr::map_dfr(colnames(ml_ret_mat), function(scheme) {
  purrr::map_dfr(oracle_top_shares, function(top_share) {
    human_returns <- oracle_human_returns[[as.character(top_share)]]
    dplyr::bind_cols(
      tibble::tibble(
        scheme = scheme,
        top_share = top_share,
        n_funds = max(1L, ceiling(top_share * length(oracle_ranked_funds)))
      ),
      combo_ir_result(ml_ret_mat[, scheme], human_returns, bench_fund_vec)
    )
  })
})

# Jointly resample months, re-rank funds inside each sample, and preserve all
# cross-series dependence. The interval therefore includes ranking uncertainty.
oracle_bootstrap_reps <- 999L
oracle_block_length <- 12L
set.seed(20260727)
oracle_bootstrap <- purrr::map_dfr(seq_len(oracle_bootstrap_reps), function(draw) {
  idx <- circular_block_indices(nrow(fund_ret_window), oracle_block_length)
  funds_b <- fund_ret_window[idx, , drop = FALSE]
  ranked_b <- rank_funds(funds_b)
  purrr::map_dfr(oracle_top_shares, function(top_share) {
    n_selected <- max(1L, ceiling(top_share * length(ranked_b)))
    human_b <- fund_portfolio_return(funds_b, ranked_b[seq_len(n_selected)])
    dplyr::bind_cols(
      tibble::tibble(draw = draw, top_share = top_share),
      combo_ir_result(
        primary_ml_ret[idx],
        human_b,
        bench_fund_vec[idx]
      )
    )
  })
})

oracle_bootstrap_summary <- oracle_bootstrap %>%
  dplyr::group_by(top_share) %>%
  dplyr::summarise(
    best_lo = stats::quantile(ir_best, 0.025, na.rm = TRUE),
    best_hi = stats::quantile(ir_best, 0.975, na.rm = TRUE),
    delta_lo = stats::quantile(delta_ir, 0.025, na.rm = TRUE),
    delta_hi = stats::quantile(delta_ir, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

oracle_plot_df <- oracle_quantile_df %>%
  tidyr::pivot_longer(
    cols = c(ir_human, ir_best),
    names_to = "portfolio",
    values_to = "ir"
  ) %>%
  dplyr::mutate(
    scheme = factor(scheme, levels = colnames(ml_ret_mat)),
    portfolio = factor(
      portfolio,
      levels = c("ir_human", "ir_best"),
      labels = c("Human only", "Best combined")
    )
  )

# The ribbon layer carries its own copy of `scheme`. It must be the SAME factor
# as in oracle_plot_df: if one layer supplies a character and another a factor,
# facet_wrap() falls back to alphabetical order and RP stops leading the grid.
oracle_ribbon <- oracle_quantile_df %>%
  dplyr::filter(scheme == primary_ml_scheme) %>%
  dplyr::left_join(oracle_bootstrap_summary, by = "top_share") %>%
  dplyr::mutate(scheme = factor(scheme, levels = colnames(ml_ret_mat)))

jp_picker <- factoRverse:::.journal_palette()
picker_cols <- c(
  "Human only" = jp_picker$categorical[[2]],
  "Best combined" = jp_picker$primary
)

oracle_quantile_plot <- ggplot2::ggplot(
  oracle_plot_df,
  ggplot2::aes(x = top_share * 100, y = ir, color = portfolio)
) +
  ggplot2::geom_ribbon(
    data = oracle_ribbon,
    ggplot2::aes(x = top_share * 100, ymin = best_lo, ymax = best_hi),
    inherit.aes = FALSE,
    fill = jp_picker$primary,
    alpha = 0.16
  ) +
  ggplot2::geom_vline(xintercept = 25, linetype = "dotted", color = jp_picker$text) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::facet_wrap(~scheme, ncol = 4) +
  ggplot2::scale_color_manual(values = picker_cols, name = NULL) +
  ggplot2::scale_x_continuous(breaks = oracle_top_shares * 100) +
  ggplot2::labs(
    x = "Ex-post top share retained (%)",
    y = "Information Ratio",
    title = "Fund-picking sensitivity",
    subtitle = "Dotted line: current top-quartile benchmark; shaded RP band: 95% block-bootstrap interval"
  ) +
  journal_area_theme() +
  ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 18))

ggplot2::ggsave(
  here::here("figures", "fund_picker_quantile_sensitivity.png"),
  oracle_quantile_plot,
  width = 15,
  height = 11,
  dpi = 300,
  bg = "white"
)

# Fixed-basket experiment within the oracle top quartile. Bands describe the
# dispersion across possible baskets, not time-series sampling uncertainty.
basket_sizes <- unique(c(1L, 2L, 3L, 5L, 10L, 20L, 50L, 100L,
                         length(reliable_top_funds)))
basket_draws_per_size <- 1000L
set.seed(20260728)
basket_draws <- purrr::map_dfr(basket_sizes, function(k) {
  draws <- if (k == length(reliable_top_funds)) 1L else basket_draws_per_size
  purrr::map_dfr(seq_len(draws), function(draw) {
    selected <- if (k == length(reliable_top_funds)) {
      reliable_top_funds
    } else {
      sample(reliable_top_funds, k, replace = FALSE)
    }
    human_returns <- fund_portfolio_return(fund_ret_window, selected)
    dplyr::bind_cols(
      tibble::tibble(basket_size = k, draw = draw),
      combo_ir_result(primary_ml_ret, human_returns, bench_fund_vec)
    )
  })
})

basket_summary <- basket_draws %>%
  dplyr::group_by(basket_size) %>%
  dplyr::summarise(
    draws = dplyr::n(),
    median_best_ir = stats::median(ir_best),
    best_ir_q05 = stats::quantile(ir_best, 0.05),
    best_ir_q95 = stats::quantile(ir_best, 0.95),
    median_delta_ir = stats::median(delta_ir),
    probability_gain = mean(delta_ir > 0),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    position = dplyr::row_number(),
    basket_label = dplyr::if_else(
      basket_size == length(reliable_top_funds),
      paste0("All\n(", basket_size, ")"),
      as.character(basket_size)
    )
  )

basket_plot <- ggplot2::ggplot(
  basket_summary,
  ggplot2::aes(x = position, y = median_best_ir)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = best_ir_q05, ymax = best_ir_q95),
    fill = jp_picker$primary,
    alpha = 0.18
  ) +
  ggplot2::geom_line(color = jp_picker$primary, linewidth = 0.9) +
  ggplot2::geom_point(color = jp_picker$primary, size = 2.8) +
  ggplot2::geom_hline(
    yintercept = annualized_ir(primary_ml_ret, bench_fund_vec),
    color = jp_picker$categorical[[2]],
    linetype = "dashed"
  ) +
  ggplot2::scale_x_continuous(
    breaks = basket_summary$position,
    labels = basket_summary$basket_label
  ) +
  ggplot2::labs(
    x = "Number of top-quartile funds in the fixed basket",
    y = "Best Information Ratio",
    title = "Top-performer basket size",
    subtitle = "Median and 5th--95th percentile across fixed baskets; dashed line is RP alone"
  ) +
  journal_area_theme() +
  ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 18))

ggplot2::ggsave(
  here::here("figures", "fund_picker_basket_size.png"),
  basket_plot,
  width = 12.5,
  height = 8.5,
  dpi = 300,
  bg = "white"
)

# Precision calibration: a fraction pi of each fixed basket is sampled from the
# true top quartile and the remainder from outside it. Basket sizes are multiples
# of four so every requested precision is represented exactly.
picker_precisions <- c(0, 0.25, 0.50, 0.75, 1)
precision_basket_sizes <- c(4L, 8L, 20L, 40L)
precision_draws_per_cell <- 500L
non_top_funds <- setdiff(reliable_funds, reliable_top_funds)

set.seed(20260729)
precision_draws <- purrr::map_dfr(precision_basket_sizes, function(k) {
  purrr::map_dfr(picker_precisions, function(precision) {
    n_top <- as.integer(precision * k)
    n_other <- k - n_top
    purrr::map_dfr(seq_len(precision_draws_per_cell), function(draw) {
      selected <- c(
        if (n_top > 0L) sample(reliable_top_funds, n_top, replace = FALSE) else character(),
        if (n_other > 0L) sample(non_top_funds, n_other, replace = FALSE) else character()
      )
      human_returns <- fund_portfolio_return(fund_ret_window, selected)
      dplyr::bind_cols(
        tibble::tibble(
          basket_size = k,
          precision = precision,
          draw = draw
        ),
        combo_ir_result(primary_ml_ret, human_returns, bench_fund_vec)
      )
    })
  })
})

precision_summary <- precision_draws %>%
  dplyr::group_by(basket_size, precision) %>%
  dplyr::summarise(
    median_best_ir = stats::median(ir_best),
    median_delta_ir = stats::median(delta_ir),
    probability_gain = mean(delta_ir > 0),
    .groups = "drop"
  )

precision_thresholds <- precision_summary %>%
  dplyr::group_by(basket_size) %>%
  dplyr::summarise(
    min_precision_positive = if (any(median_delta_ir > 0)) {
      min(precision[median_delta_ir > 0])
    } else {
      NA_real_
    },
    min_precision_likely = if (any(probability_gain >= 0.75)) {
      min(precision[probability_gain >= 0.75])
    } else {
      NA_real_
    },
    delta_at_full_precision = median_delta_ir[precision == 1],
    .groups = "drop"
  )

precision_plot <- ggplot2::ggplot(
  precision_summary,
  ggplot2::aes(
    x = factor(precision, levels = picker_precisions),
    y = factor(basket_size, levels = rev(precision_basket_sizes)),
    fill = median_delta_ir
  )
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(
      # Three decimals: incremental IRs here run to ~0.012, so two decimals
      # collapse every cell to "0.00" or "0.01" and carry no information.
      label = sprintf("%.3f\n(%d%%)", median_delta_ir, round(100 * probability_gain))
    ),
    size = 4.5,
    color = jp_picker$text
  ) +
  ggplot2::scale_fill_gradient2(
    low = jp_picker$negative,
    mid = "#EBEEF1",
    high = jp_picker$primary,
    midpoint = 0,
    breaks = c(0, 0.003, 0.006, 0.009, 0.012),
    labels = function(x) sprintf("%.3f", x),
    name = expression(Delta * " IR")
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      direction = "vertical",
      barheight = grid::unit(55, "mm"),
      barwidth = grid::unit(7, "mm"),
      title.position = "top"
    )
  ) +
  ggplot2::scale_x_discrete(labels = paste0(100 * picker_precisions, "%")) +
  ggplot2::labs(
    x = "Fund-picker precision",
    y = "Basket size",
    title = "Fund-picker precision",
    subtitle = "Cells show median incremental IR; parentheses show probability of a positive gain"
  ) +
  journal_area_theme() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    legend.position = "right",
    plot.margin = ggplot2::margin(10, 10, 10, 18)
  )

ggplot2::ggsave(
  here::here("figures", "fund_picker_precision.png"),
  precision_plot,
  width = 11.5,
  height = 8.5,
  dpi = 300,
  bg = "white"
)

# Past-only diagnostic: use the preceding 12 months (at least nine observations)
# to select the next month's top quartile. Evaluation begins after the warm-up.
trailing_lookback <- 12L
trailing_min_obs <- 9L
trailing_eval_idx <- seq.int(trailing_lookback + 1L, nrow(fund_ret_window))
trailing_q75 <- vapply(trailing_eval_idx, function(t) {
  history <- fund_ret_window[(t - trailing_lookback):(t - 1L), , drop = FALSE]
  scores <- colMeans(history, na.rm = TRUE)
  valid <- colSums(is.finite(history)) >= trailing_min_obs & is.finite(scores)
  if (!any(valid)) return(NA_real_)
  ranked <- names(scores)[valid][order(-scores[valid], names(scores)[valid])]
  selected <- ranked[seq_len(max(1L, ceiling(0.25 * length(ranked))))]
  current <- fund_ret_window[t, selected, drop = TRUE]
  if (all(!is.finite(current))) NA_real_ else mean(current, na.rm = TRUE)
}, numeric(1))

trailing_result <- combo_ir_result(
  primary_ml_ret[trailing_eval_idx],
  trailing_q75,
  bench_fund_vec[trailing_eval_idx]
)

# Compact table: oracle threshold sensitivity, basket dispersion, precision
# thresholds, and the separate past-only diagnostic.
oracle_table <- oracle_quantile_df %>%
  dplyr::filter(scheme == primary_ml_scheme) %>%
  dplyr::left_join(oracle_bootstrap_summary, by = "top_share") %>%
  dplyr::arrange(dplyr::desc(top_share))

oracle_table_rows <- sprintf(
  "%.0f\\%% & %d & %.2f & %.2f & %.2f & %.2f & %.0f\\%% & [%.2f, %.2f] \\\\",
  100 * oracle_table$top_share,
  oracle_table$n_funds,
  oracle_table$ir_human,
  oracle_table$ir_ml,
  oracle_table$ir_best,
  oracle_table$delta_ir,
  100 * oracle_table$w_ir,
  oracle_table$delta_lo,
  oracle_table$delta_hi
)

basket_table_rows <- sprintf(
  "%s & %d & %.2f & %.2f & %.2f & %.2f & %.0f\\%% & \\\\",
  gsub("\n", " ", basket_summary$basket_label),
  basket_summary$draws,
  basket_summary$median_best_ir,
  basket_summary$best_ir_q05,
  basket_summary$best_ir_q95,
  basket_summary$median_delta_ir,
  100 * basket_summary$probability_gain
)

format_precision <- function(x) {
  ifelse(is.finite(x), sprintf("%.0f\\%%", 100 * x), "--")
}
precision_table_rows <- sprintf(
  "%d & %s & %s & %.2f & & & & \\\\",
  precision_thresholds$basket_size,
  format_precision(precision_thresholds$min_precision_positive),
  format_precision(precision_thresholds$min_precision_likely),
  precision_thresholds$delta_at_full_precision
)

picker_table_tex <- c(
  "\\begin{table}[p]",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  "\\renewcommand{\\arraystretch}{1.02}",
  paste0(
    "\\caption[Fund-picking robustness of the RP--human frontier]",
    "{\\footnotesize \\textbf{: Fund-picking ",
    "robustness of the RP--human frontier.} RP is the pre-specified ML portfolio ",
    "because it is a transparent, risk-controlled ensemble; it was not selected ",
    "because it produced the strongest result. ML returns are net of transaction ",
    "costs and the 1\\% annual management fee. All Information Ratios use 15th-to-15th ",
    "monthly returns and the IBOV as benchmark, and are computed as the annualized ",
    "arithmetic mean active return divided by the tracking error, matching ",
    "Table~\\ref{combofront} rather than the geometric convention of ",
    "Table~\\ref{actret}. Panel A ranks funds over the full ",
    "evaluation window and varies the retained top share. Human IR and RP IR are ",
    "standalone values; Best IR is the maximum over RP--human weights from zero to ",
    "one in five-percentage-point increments, and $w^{*}$ is the corresponding RP ",
    "weight. $\\Delta$IR is Best IR minus the better standalone IR. Because the ",
    "weight grid contains both endpoints, $\\Delta$IR is non-negative by ",
    "construction and is inflated by the maximization over the grid; the bracket ",
    "is therefore a percentile band for a bounded statistic, not a two-sided ",
    "significance interval, and a lower bound of 0.00 carries no test of a null. ",
    "Bands come from 999 joint circular 12-month block-bootstrap samples, with ",
    "funds re-ranked inside every sample. Panel B draws 1,000 fixed equal-weight ",
    "baskets without replacement from the ex-post top quartile. P5 and P95 describe ",
    "basket-selection dispersion, not confidence intervals. Panel C uses 500 baskets ",
    "per cell and controls the fraction selected from the ex-post top quartile; ",
    "Min. positive is the lowest precision with positive median $\\Delta$IR, which ",
    "is equivalent to $P(\\Delta\\mathrm{IR}>0)>50\\%$ because $\\Delta$IR is ",
    "non-negative by construction; Min. likely is the stricter crossing at ",
    "$P(\\Delta\\mathrm{IR}>0)\\geq 75\\%$. The ",
    "final row replaces full-window ranking with a trailing 12-month, past-only ",
    "top-quartile rule.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{1.7cm}*{7}{>{\\raggedleft\\arraybackslash}X}}",
  "\\hline",
  "\\multicolumn{8}{l}{\\textit{Panel A: oracle top-share sensitivity (RP)}} \\\\",
  "Top share & Funds & Human IR & RP IR & Best IR & $\\Delta$IR & $w^{*}$ & 95\\% band $\\Delta$IR \\\\",
  "\\hline",
  oracle_table_rows,
  "\\hline",
  "\\multicolumn{8}{l}{\\textit{Panel B: fixed baskets sampled from the oracle top quartile}} \\\\",
  "Funds & Draws & Median best & P5 & P95 & Median $\\Delta$IR & $P(\\Delta>0)$ & \\\\",
  "\\hline",
  basket_table_rows,
  "\\hline",
  "\\multicolumn{8}{l}{\\textit{Panel C: calibrated picker precision}} \\\\",
  "Funds & Min. positive & Min. likely & $\\Delta$IR at 100\\% & & & & \\\\",
  "\\hline",
  precision_table_rows,
  "\\hline",
  sprintf(
    "\\multicolumn{8}{l}{\\textit{Past-only Q75 (12m, $T=%d$): Human %.2f; Best %.2f; $\\Delta$IR %.2f; $w^{*}$ %.0f\\%%.}} \\\\",
    length(trailing_eval_idx),
    trailing_result$ir_human,
    trailing_result$ir_best,
    trailing_result$delta_ir,
    100 * trailing_result$w_ir
  ),
  "\\hline",
  "\\end{tabularx}",
  "\\label{fundpickerrobust}",
  "\\end{table}"
)
writeLines(picker_table_tex, here::here("tables", "fund_picker_robustness.tex"))

picker_figures_tex <- c(
  latex_figure(
    "fund_picker_quantile_sensitivity.png",
    "Fund-picking sensitivity of the combination frontier.",
    paste(
      sprintf(
        paste(
          "Funds are ranked by the arithmetic mean of their monthly 15th-to-15th",
          "returns over the full evaluation window. That statistic mildly favours",
          "volatile funds relative to a compounded score, but %.0f\\%% of the",
          "top-quartile membership is unchanged under a geometric ranking. Because",
          "eligibility requires only 36 monthly observations, %.0f\\%% of",
          "top-quartile funds are ranked on a shorter history than the full window,",
          "so part of the measured advantage reflects short-sample dispersion."
        ),
        rank_overlap_pct, rank_partial_history_pct
      ),
      "The retained share varies from the top 50\\% to the top 5\\%. Red lines show the",
      "Information Ratio of the equal-weight human portfolio; blue lines show the",
      "maximum Information Ratio over five-percentage-point combinations with each",
      "ML portfolio. RP is highlighted because it is the pre-specified transparent,",
      "risk-controlled ensemble, not because it is ex-post best; the other seven ML",
      "schemes show whether the quantile pattern generalizes. The dotted line marks",
      "the baseline top quartile. In the RP panel, shading is the 95\\% interval from",
      "999 joint circular 12-month block-bootstrap samples with funds re-ranked in",
      "each sample. Because the main rankings use the full window, these curves are",
      "oracle sensitivity bounds rather than implementable investment strategies."
    ),
    "fig:picker_quantile"
  ),
  "",
  latex_figure(
    "fund_picker_basket_size.png",
    "Top-performer basket size.",
    paste(
      "Each draw samples, without replacement, a fixed equal-weight basket from the",
      "funds ranked in the ex-post top quartile and holds that basket throughout the",
      "evaluation window. For each basket size, 1,000 draws are combined with RP over",
      "a five-percentage-point weight grid. The solid line is the median attainable",
      "Information Ratio and the band spans its 5th to 95th percentiles across basket",
      "draws. The dashed line is standalone RP; the final point includes every",
      "top-quartile fund. The band therefore measures fund-selection dispersion, not",
      "time-series sampling uncertainty, and the ex-post top-quartile restriction",
      "makes the exercise an oracle diversification benchmark."
    ),
    "fig:picker_basket"
  ),
  "",
  latex_figure(
    "fund_picker_precision.png",
    "Fund-picker precision.",
    paste(
      "For basket sizes 4, 8, 20 and 40, a controlled fraction of funds is sampled",
      "without replacement from the ex-post top quartile and the remainder from the",
      "other eligible funds. Each cell summarizes 500 fixed equal-weight baskets.",
      "Every basket is combined with RP over a five-percentage-point weight grid.",
      "Cell colour and the first number report the median incremental Information",
      "Ratio relative to the better standalone RP or human basket; the parenthesized",
      "number is the fraction of draws with a positive increment. The exercise",
      "calibrates how accurately an investor must identify eventual winners, but",
      "because winner status uses the full window it is not an implementable picker."
    ),
    "fig:picker_precision"
  )
)
writeLines(
  picker_figures_tex,
  here::here("figures", "fund_picker_robustness_figures.tex")
)

# ---- Mean-variance spanning -------------------------------------------------
# For Y = alpha + B X + epsilon, spanning imposes alpha = 0 and B 1 = 1
# for every test asset. We report both directions and use a restricted-residual
# circular block bootstrap to preserve serial and cross-series dependence.
spanning_wald_stat <- function(benchmark_returns, test_returns) {
  benchmark_returns <- as.matrix(benchmark_returns)
  test_returns <- as.matrix(test_returns)
  if (nrow(benchmark_returns) != nrow(test_returns) ||
      any(!is.finite(benchmark_returns)) || any(!is.finite(test_returns))) {
    rlang::abort("Spanning-test return matrices must be finite and row-aligned.")
  }
  design <- cbind(intercept = 1, benchmark_returns)
  if (qr(design)$rank < ncol(design)) {
    rlang::abort("The spanning-test benchmark design matrix is rank deficient.")
  }
  xtx_inv <- solve(crossprod(design))
  coefficients <- xtx_inv %*% crossprod(design, test_returns)
  residuals <- test_returns - design %*% coefficients
  sigma <- crossprod(residuals) / (nrow(design) - ncol(design))
  restriction_one <- rbind(
    c(1, rep(0, ncol(benchmark_returns))),
    c(0, rep(1, ncol(benchmark_returns)))
  )
  restriction <- kronecker(diag(ncol(test_returns)), restriction_one)
  target <- rep(c(0, 1), ncol(test_returns))
  gap <- as.vector(restriction %*% as.vector(coefficients) - target)
  coefficient_vcov <- kronecker(sigma, xtx_inv)
  gap_vcov <- restriction %*% coefficient_vcov %*% t(restriction)
  if (rcond(gap_vcov) < 1e-12) {
    rlang::abort("The spanning-test restriction covariance is numerically singular.")
  }
  statistic <- as.numeric(crossprod(gap, solve(gap_vcov, gap)))
  list(
    statistic = statistic,
    df = length(gap),
    coefficients = coefficients,
    residuals = residuals,
    design = design,
    restriction_one = restriction_one,
    xtx_inv = xtx_inv
  )
}

spanning_test <- function(
    benchmark_returns,
    test_returns,
    bootstrap_reps = 999L,
    block_length = 12L,
    seed = 1L) {
  observed <- spanning_wald_stat(benchmark_returns, test_returns)
  constraint_target <- c(0, 1)
  restricted_coefficients <- apply(
    observed$coefficients,
    2,
    function(coefficient) {
      middle <- observed$restriction_one %*% observed$xtx_inv %*%
        t(observed$restriction_one)
      coefficient - observed$xtx_inv %*% t(observed$restriction_one) %*%
        solve(
          middle,
          observed$restriction_one %*% coefficient - constraint_target
        )
    }
  )
  restricted_fitted <- observed$design %*% restricted_coefficients
  restricted_residuals <- test_returns - restricted_fitted
  restricted_residuals <- scale(
    restricted_residuals,
    center = TRUE,
    scale = FALSE
  )

  set.seed(seed)
  bootstrap_statistics <- vapply(seq_len(bootstrap_reps), function(draw) {
    idx <- circular_block_indices(nrow(observed$design), block_length)
    test_star <- restricted_fitted + restricted_residuals[idx, , drop = FALSE]
    spanning_wald_stat(benchmark_returns, test_star)$statistic
  }, numeric(1))

  tibble::tibble(
    statistic = observed$statistic,
    df = observed$df,
    asymptotic_p = stats::pchisq(observed$statistic, observed$df, lower.tail = FALSE),
    bootstrap_p = (1 + sum(bootstrap_statistics >= observed$statistic)) /
      (bootstrap_reps + 1),
    bootstrap_reps = bootstrap_reps,
    block_length = block_length,
    design_condition = kappa(observed$design)
  )
}

max_long_only_ratio <- function(return_matrix, reference_returns) {
  return_matrix <- as.matrix(return_matrix)
  if (nrow(return_matrix) != length(reference_returns) ||
      any(!is.finite(return_matrix)) || any(!is.finite(reference_returns))) {
    rlang::abort("Long-only ratio inputs must be finite and row-aligned.")
  }
  centered <- sweep(return_matrix, 1, reference_returns, "-")
  n_assets <- ncol(centered)
  objective <- function(weights) {
    portfolio <- as.numeric(centered %*% weights)
    portfolio_sd <- stats::sd(portfolio)
    if (!is.finite(portfolio_sd) || portfolio_sd <= 0) return(1e6)
    -mean(portfolio) / portfolio_sd * sqrt(12)
  }
  starts <- rbind(rep(1 / n_assets, n_assets), diag(n_assets))
  solutions <- lapply(seq_len(nrow(starts)), function(i) {
    nloptr::slsqp(
      x0 = starts[i, ],
      fn = objective,
      lower = rep(0, n_assets),
      upper = rep(1, n_assets),
      heq = function(weights) sum(weights) - 1,
      control = list(xtol_rel = 1e-10, maxeval = 1000, print_level = 0),
      deprecatedBehavior = FALSE
    )
  })
  valid <- vapply(
    solutions,
    function(solution) {
      solution$convergence > 0 &&
        all(is.finite(solution$par)) &&
        abs(sum(solution$par) - 1) < 1e-6 &&
        all(solution$par >= -1e-8)
    },
    logical(1)
  )
  if (!any(valid)) rlang::abort("The long-only ratio optimization did not converge.")
  objectives <- vapply(solutions[valid], function(solution) solution$value, numeric(1))
  best <- solutions[valid][[which.min(objectives)]]
  list(ratio = -best$value, weights = best$par)
}

spanning_ml_names <- c("RP", "CS", "MVO_BOX")
if (!all(spanning_ml_names %in% colnames(ml_ret_mat))) {
  rlang::abort("One or more pre-specified spanning-test ML portfolios are missing.")
}
spanning_ml <- ml_ret_mat[, spanning_ml_names, drop = FALSE]
spanning_human <- perf_mat[, c("FUNDS_MEAN", "FUNDS_Q75"), drop = FALSE]

spanning_specs <- list(
  "ML spans human" = list(benchmark = spanning_ml, test = spanning_human),
  "Human spans ML" = list(benchmark = spanning_human, test = spanning_ml)
)

spanning_results <- purrr::imap_dfr(spanning_specs, function(spec, direction) {
  test_result <- spanning_test(
    spec$benchmark,
    spec$test,
    bootstrap_reps = 999L,
    block_length = 12L,
    seed = if (direction == "ML spans human") 20260730L else 20260731L
  )
  # Per-test-asset intercepts drive the joint rejection, so report them: a
  # sufficiently NEGATIVE alpha also rejects spanning, but only a short seller
  # could exploit it. Reporting the sign keeps the long-only reading honest.
  fit <- spanning_wald_stat(spec$benchmark, spec$test)
  resid_vcov <- crossprod(fit$residuals) /
    (nrow(fit$design) - ncol(fit$design))
  alpha_hat <- fit$coefficients[1, ]
  alpha_se <- sqrt(diag(resid_vcov) * fit$xtx_inv[1, 1])
  alpha_note <- paste(
    sprintf(
      "%s $\\alpha$ %.2f ($t$ %.2f)",
      gsub("_", "\\\\_", colnames(spec$test)), alpha_hat, alpha_hat / alpha_se
    ),
    collapse = "; "
  )
  expanded <- cbind(spec$benchmark, spec$test)
  benchmark_sharpe <- max_long_only_ratio(spec$benchmark, rf_vec)$ratio
  expanded_sharpe <- max_long_only_ratio(expanded, rf_vec)$ratio
  benchmark_ir <- max_long_only_ratio(spec$benchmark, bench_fund_vec)$ratio
  expanded_ir <- max_long_only_ratio(expanded, bench_fund_vec)$ratio
  test_result %>%
    dplyr::mutate(
      direction = direction,
      benchmark_assets = paste(colnames(spec$benchmark), collapse = ", "),
      test_assets = paste(colnames(spec$test), collapse = ", "),
      alpha_note = alpha_note,
      delta_sharpe = max(0, expanded_sharpe - benchmark_sharpe),
      delta_ir = max(0, expanded_ir - benchmark_ir),
      .before = 1
    )
})

spanning_table_rows <- sprintf(
  "%s & %d & %d & %.2f & %d & %.3f & %.3f & %.2f & %.3f & %.3f \\\\",
  spanning_results$direction,
  vapply(spanning_specs, function(x) ncol(x$benchmark), integer(1)),
  vapply(spanning_specs, function(x) ncol(x$test), integer(1)),
  spanning_results$statistic,
  spanning_results$df,
  spanning_results$asymptotic_p,
  spanning_results$bootstrap_p,
  spanning_results$design_condition,
  spanning_results$delta_sharpe,
  spanning_results$delta_ir
)

spanning_table_tex <- c(
  "\\begin{table}[h]",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  paste0(
    "\\caption[Two-direction mean--variance spanning tests]",
    "{\\footnotesize \\textbf{: Two-direction ",
    "mean--variance spanning tests.} For $Y_t=\\alpha+B X_t+\\varepsilon_t$, the ",
    "joint restrictions $\\alpha=0$ and $B\\mathbf{1}=\\mathbf{1}$ state that the ",
    "$K$ benchmark assets span the mean--variance opportunity set of the $N$ test ",
    "assets. Rejection means the test assets shift the unconstrained mean--variance ",
    "frontier, which a sufficiently negative intercept achieves through a short ",
    "sale; the economically relevant magnitudes for a long-only investor are ",
    "therefore $\\Delta$SR and $\\Delta$IR, and the intercepts reported beneath the ",
    "table show the direction of each deviation. ",
    "In ``ML spans human'', $X$ contains RP, CS and MVO\\_BOX and ",
    "$Y$ contains FUNDS\\_MEAN and the ex-post FUNDS\\_Q75; the reverse row swaps ",
    "the sets. The three ML portfolios are pre-specified representatives of a ",
    "risk-controlled ensemble, direct signal weighting and constrained ",
    "mean--variance optimization, keeping the benchmark dimension modest rather ",
    "than selecting portfolios by performance. $p_{\\chi^2}$ is the asymptotic Wald ",
    "p-value; $p_{BB}$ uses 999 restricted-residual circular 12-month block-bootstrap ",
    "replications. $\\kappa(X)$ is the condition number of the regression design. ",
    "$\\Delta$SR and $\\Delta$IR are increases in the maximum long-only Sharpe ratio ",
    "relative to CDI and Information Ratio relative to IBOV after adding the test ",
    "assets. Results are conditional on the common full evaluation window and, for ",
    "FUNDS\\_Q75, an ex-post fund classification.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{2.5cm}*{9}{>{\\raggedleft\\arraybackslash}X}}",
  "\\hline",
  "Direction & $K$ & $N$ & Wald & df & $p_{\\chi^2}$ & $p_{BB}$ & $\\kappa(X)$ & $\\Delta$SR & $\\Delta$IR \\\\",
  "\\hline",
  spanning_table_rows,
  "\\hline",
  # A paragraph column so the intercept notes wrap instead of overfilling the
  # line; `l` would overflow \linewidth for the three-asset direction.
  sprintf(
    "\\multicolumn{10}{@{}p{0.98\\linewidth}@{}}{\\textit{%s: %s (percent per month).}} \\\\",
    spanning_results$direction, spanning_results$alpha_note
  ),
  "\\hline",
  "\\end{tabularx}",
  "\\label{spanningtests}",
  "\\end{table}"
)
writeLines(spanning_table_tex, here::here("tables", "spanning_tests.tex"))

# ---- Appendix: hyperparameter search space ---------------------------------
# The bounds are READ from the tuning strategies stored with each base learner
# rather than transcribed by hand, so this table cannot drift from the
# configuration that was actually run. Every (model, hyperparameter) pair must
# map to a display row: an unmapped pair aborts instead of being dropped
# silently, which is what allows the table to stay honest as the config evolves.
#
# ARM-AGNOSTIC. A grid run stores a grid_search strategy whose
# hyperparameter_list holds the full candidate vector per hyperparameter; a
# Bayesian run stores a two-element [lo, hi] domain. Both are summarised by
# their min and max, so the table reports the range actually searched either
# way. Insisting on `bayesian_opt_strategy` here aborted the whole script on the
# paper's grid run.
bo_strategies <- purrr::imap(sb_list, function(sbr, nm) {
  ts <- tryCatch(sbr@sb_backtest_config@tuning_strategy, error = function(e) NULL)
  if (is.null(ts) || !"hyper_grid_domain" %in% methods::slotNames(ts)) return(NULL)
  ts
})
names(bo_strategies) <- fi_model_label(names(bo_strategies))
bo_strategies <- purrr::compact(bo_strategies)
bo_model_order <- intersect(fi_model_order, names(bo_strategies))
bo_strategies <- bo_strategies[bo_model_order]
if (length(bo_strategies) == 0L) {
  rlang::abort("No tuning strategy carrying a hyperparameter domain was found in the base-learner configs.")
}

# Name the arm from the configs rather than assuming it, so the caption cannot
# describe a Bayesian search for a grid run.
bo_methods <- unique(vapply(bo_strategies, function(ts) {
  m <- tryCatch(as.character(ts@tuning_method), error = function(e) NA_character_)
  if (length(m) != 1L || is.na(m)) "unknown" else m
}, character(1)))
bo_arm_label <- if (identical(bo_methods, "grid_search")) {
  "Grid-search"
} else if (length(bo_methods) == 1L && grepl("bayes", bo_methods)) {
  "Bayesian-optimization"
} else {
  "Hyperparameter"
}
message("Search-space appendix: tuning method(s) = ",
        paste(bo_methods, collapse = ", "), " -> '", bo_arm_label, "'")

bo_bounds <- purrr::imap_dfr(bo_strategies, function(ts, label) {
  hl <- ts@hyper_grid_domain@hyperparameter_list
  purrr::imap_dfr(hl, function(v, hyper) {
    # A Bayesian domain is [lo, hi]; a grid is the full candidate vector. Both
    # reduce to a range, so accept any non-empty numeric.
    if (!is.numeric(v) || length(v) < 1L || anyNA(v)) {
      rlang::abort(paste0("Unexpected search domain for ", label, "::", hyper, "."))
    }
    tibble::tibble(model = label, hyper = hyper, lo = min(v), hi = max(v))
  })
})

# NN1-NN3 differ only in depth, so they share one search space and one column.
# Verify rather than assume: if they ever diverge, the table must split them.
bo_nn <- intersect(c("NN1", "NN2", "NN3"), bo_bounds$model)
if (length(bo_nn) > 1L) {
  bo_nn_sets <- lapply(bo_nn, function(m) {
    bo_bounds %>% dplyr::filter(model == m) %>%
      dplyr::arrange(hyper) %>% dplyr::select(hyper, lo, hi)
  })
  if (!all(vapply(bo_nn_sets[-1], function(d) isTRUE(all.equal(d, bo_nn_sets[[1]])),
                  logical(1)))) {
    rlang::abort("NN1-NN3 no longer share one search space; split the appendix columns.")
  }
  bo_bounds <- bo_bounds %>%
    dplyr::filter(!model %in% bo_nn[-1]) %>%
    dplyr::mutate(model = dplyr::if_else(model == bo_nn[[1]], "NN", model))
}

# Keyed on model AND hyperparameter: `alpha` is the L1/L2 mixing weight in
# glmnet but the L1 penalty in XGBoost, so the name alone is ambiguous.
bo_concept <- c(
  "GLMNET::lambda.min.ratio" = "Penalty path floor $\\lambda^{\\psi}$",
  "GLMNET::alpha"            = "L1/L2 mixing $\\lambda^{\\rho}$",
  "RF::num.trees"            = "Trees / boosting rounds $\\lambda^{T}$, $\\lambda^{F}$",
  "XGB::nrounds"             = "Trees / boosting rounds $\\lambda^{T}$, $\\lambda^{F}$",
  "RF::max.depth"            = "Maximum depth $\\lambda^{D}$",
  "XGB::max_depth"           = "Maximum depth $\\lambda^{D}$",
  "RF::min.bucket"           = "Minimum terminal node size $\\lambda^{L}$",
  "XGB::min_child_weight"    = "Minimum terminal node size $\\lambda^{L}$",
  "RF::mtry"                 = "Feature subsampling fraction $\\lambda^{R}$",
  "XGB::colsample_bytree"    = "Feature subsampling fraction $\\lambda^{R}$",
  "XGB::subsample"           = "Row subsampling fraction $\\lambda^{S}$",
  "XGB::eta"                 = "Learning rate",
  "NN::lr"                   = "Learning rate",
  "XGB::alpha"               = "L1 penalty",
  "NN::regularizer_l1"       = "L1 penalty",
  "NN::regularizer_l2"       = "L2 penalty",
  "XGB::gamma"               = "Minimum split loss",
  "NN::droprate"             = "Dropout rate",
  "NN::size_of_batch"        = "Batch size $\\lambda^{b}$",
  "NN::number_of_epochs"     = "Epochs $\\lambda^{e}$"
)
bo_row_order <- c(
  "Penalty path floor $\\lambda^{\\psi}$",
  "L1/L2 mixing $\\lambda^{\\rho}$",
  "Trees / boosting rounds $\\lambda^{T}$, $\\lambda^{F}$",
  "Maximum depth $\\lambda^{D}$",
  "Minimum terminal node size $\\lambda^{L}$",
  "Feature subsampling fraction $\\lambda^{R}$",
  "Row subsampling fraction $\\lambda^{S}$",
  "Learning rate",
  "L1 penalty",
  "L2 penalty",
  "Minimum split loss",
  "Dropout rate",
  "Batch size $\\lambda^{b}$",
  "Epochs $\\lambda^{e}$"
)

bo_bounds$key <- paste(bo_bounds$model, bo_bounds$hyper, sep = "::")
bo_unmapped <- setdiff(bo_bounds$key, names(bo_concept))
if (length(bo_unmapped) > 0L) {
  rlang::abort(paste0(
    "Unmapped hyperparameter(s) in the appendix search-space table: ",
    paste(bo_unmapped, collapse = ", "), "."
  ))
}
bo_bounds$concept <- unname(bo_concept[bo_bounds$key])
stopifnot(all(bo_bounds$concept %in% bo_row_order))

# Exact powers of ten below 0.01 render as 10^k; everything else keeps its
# literal value, so a column of fractions reads 0.1 / 0.6 / 0.75 rather than
# mixing $10^{-1}$ into a list of decimals.
fmt_bo <- function(x) {
  if (!is.finite(x)) return("--")
  if (x == 0) return("0")
  if (x == round(x) && abs(x) >= 1) return(format(x, scientific = FALSE, trim = TRUE))
  e <- log10(abs(x))
  if (abs(x) < 0.01 && abs(e - round(e)) < 1e-9) {
    return(sprintf("$10^{%d}$", as.integer(round(e))))
  }
  sub("[.]$", "", sub("0+$", "", sprintf("%.5f", x)))
}

bo_cols <- intersect(c("GLMNET", "RF", "XGB", "NN"), unique(bo_bounds$model))
bo_col_label <- c(GLMNET = "ENET", RF = "RF", XGB = "XGB", NN = "NN1--NN3")

# NN1--NN3's cells hold longer bracketed intervals (e.g. "[1e-05, 0.01]"), so an
# equal-width X column wraps there first. Give it extra share of the X-column
# width via tabularx's \hsize ratio trick, renormalized so the ratios still sum
# to the column count -- otherwise the columns would over- or under-fill
# \linewidth.
bo_col_weight <- setNames(rep(1, length(bo_cols)), bo_cols)
if ("NN" %in% names(bo_col_weight)) bo_col_weight[["NN"]] <- 1.35
bo_col_weight <- bo_col_weight / sum(bo_col_weight) * length(bo_col_weight)
bo_colspec <- paste(
  sprintf(">{\\hsize=%.3f\\hsize\\raggedleft\\arraybackslash}X", bo_col_weight[bo_cols]),
  collapse = ""
)

bo_cell <- function(concept, model) {
  r <- bo_bounds[bo_bounds$concept == concept & bo_bounds$model == model, , drop = FALSE]
  if (nrow(r) == 0L) return("--")
  if (nrow(r) > 1L) {
    rlang::abort(paste0("Two hyperparameters map to '", concept, "' for ", model, "."))
  }
  sprintf("[%s, %s]", fmt_bo(r$lo[[1]]), fmt_bo(r$hi[[1]]))
}

bo_table_rows <- vapply(intersect(bo_row_order, bo_bounds$concept), function(concept) {
  paste0(concept, " & ",
         paste(vapply(bo_cols, function(m) bo_cell(concept, m), character(1)),
               collapse = " & "),
         " \\\\")
}, character(1), USE.NAMES = FALSE)

bo_budget_row <- function(label, field) {
  paste0(label, " & ",
         paste(vapply(bo_cols, function(m) {
           src <- if (m == "NN") bo_nn[[1]] else m
           format(methods::slot(bo_strategies[[src]], field), trim = TRUE)
         }, character(1)), collapse = " & "),
         " \\\\")
}

bo_ref <- bo_strategies[[bo_cols[[1]]]]

# Budget rows are arm-specific. `bayesian_opt_strategy` carries init_points /
# n_iter / acq; `grid_search_strategy` adds NO slots beyond tuning_strategy, so
# reading those off a grid run errors. A grid search's budget is instead the
# number of combinations evaluated at each refit -- the product of the candidate
# counts, which is what run_paper_machine.R reports as "fits/date".
bo_is_bayes <- identical(bo_arm_label, "Bayesian-optimization")

bo_budget_rows <- if (bo_is_bayes) {
  c(bo_budget_row("Initial design points", "init_points"),
    bo_budget_row("Acquisition iterations", "n_iter"))
} else {
  paste0("Grid combinations per refit & ",
         paste(vapply(bo_cols, function(m) {
           src <- if (m == "NN") bo_nn[[1]] else m
           hl  <- bo_strategies[[src]]@hyper_grid_domain@hyperparameter_list
           format(prod(vapply(hl, length, numeric(1))), trim = TRUE)
         }, character(1)), collapse = " & "),
         " \\\\")
}

# Only a Bayesian run has an acquisition function to name.
bo_note <- sprintf(
  paste0("\\multicolumn{%d}{@{}p{0.98\\linewidth}@{}}{\\textit{%sObjective: ",
         "validation %s over a %d-month validation window.}} \\\\"),
  length(bo_cols) + 1L,
  if (bo_is_bayes) paste0("Acquisition function: ", toupper(bo_ref@acq), ". ") else "",
  toupper(bo_ref@chosen_eval_metric), bo_ref@validation_sample_size
)

bounds_table_tex <- c(
  "\\begin{table}[h]",
  "\\captionsetup{",
  "  justification=raggedright,",
  "  labelsep=none,",
  "  singlelinecheck=false,",
  "  labelformat=boldlabel,",
  "}",
  "\\renewcommand{\\arraystretch}{1.25}",
  paste0(
    "\\caption[", bo_arm_label, " search space]",
    "{\\footnotesize \\textbf{: ", bo_arm_label, " search space.} Each entry is ",
    "the range searched for that hyperparameter; \"--\" means the ",
    "hyperparameter does not apply to that model. Bounds are read directly from ",
    "the tuning configuration stored with each fitted base learner, so they ",
    "describe the search that was actually run. Hyperparameters are re-optimized ",
    "at every walk-forward refit, so the table reports the domain rather than a ",
    "selected value; Figure~\\ref{fig:hyper_time} shows the chosen values over ",
    "time. OLS is unregularized and has no tuned hyperparameters. NN1--NN3 share ",
    "one search space and differ only in depth. Symbols follow the notation of ",
    "Section~\\ref{sec:methods}; the mixing weight $\\lambda^{\\rho}$ of ",
    "Equation~\\ref{elasticeq} is the ridge share, which is one minus ",
    "\\texttt{glmnet}'s \\texttt{alpha} and therefore searched over the same ",
    "interval. The penalty path floor is \\texttt{lambda.min.ratio}, the ratio of ",
    "the smallest to the largest $\\lambda^{\\psi}$ on the fitted path, not ",
    "$\\lambda^{\\psi}$ itself. ",
    # The last two sentences describe the search budget, which differs by arm --
    # leaving the Bayesian wording in place made the caption contradict the
    # "Grid combinations per refit" row on a grid run.
    if (bo_is_bayes) {
      paste0("Integer-valued hyperparameters are searched on the integers ",
             "within the stated interval. The final rows give the search ",
             "budget: the size of the initial space-filling design and the ",
             "number of subsequent acquisition steps, per refit.}")
    } else {
      paste0("Each hyperparameter is searched over an explicit list of ",
             "candidate values rather than a continuous interval, so the ",
             "columns report that list's range. The final row gives the search ",
             "budget: the number of grid combinations evaluated per refit, ",
             "i.e.\\ the product of the candidate counts.}")
    }
  ),
  # \scriptsize, matching every other generated table's body font: at
  # \footnotesize the bracketed intervals wrap mid-cell in the narrower columns.
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{4pt}",
  paste0(
    "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{5.0cm}",
    bo_colspec, "}"
  ),
  "\\hline",
  paste0("Hyperparameter & ",
         paste(unname(bo_col_label[bo_cols]), collapse = " & "), " \\\\"),
  "\\hline",
  bo_table_rows,
  "\\hline",
  bo_budget_rows,
  "\\hline",
  bo_note,
  "\\hline",
  "\\end{tabularx}",
  "\\label{hyperchoice}",
  "\\end{table}"
)
writeLines(bounds_table_tex, here::here("tables", "hyperparameter_bounds.tex"))

# ---- Appendix: realised grid and Bayesian search spaces -------------------
# The first table builder above predates the Bayesian robustness run. Replace
# its one-arm output with a two-panel table read from the recorded workflows.
# This preserves the realised domains when the current script later changes.
hp_scalar_or_na <- function(x) {
  if (is.null(x) || length(x) == 0L) NA_real_ else as.numeric(x[[1]])
}

hp_extract_workflow <- function(base_results, arm) {
  purrr::imap_dfr(base_results, function(sbr, nm) {
    workflow <- sbr@sb_backtest_workflow[[1]]
    domain <- workflow$hyper_grid_domain_list
    if (is.null(domain)) return(tibble::tibble())

    purrr::imap_dfr(domain, function(value, hyper) {
      fixed <- is.list(value) && identical(value$distribution_choice, "constant")
      domain_values <- if (fixed) as.numeric(value$value) else as.numeric(value)
      if (length(domain_values) == 0L || anyNA(domain_values) || any(!is.finite(domain_values))) {
        rlang::abort(paste0("Invalid recorded domain for ", arm, "::", nm, "::", hyper, "."))
      }
      tibble::tibble(
        arm = arm,
        model = fi_model_label(nm),
        hyper = hyper,
        candidate_values = list(domain_values),
        lo = min(domain_values),
        hi = max(domain_values),
        fixed = fixed,
        method = workflow$tuning_method,
        init_points = hp_scalar_or_na(workflow$init_points),
        n_iter = hp_scalar_or_na(workflow$n_iter),
        acquisition = workflow$acq
      )
    })
  })
}

hp_grid <- hp_extract_workflow(sb_list, "Grid search")
if (nrow(hp_grid) == 0L || !all(hp_grid$method == "grid_search")) {
  rlang::abort("The baseline bundle does not contain a complete grid-search specification.")
}

# No later plot requires the grid base-learner library. Releasing it before
# loading the robustness bundle avoids retaining both large result objects.
rm(mb_grid, sb_list)
invisible(gc())
hp_bayes_bundle <- read_run_bundle("mb_bayes.qs")
hp_bayes <- hp_extract_workflow(
  hp_bayes_bundle@base_sb_backtest_results_list,
  "Bayesian optimization"
)
rm(hp_bayes_bundle)
invisible(gc())
if (nrow(hp_bayes) == 0L || !all(hp_bayes$method == "bayesian_opt")) {
  rlang::abort("The robustness bundle does not contain a complete Bayesian-optimization specification.")
}
hp_bounds <- dplyr::bind_rows(hp_grid, hp_bayes)

# NN1--NN3 share one displayed column only when their recorded domains agree.
hp_collapse_nn <- function(d) {
  purrr::map_dfr(unique(d$arm), function(this_arm) {
    arm_data <- dplyr::filter(d, arm == this_arm)
    nn_models <- intersect(c("NN1", "NN2", "NN3"), unique(arm_data$model))
    if (length(nn_models) > 1L) {
      nn_sets <- lapply(nn_models, function(model) {
        arm_data %>%
          dplyr::filter(model == !!model) %>%
          dplyr::arrange(hyper) %>%
          dplyr::select(hyper, lo, hi, fixed, method)
      })
      if (!all(vapply(nn_sets[-1], function(x) isTRUE(all.equal(x, nn_sets[[1]])),
                      logical(1)))) {
        rlang::abort(paste0("NN1--NN3 do not share one recorded search space in the ",
                            this_arm, " arm."))
      }
      arm_data <- arm_data %>%
        dplyr::filter(!model %in% nn_models[-1]) %>%
        dplyr::mutate(model = dplyr::if_else(model == nn_models[[1]], "NN", model))
    }
    arm_data
  })
}
hp_bounds <- hp_collapse_nn(hp_bounds)

# Implementation names are learner-specific: glmnet alpha is a mixing weight,
# whereas XGB alpha is an L1 penalty. The manuscript symbols are kept separate
# from their plain-language names in the appendix.
hp_name <- c(
  "GLMNET::lambda.min.ratio" = "Penalty path floor",
  "GLMNET::alpha" = "L1/L2 mixing",
  "RF::num.trees" = "Trees / boosting rounds",
  "XGB::nrounds" = "Trees / boosting rounds",
  "RF::max.depth" = "Maximum depth",
  "XGB::max_depth" = "Maximum depth",
  "RF::min.bucket" = "Minimum terminal node size",
  "XGB::min_child_weight" = "Minimum terminal node size",
  "RF::mtry" = "Feature subsampling fraction",
  "XGB::colsample_bytree" = "Feature subsampling fraction",
  "XGB::subsample" = "Row subsampling fraction",
  "XGB::eta" = "Learning rate",
  "NN::lr" = "Learning rate",
  "XGB::alpha" = "L1 penalty",
  "NN::regularizer_l1" = "L1 penalty",
  "NN::regularizer_l2" = "L2 penalty",
  "XGB::gamma" = "Minimum split loss",
  "NN::droprate" = "Dropout rate",
  "NN::size_of_batch" = "Batch size",
  "NN::number_of_epochs" = "Epochs"
)
hp_symbol <- c(
  "Penalty path floor" = "$\\lambda^{\\psi}$",
  "L1/L2 mixing" = "$\\lambda^{\\rho}$",
  "Trees / boosting rounds" = "$\\lambda^{T}$ / $\\lambda^{F}$",
  "Maximum depth" = "$\\lambda^{D}$",
  "Minimum terminal node size" = "$\\lambda^{L}$",
  "Feature subsampling fraction" = "$\\lambda^{R}$",
  "Row subsampling fraction" = "$\\lambda^{S}$",
  "Learning rate" = "$\\lambda^{\\eta}$",
  "L1 penalty" = "$\\lambda^{\\alpha}$ / $\\lambda^{\\omega}$",
  "L2 penalty" = "$\\lambda^{\\delta}$",
  "Minimum split loss" = "$\\lambda^{\\gamma}$",
  "Dropout rate" = "$\\lambda^{R}_{5}$",
  "Batch size" = "$\\lambda^{b}$",
  "Epochs" = "$\\lambda^{e}$"
)
hp_bounds$key <- paste(hp_bounds$model, hp_bounds$hyper, sep = "::")
hp_unmapped <- setdiff(hp_bounds$key, names(hp_name))
if (length(hp_unmapped) > 0L) {
  rlang::abort(paste0("Unmapped hyperparameter(s): ",
                      paste(hp_unmapped, collapse = ", "), "."))
}
hp_bounds$name <- unname(hp_name[hp_bounds$key])
hp_row_order <- names(hp_symbol)
hp_models <- c("GLMNET", "RF", "XGB", "NN")
hp_model_labels <- c(GLMNET = "ENET", RF = "RF", XGB = "XGB", NN = "NN1--NN3")

hp_format <- function(x) {
  if (x == 0) return("0")
  if (x == round(x) && abs(x) >= 1) return(format(x, scientific = FALSE, trim = TRUE))
  exponent <- log10(abs(x))
  if (abs(x) < 0.01 && abs(exponent - round(exponent)) < 1e-9) {
    return(sprintf("$10^{%d}$", as.integer(round(exponent))))
  }
  sub("[.]$", "", sub("0+$", "", sprintf("%.5f", x)))
}
hp_cell <- function(arm, name, model) {
  row <- hp_bounds[hp_bounds$arm == arm & hp_bounds$name == name &
                     hp_bounds$model == model, , drop = FALSE]
  if (nrow(row) == 0L) return("--")
  if (nrow(row) > 1L) {
    rlang::abort(paste0("Duplicate table cell for ", arm, "::", name, "::", model, "."))
  }
  values <- row$candidate_values[[1]]
  if (row$fixed[[1]]) return(paste0("Fixed: ", hp_format(values[[1]])))
  if (identical(arm, "Grid search")) {
    return(paste(vapply(values, hp_format, character(1)), collapse = ", "))
  }
  sprintf("[%s, %s]", hp_format(row$lo[[1]]), hp_format(row$hi[[1]]))
}
hp_panel_rows <- function(arm, panel) {
  names_present <- intersect(hp_row_order, unique(hp_bounds$name[hp_bounds$arm == arm]))
  c(
    paste0("\\multicolumn{6}{@{}l}{\\textit{Panel ", panel, ". ", arm, "}} ",
           intToUtf8(92), intToUtf8(92)),
    vapply(names_present, function(name) {
      paste0(name, " & ", hp_symbol[[name]], " & ",
             paste(vapply(hp_models, function(model) hp_cell(arm, name, model), character(1)),
                   collapse = " & "),
             intToUtf8(92), intToUtf8(92))
    }, character(1), USE.NAMES = FALSE)
  )
}
hp_grid_budget <- function(model) {
  rows <- hp_bounds[hp_bounds$arm == "Grid search" & hp_bounds$model == model, , drop = FALSE]
  if (nrow(rows) == 0L) return("--")
  format(prod(vapply(rows$candidate_values, length, integer(1))), trim = TRUE)
}
hp_bayes_budget <- function(field, model) {
  rows <- hp_bounds[hp_bounds$arm == "Bayesian optimization" & hp_bounds$model == model, , drop = FALSE]
  values <- unique(rows[[field]])
  if (length(values) != 1L || is.na(values)) {
    rlang::abort(paste0("Invalid Bayesian budget for ", model, "::", field, "."))
  }
  as.numeric(values)
}
hp_budget_row <- function(label, values) {
  paste0(label, " &  & ", paste(values, collapse = " & "),
         intToUtf8(92), intToUtf8(92))
}

hp_table_tex <- c(
  "\\begin{table}[p]",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  "\\renewcommand{\\arraystretch}{0.85}",
  "\\caption[Hyperparameter search spaces]{\\footnotesize \\textbf{: Hyperparameter search spaces.} The table reports the realised settings recorded in the completed grid-search baseline and Bayesian-optimization robustness runs.}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{3.0cm}>{\\centering\\arraybackslash}m{1.55cm}*{4}{>{\\raggedleft\\arraybackslash}X}}",
  "\\hline",
  paste0("Hyperparameter name & Symbol & ",
         paste(unname(hp_model_labels[hp_models]), collapse = " & "),
         " ", intToUtf8(92), intToUtf8(92)),
  "\\hline",
  hp_panel_rows("Grid search", "A"),
  "\\hline",
  hp_budget_row("Grid combinations per refit", vapply(hp_models, hp_grid_budget, character(1))),
  "\\hline",
  hp_panel_rows("Bayesian optimization", "B"),
  "\\hline",
  hp_budget_row("Initial design points", vapply(hp_models, function(m) hp_bayes_budget("init_points", m), numeric(1))),
  hp_budget_row("Acquisition iterations", vapply(hp_models, function(m) hp_bayes_budget("n_iter", m), numeric(1))),
  hp_budget_row("Total model evaluations per refit", vapply(hp_models, function(m) {
    hp_bayes_budget("init_points", m) + hp_bayes_budget("n_iter", m)
  }, numeric(1))),
  "\\hline",
  paste0("\\multicolumn{6}{@{}p{0.98\\linewidth}@{}}{\\textit{The grid-search arm is the primary specification. Its cells list all candidate values, and its final row gives the number of combinations evaluated at each refit. The Bayesian-optimization arm is a robustness check. Its non-fixed cells give continuous bounds, ``Fixed'' denotes a parameter excluded from the Gaussian-process search, and UCB is the acquisition rule. Both arms minimize validation RMSE over a 36-month expanding-window validation sample and re-optimize at each annual refit. OLS has no tuning parameters and is omitted. The ENET mixing weight is the ridge share, equal to one minus \\texttt{glmnet}'s \\texttt{alpha}; the penalty path floor is \\texttt{lambda.min.ratio}, not the fitted penalty itself.}} ",
         intToUtf8(92), intToUtf8(92)),
  "\\hline",
  "\\end{tabularx}",
  "\\label{hyperchoice}",
  "\\end{table}"
)
writeLines(hp_table_tex, here::here("tables", "hyperparameter_bounds.tex"))

# Keep the manuscript's figure assets synchronized with the generated outputs.
generated_figure_files <- c(
  "bh_eligibility_heatmap.png",
  "feature_importance_bar.png",
  "feature_importance_heatmap.png",
  "feature_importance_heatmap_gross.png",
  "composition_sector_weight.png",
  "composition_liquidity_weight.png",
  "portfolio_characteristics.png",
  "surrogate_fit.png",
  "hyperparameters_over_time.png",
  "cumulative_return.png",
  "correlogram_predictions.png",
  "correlogram_performance.png",
  "combination_frontier.png",
  "fund_picker_quantile_sensitivity.png",
  "fund_picker_basket_size.png",
  "fund_picker_precision.png"
)
latex_figure_dir <- here::here("latex_paper", "figures")
dir.create(latex_figure_dir, recursive = TRUE, showWarnings = FALSE)
copied_figures <- file.copy(
  from = here::here("figures", generated_figure_files),
  to = file.path(latex_figure_dir, generated_figure_files),
  overwrite = TRUE
)
if (!all(copied_figures)) {
  rlang::abort("Not all generated figures were copied to latex_paper/figures.")
}


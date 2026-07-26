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

data_root <- here::here("data")
result_folders <- list.dirs(data_root, full.names = FALSE, recursive = FALSE)

for (folder in result_folders) {
  qs_file <- list.files(file.path(data_root, folder), pattern = "\\.qs$", full.names = TRUE)
  assign(folder, qs::qread(qs_file, nthreads = 4))
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

funds_daily <- read_economatica_daily_returns(
  here::here("data", "bases_fundos_diarios.xlsx")
)
ibov_daily <- read_economatica_daily_returns(
  here::here("data", "ibov_diarios.xlsx")
)
cdi_daily <- read_economatica_daily_returns(
  here::here("data", "CDI_diarios.xlsx")
)

# ---- ML backtest window ----------------------------------------------------
# Established before the fund benchmarks so every daily series can be compounded
# to the exact monthly endpoints used by the ML backtest.
meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts <-
  restore_meta_xts_dates(meta_port_ind_signals$backtest_meta_portfolios_results$
                           ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts)

ml_index <- meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts@data %>%
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
  med_fund_row(reliable_funds, "MED_FUND"),
  med_fund_row(reliable_top_funds, "MED_FUND_Q75")
)

# ---- ML meta-portfolio performance, net of a fictitious management fee -----
# A 1% p.a. fee is monthlised and removed geometrically, putting the machine
# portfolios on the same net-of-fee footing as the human managers.
mgmt_fee_annual  <- 0.01
mgmt_fee_monthly <- (1 + mgmt_fee_annual)^(1 / 12) - 1

deduct_management_fee <- function(returns_pct, monthly_fee = mgmt_fee_monthly) {
  ((1 + returns_pct / 100) / (1 + monthly_fee) - 1) * 100
}

ml_returns_m_xts <- meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts@data

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
  "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{2.5cm}",
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
    "\\caption[Table 3: \\footnotesize ]{\\footnotesize \\textbf{: Out-of-sample ",
    "net active summary statistics of the ML Ensemble.} This table reports ",
    "out-of-sample net active summary statistics for the ML Ensemble portfolios and ",
    "the human-manager benchmarks. The ML Ensemble averages the return predictions ",
    "across all ML models (OLS, Elastic Net, Random Forest, XGBoost and three neural ",
    "networks) before portfolio construction; the ML rows differ only by the ",
    "portfolio-construction scheme. Annualized Active Geometric Return ",
    "(Ret.), Tracking Error (TE), Information Ratio (IR), Active ",
    "Expected Shortfall (AES), Active Ulcer Index (Ulcer), Annualized Active Martin ",
    "Ratio (MR), CAPM alpha ($\\alpha$, in percent per month) and its t-statistic ",
    "($t(\\alpha)$) are displayed. ML portfolio returns are net of transaction costs ",
    "and of a 1\\% p.a. management fee. The human-manager benchmarks come in two ",
    "forms. FUNDS\\_MEAN and FUNDS\\_Q75 apply the metrics to a single aggregated ",
    "return series --- an equally weighted portfolio of, respectively, all eligible ",
    "funds and the top-quartile eligible funds (ranked by mean return over the ",
    "evaluation window) --- ",
    "and therefore ",
    "embed the diversification of holding many funds at once. MED\\_FUND and ",
    "MED\\_FUND\\_Q75 instead compute each metric for every fund individually and then ",
    "report the median across funds, describing the experience of an investor who ",
    "picks a single (respectively, any or top-quartile) fund. Every human-manager ",
    "benchmark uses the same universe of funds with at least 36 monthly observations. ",
    "The median (rather ",
    "than the mean) is used because averaging non-linear ratios across funds is ",
    "dominated by small-denominator outliers; the reported ratios and $t(\\alpha)$ ",
    "are therefore cross-fund medians of fund-level quantities, not statistics of a ",
    "tradable portfolio.}"
  ),
  "\\footnotesize",
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
mb_net_returns_obj <- restore_meta_xts_dates(
  mb_ind_signals_result$sb_eval_backtest_results_cohort@port_returns_m_xts_list$net_returns_m_xts
)
mb_returns_m_xts <- mb_net_returns_obj@data

# The per-model backtest carries a few extra warm-up months at the start; trim
# to the ensemble window (ml_index, Dec-2015 .. May-2024) so both tables report
# the same out-of-sample period.
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
# "c__cs_eval_model_sb_glmnet_bayes_s__..._ind_signals" -> model GLMNET, scheme CS.
model_from_col  <- function(x) toupper(sub("_bayes$", "",
                     sub("^c__(?:cs|rp)_eval_model_sb_(.*)_s__.*$", "\\1", x)))
scheme_from_col <- function(x) toupper(sub("^c__(cs|rp)_.*$", "\\1", x))

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
    "\\caption[Table 4: \\footnotesize ]{\\footnotesize \\textbf{: Out-of-sample ",
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
plot_base_size <- 17

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
latex_figure <- function(file, fig_num, bold_title, body, label, fig_dir = "figures") {
  c(
    "\\begin{figure}[htbp]",
    "\\centering",
    "\\captionsetup{",
    "  justification=raggedright,",
    "  labelsep=none,",
    "  singlelinecheck=false,",
    "  labelformat=boldlabel,",
    "}",
    sprintf("\\caption[Figure %d: \\footnotesize ]{\\footnotesize \\textbf{: %s} %s}",
            fig_num, bold_title, body),
    sprintf("\\includegraphics[width=\\linewidth]{%s/%s}", fig_dir, file),
    sprintf("\\label{%s}", label),
    "\\end{figure}"
  )
}

dir.create(here::here("figures"), showWarnings = FALSE)

# ---- Feature importance across ML models -----------------------------------
# For each model in the individual-signals run, factoRverse stores a
# feature_importance_m_df: a global OLS surrogate of the model output on the
# same (standardized) signals, so `normalized_importance` is a regression
# coefficient. Because signals within a theme are highly correlated, individual
# coefficients offset each other; summing them within a theme cancels that
# collinearity artifact and yields the theme's NET contribution. We show, per
# theme, faceted by model (the grid strategy):
#   * a diverging bar of the summed net importance in the last period;
#   * a tile heatmap of the summed net importance per theme over time.
sb_list <- mb_ind_signals_result$chosen_mb_backtest_results_initial@base_sb_backtest_results_list

# Compact model label from the backtest element name:
# "c__glmnet_bayes_f__..._ind_signals" -> "GLMNET"; "c__ols_f__..." -> "OLS".
fi_model_label <- function(nm) toupper(sub("_bayes$", "", sub("_f__.*$", "", sub("^c__", "", nm))))
fi_model_order <- c("OLS", "GLMNET", "RF", "XGB", "NN1", "NN2", "NN3")

# Long feature-importance table across all models, themes only (drop the
# intercept and any untagged rows).
fi_all <- purrr::imap_dfr(sb_list, function(sbr, nm) {
  d <- sbr@feature_importance_m_df@data %>%
    dplyr::filter(!is.na(theme)) %>%
    dplyr::select(dates, theme, normalized_importance)
  d$model <- fi_model_label(nm)
  d
})
fi_all$model <- factor(fi_all$model, levels = intersect(fi_model_order, unique(fi_all$model)))

# Display relabelling of theme values (e.g. "forced" -> "industries").
fi_all$theme <- dplyr::recode(fi_all$theme, forced = "industries")

# Order themes by overall net (summed) importance so both figures read
# consistently (largest net contributors at the top).
theme_order <- fi_all %>%
  dplyr::group_by(theme) %>%
  dplyr::summarise(m = sum(normalized_importance, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(m) %>%
  dplyr::pull(theme)
fi_all$theme <- factor(fi_all$theme, levels = theme_order)

jp_fi <- factoRverse:::.journal_palette()

# Diverging bars: net (signed sum) importance per theme, last period, by model.
fi_bar <- fi_all %>%
  dplyr::group_by(model) %>%
  dplyr::filter(dates == max(dates)) %>%
  dplyr::group_by(model, theme) %>%
  dplyr::summarise(importance = sum(normalized_importance, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(sign = ifelse(importance >= 0, "Positive", "Negative"))

fi_bar_grid <- ggplot2::ggplot(fi_bar, ggplot2::aes(x = theme, y = importance, fill = sign)) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(~model, ncol = 3) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = c(Positive = jp_fi$primary, Negative = jp_fi$negative),
                             guide = "none") +
  ggplot2::labs(x = NULL, y = "Net (summed) normalized importance (last period)",
                title = "Net feature importance by theme across ML models (last period)") +
  journal_area_theme()

# Tile heatmap of net (summed) importance per theme over time, faceted by model.
fi_heat <- fi_all %>%
  dplyr::group_by(model, dates, theme) %>%
  dplyr::summarise(importance = sum(normalized_importance, na.rm = TRUE), .groups = "drop")

fi_heat_grid <- ggplot2::ggplot(fi_heat, ggplot2::aes(x = dates, y = theme, fill = importance)) +
  ggplot2::geom_tile() +
  ggplot2::facet_wrap(~model, ncol = 2) +
  ggplot2::scale_fill_gradient2(low = jp_fi$negative, mid = "#EBEEF1", high = jp_fi$primary,
                                midpoint = 0, name = "Net norm.\nimportance") +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Net feature importance by theme over time across ML models") +
  journal_area_theme() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

ggplot2::ggsave(here::here("figures", "feature_importance_bar.png"),
                fi_bar_grid, width = 14, height = 11, dpi = 300, bg = "white")
ggplot2::ggsave(here::here("figures", "feature_importance_heatmap.png"),
                fi_heat_grid, width = 14, height = 12, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the feature-importance figures ------------------
feature_importance_figures_tex <- c(
  latex_figure(
    "feature_importance_bar.png", 1,
    "Net feature importance by theme across ML models (last period).",
    paste(
      "Net (signed sum of) normalized feature importance within each signal",
      "theme in the final out-of-sample period, for every ML model (OLS, GLMNET,",
      "RF, XGB, NN1--NN3). Feature importance is a global OLS surrogate of the",
      "model output on the same signals; because signals within a theme are",
      "highly correlated their coefficients offset, so importances are summed",
      "within each theme to recover the net contribution. Bars are coloured by",
      "sign; themes are ordered by overall net importance."
    ),
    "fig:fi_bar"
  ),
  "",
  latex_figure(
    "feature_importance_heatmap.png", 2,
    "Net feature importance by theme over time across ML models.",
    paste(
      "Net (signed sum of) normalized feature importance per signal theme at",
      "each rebalance date, for every ML model (OLS, GLMNET, RF, XGB, NN1--NN3).",
      "Each panel shows one model; colour encodes net importance on a diverging",
      "scale centred at zero. Themes are ordered by overall net importance."
    ),
    "fig:fi_heatmap"
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
pbr_list <- meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_backtest_results_list

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
  pbr_list, "liquidity_classification", value_col = "weights", fill_lab = "Liquidity class",
  y_lab = "Share of portfolio weight", ncol = 3, group_levels = liquidity_levels,
  bench_value_col = "ibov_bench_weights", bench_label = "IBOV",
  title = "Weight composition by liquidity class across portfolios"
)

dir.create(here::here("figures"), showWarnings = FALSE)
ggplot2::ggsave(here::here("figures", "composition_sector_weight.png"),
                sector_grid, width = 14, height = 12, dpi = 300, bg = "white")
ggplot2::ggsave(here::here("figures", "composition_liquidity_weight.png"),
                liquidity_grid, width = 14, height = 12, dpi = 300, bg = "white")


# ---- Paste-ready LaTeX for the composition figures -------------------------
composition_figures_tex <- c(
  latex_figure(
    "composition_sector_weight.png", 3,
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
    "composition_liquidity_weight.png", 4,
    "Portfolio weight composition by market capitalization.",
    paste(
      "Out-of-sample monthly composition of portfolio weight across",
      "market-capitalization classes (mega, large, mid, small and micro caps),",
      "for each ML-ensemble portfolio-construction scheme and the IBOV benchmark.",
      "Each panel stacks the sum of portfolio weights within each size class to",
      "100\\% at every rebalance date."
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
char_metrics <- c("sharpe_12m", "ni_me", "roe_12m", "gp_at_12m", "div_me_med_36m",
                  "ret_vol_12m", "log_mkt_cap", "qmj", "fcf_be_12m_chg3y")

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
  fcf_be_12m_chg3y = "FCF/BE 3y change"
)

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
  "portfolio_characteristics.png", 5,
  "Average portfolio characteristics over time.",
  paste(
    "Weighted-average stock characteristics of each ML-ensemble portfolio",
    "(sum of end-of-period portfolio weights times the characteristic) at every",
    "rebalance date. Each panel is one characteristic: Sharpe ratio (12m),",
    "earnings yield (NI/ME), return on equity (12m), gross profitability",
    "(GP/AT), dividend yield (36m median), return volatility (12m), log market",
    "capitalization, quality minus junk (QMJ) and the three-year change in",
    "FCF/BE. Each line is one portfolio-construction scheme (RP, RP\\_BOX, HRP,",
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
# active return unexplained by the market + style basis. `signal_ports` must be
# loaded in the session (it holds the cap-scaled long-only signal portfolios).

# Daily CDI is compounded on the same 15th-to-15th endpoints as every return
# series in the attribution.
cdi <- tibble::tibble(ym = format(ml_index, "%Y-%m"), rf = cdi_ret)

# Long-only factor-mimicking portfolios (net returns, percent).
sp_returns_obj <- restore_meta_xts_dates(
  signal_ports$signal_port_backtest_cohort_ind_signals@
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
  cma = as.numeric(sp_xts[, fcol("assets_gr1y_tr")])      # investment (short high growth)
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
ff_block <- function(label) {
  r <- ff_results[[label]]
  if (is.null(r)) return(sprintf("%s & \\multicolumn{8}{c}{--} \\\\", gsub("_", "\\\\_", label)))
  c(ff_est_row(label, r), ff_t_row(r))
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
    "\\caption[Table 5: \\footnotesize ]{\\footnotesize \\textbf{: Long-only factor ",
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
    "fit. MED\\_FUND and ",
    "MED\\_FUND\\_Q75 regress each fund individually and report the median loading ",
    "across (all / top-quartile) funds.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  paste0(
    "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{1.6cm}",
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
  ff_block("MED_FUND"),
  ff_block("MED_FUND_Q75"),
  "\\hline",
  "\\end{tabularx}",
  "\\label{factorattr}",
  "\\end{table}"
)

writeLines(factor_table_tex, here::here("tables", "factor_attribution.tex"))

# ---- Hierarchical (base-learner -> meta-learner) feature importance --------
# factoRverse's built-in "Hierarchical Feature Importance" plot for a
# sb_metabacktest_results object draws the signals -> base learners -> meta
# learner importance tree (via igraph/ggraph). Requires the ggraph/igraph/
# ggrepel plotting packages. The method ends in print(p) and returns the object
# invisibly, so we render it into a null device, recover the ggplot with
# last_plot(), replace its hard-coded size-2 node labels with repelled labels,
# override its enormous backtest-id subtitle, and save that.
hfi_top_n    <- 10L
hfi_subtitle <- "ML Ensemble"   # set to NULL to remove the subtitle entirely

grDevices::pdf(NULL)
plot(mb_ind_signals_result$chosen_mb_backtest_results_initial,
     plot_id = "Hierarchical Feature Importance",
     top_n = hfi_top_n, palette = "journal")
grDevices::dev.off()

hfi_plot <- ggplot2::last_plot()
is_node_text <- vapply(
  hfi_plot$layers,
  function(layer) inherits(layer$geom, "GeomText"),
  logical(1)
)
hfi_plot$layers <- hfi_plot$layers[!is_node_text]
jp_hfi <- factoRverse:::.journal_palette()
hfi_plot <- hfi_plot +
  ggrepel::geom_text_repel(
    data = hfi_plot$data,
    ggplot2::aes(x = x, y = y, label = name),
    inherit.aes = FALSE,
    color = jp_hfi$text,
    size = 3.6,
    fontface = "bold",
    seed = 20260726,
    box.padding = 0.25,
    point.padding = 0.2,
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  ggplot2::labs(subtitle = hfi_subtitle) +
  ggplot2::theme(
    text = ggplot2::element_text(size = plot_base_size, color = jp_hfi$text),
    plot.title = ggplot2::element_text(size = plot_base_size + 2, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = plot_base_size, face = "italic"),
    legend.title = ggplot2::element_text(size = plot_base_size),
    legend.text = ggplot2::element_text(size = plot_base_size - 1)
  )
ggplot2::ggsave(here::here("figures", "hierarchical_feature_importance.png"),
                hfi_plot, width = 14, height = 10, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the hierarchical feature-importance figure ------
hierarchical_fi_figure_tex <- latex_figure(
  "hierarchical_feature_importance.png", 6,
  "Hierarchical feature-importance decomposition.",
  paste(
    "Decomposition of feature importance from the individual signals through the",
    "base learners (OLS, GLMNET, RF, XGB, NN1--NN3) to the meta learner (the ML",
    "Ensemble). Nodes are signals, base learners and the meta learner; edge width",
    "and colour encode the importance each signal contributes to a base learner,",
    "and each base learner to the meta learner. Only the",
    sprintf("%d most important signals per learner are shown.", hfi_top_n),
    "Estimated on the individual-signals run."
  ),
  "fig:hier_fi"
)

writeLines(hierarchical_fi_figure_tex,
           here::here("figures", "hierarchical_feature_importance_figure.tex"))

# ---- Global surrogate fit: how linear each ML model's predictions are -------
# final_gsm is the fitted OLS global surrogate (an `lm`) regressing each model's
# out-of-sample predictions on the [-1, 1] cross-sectionally scaled signals.
# Its R^2 is the share of prediction variance explained linearly; 1 - R^2 and
# the residual variance measure the nonlinearity/interactions the linear
# surrogate cannot capture. Only the final refit's surrogate is stored, so this
# is an end-of-sample snapshot per model (not a time series).
jp_gsm <- factoRverse:::.journal_palette()

gsm_stats <- purrr::imap_dfr(sb_list, function(sbr, nm) {
  g <- sbr@final_gsm
  if (!inherits(g, "lm")) return(NULL)          # GSM must be the OLS (lm) surrogate
  tibble::tibble(
    model     = fi_model_label(nm),
    intercept = stats::coef(g)[["(Intercept)"]],  # ~ mean predicted active return
    r2        = summary(g)$r.squared,
    resid_var = stats::var(stats::residuals(g))
  )
})
gsm_stats$model <- factor(gsm_stats$model,
                          levels = intersect(fi_model_order, gsm_stats$model))

gsm_long <- gsm_stats %>%
  tidyr::pivot_longer(c(intercept, r2, resid_var), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = factor(
    dplyr::recode(metric,
                  intercept = "Surrogate intercept",
                  r2        = "Surrogate R²",
                  resid_var = "Residual variance"),
    levels = c("Surrogate intercept", "Surrogate R²", "Residual variance")
  ))

gsm_plot <- ggplot2::ggplot(gsm_long, ggplot2::aes(x = model, y = value, fill = model)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::facet_wrap(~metric, ncol = 3, scales = "free_y") +
  ggplot2::scale_fill_manual(
    values = rep(jp_gsm$categorical, length.out = nlevels(gsm_stats$model)),
    guide = "none"
  ) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Linear explainability of each ML model's predictions (final refit)") +
  journal_area_theme()

ggplot2::ggsave(here::here("figures", "surrogate_fit.png"),
                gsm_plot, width = 15, height = 6, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the surrogate-fit figure ------------------------
surrogate_fit_figure_tex <- latex_figure(
  "surrogate_fit.png", 7,
  "Linear explainability of each ML model's predictions.",
  paste(
    "For each ML model (OLS, GLMNET, RF, XGB, NN1--NN3), the out-of-sample",
    "predictions are regressed on the signals (each cross-sectionally scaled to",
    "$[-1, 1]$ that month, hence approximately mean-zero) via the OLS global",
    "surrogate model. \\emph{Left (intercept):} because the scaled signals are",
    "centred, the intercept is approximately the mean of the model's predictions",
    "-- its baseline predicted active return for a characteristic-neutral (median)",
    "stock. It captures the unconditional level of predicted active returns and",
    "also absorbs any level shift induced by the model's curvature (since it",
    "equals the average prediction, $\\mathrm{E}[f(x)]$, not the prediction at the",
    "average, $f(\\mathrm{E}[x])$); it is a common shift across stocks and does not",
    "affect the cross-sectional ranking. \\emph{Centre (}$R^2$\\emph{):} the share",
    "of prediction variance that is linear in the signals -- values near one",
    "indicate an essentially linear model. \\emph{Right (residual variance):} the",
    "variance of the surrogate residuals, i.e. the part of the predictions the",
    "linear surrogate cannot reproduce. Together, $1 - R^2$ and the residual",
    "variance measure the nonlinearity and interaction structure the ML model",
    "captures beyond a linear function of the signals; both are near zero for the",
    "linear models (OLS, GLMNET) and larger for the tree and neural-network",
    "models. Values are for the final-refit surrogate on the individual-signals run."
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
                title = "Selected hyperparameters over time, by model") +
  journal_area_theme()

hyper_height <- max(6, ceiling(length(panel_levels) / hyper_ncol) * 3)
ggplot2::ggsave(here::here("figures", "hyperparameters_over_time.png"),
                hyper_plot, width = 16, height = hyper_height,
                dpi = 300, bg = "white", limitsize = FALSE)

# ---- Paste-ready LaTeX for the hyperparameter-over-time figure -------------
hyperparameters_figure_tex <- latex_figure(
  "hyperparameters_over_time.png", 8,
  "Selected hyperparameters over time.",
  paste(
    "Bayesian-tuned hyperparameter values chosen at each walk-forward refit, for",
    "every tuned ML model (GLMNET, RF, XGB, NN1--NN3; OLS is untuned and omitted).",
    "Hyperparameters that play the same role across models share a panel and are",
    "drawn as separate coloured lines: number of trees (RF num.trees, XGB nrounds),",
    "maximum tree depth, minimum leaf size (RF min.bucket, XGB min\\_child\\_weight),",
    "learning rate (XGB eta, NN lr), early-stopping iteration and feature",
    "subsampling (RF mtry, XGB colsample\\_bytree; both fractions of the features",
    "sampled). Model-specific hyperparameters keep their own panels, and the three",
    "neural networks share each remaining NN panel. Points",
    "are the selected values at each refit date; vertical scales differ across",
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
  "cumulative_return.png", 9,
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

correlogram_plot <- function(long_df, title, ncol = 1L, text_size = 4) {
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
preds_df <- mb_ind_signals_result$chosen_mb_backtest_results_initial@
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
  text_size = 4.1
)
ggplot2::ggsave(here::here("figures", "correlogram_performance.png"),
                perf_cor_plot, width = 12, height = 10.5, dpi = 300, bg = "white")

# ---- Paste-ready LaTeX for the correlogram figures -------------------------
correlogram_figures_tex <- c(
  latex_figure(
    "correlogram_predictions.png", 10,
    "Agreement across base-learner predictions.",
    paste(
      "Pairwise correlation of the out-of-sample return predictions of the seven",
      "base learners (OLS, GLMNET, RF, XGB, NN1--NN3). Correlations are computed",
      "cross-sectionally within each month and then averaged over months, so they",
      "measure whether the models rank the same stocks alike rather than sharing a",
      "common time-series level. Lower correlations indicate more independent",
      "signals and hence more scope for the ensemble to add value."
    ),
    "fig:corr_pred"
  ),
  "",
  latex_figure(
    "correlogram_performance.png", 11,
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
  # median() over a single-column case returns that column's own value.
  tibble::tibble(
    sharpe      = stats::median(colMeans(xe, na.rm = TRUE) / sd_e * sqrt(12), na.rm = TRUE),
    ir          = stats::median(act_ret / act_risk, na.rm = TRUE),
    active_ret  = stats::median(act_ret, na.rm = TRUE),
    active_risk = stats::median(act_risk, na.rm = TRUE)
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
pure_human_pts <- combo_df %>% dplyr::filter(near0(w)) %>%
  dplyr::distinct(human, active_risk, active_ret)
max_ir_pts <- combo_df %>%
  dplyr::group_by(scheme, human) %>%
  dplyr::slice_max(ir, n = 1, with_ties = FALSE) %>%
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
  ggplot2::geom_point(data = pure_human_pts, ggplot2::aes(x = active_risk, y = active_ret),
                      inherit.aes = FALSE, shape = 15, size = 3.2, color = jp_comb$text) +
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
combo_table_body <- sprintf(
  "%s & %s & %.2f & %.2f & %.2f & %.0f\\%% & %.2f & %.2f & %.2f & %.0f\\%% \\\\",
  gsub("_", "\\\\_", as.character(combo_summary$scheme)),
  gsub("_", "\\\\_", as.character(combo_summary$human)),
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
  "\\renewcommand{\\arraystretch}{1.25}",
  paste0(
    "\\caption[Table 6: \\footnotesize ]{\\footnotesize \\textbf{: Man-and-machine ",
    "combination frontier.} Each row combines one ML Ensemble portfolio with one ",
    "human-manager benchmark, rebalanced monthly to constant weights $w$ in the ML ",
    "portfolio and $1-w$ in the manager, over a grid $w \\in \\{0, 0.05, \\ldots, 1\\}$. ",
    "Because the two-asset weight space is one-dimensional, the frontier is traced ",
    "with a deterministic grid rather than random draws. Sharpe ratios are ",
    "annualized and computed on returns in excess of the CDI; Information Ratios ",
    "are annualized and computed on returns active against IBOV. For each metric ",
    "the table reports the pure-manager value ($w = 0$), the pure-ML value ",
    "($w = 1$), the best value attained on the grid, and the weight $w^{*}$ that ",
    "attains it. An interior $w^{*}$ indicates a diversification gain from ",
    "combining machine and human portfolios. Four manager archetypes are ",
    "considered: a portfolio diversified across all eligible funds (FUNDS\\_MEAN) or across ",
    "top-quartile funds (FUNDS\\_Q75), and a single fund drawn either from the eligible ",
    "universe (MED\\_FUND) or from the top quartile (MED\\_FUND\\_Q75). For the ",
    "single-fund rows, metrics are computed for the combination with ",
    "each fund individually and the median across funds is reported; averaging the ",
    "combined returns across funds would instead collapse exactly to the ",
    "FUNDS\\_MEAN combination. When a fund has closed or has not yet launched, that ",
    "month is held entirely in the ML portfolio, so each combination is a ",
    "continuously invested path.}"
  ),
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  paste0(
    "\\begin{tabularx}{\\linewidth}{>{\\raggedright\\arraybackslash}m{1.4cm}",
    ">{\\raggedright\\arraybackslash}m{2.4cm}*{8}{>{\\raggedleft\\arraybackslash}X}}"
  ),
  "\\hline",
  paste("& & \\multicolumn{4}{c}{Sharpe ratio} &",
        "\\multicolumn{4}{c}{Information ratio} \\\\"),
  paste("Portfolio & Manager & Human & ML & Best & $w^{*}$ &",
        "Human & ML & Best & $w^{*}$ \\\\"),
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
  "combination_frontier.png", 12,
  "Man and machine: active risk-return frontier.",
  paste(
    "Annualized active return and risk for monthly rebalanced combinations holding",
    "$w$ in an ML Ensemble portfolio and $1-w$ in a human-manager benchmark.",
    "FUNDS\\_MEAN and FUNDS\\_Q75 are diversified fund portfolios; MED\\_FUND and",
    "MED\\_FUND\\_Q75 report the median result across individual-fund combinations.",
    "All returns are measured 15th-to-15th against IBOV. Squares, triangles and",
    "diamonds mark the pure manager, pure ML portfolio and maximum-IR combination."
  ),
  "fig:combo_frontier"
)

writeLines(combination_figure_tex,
           here::here("figures", "combination_frontier_figure.tex"))

# Keep the manuscript's figure assets synchronized with the generated outputs.
generated_figure_files <- c(
  "feature_importance_bar.png",
  "feature_importance_heatmap.png",
  "composition_sector_weight.png",
  "composition_liquidity_weight.png",
  "portfolio_characteristics.png",
  "hierarchical_feature_importance.png",
  "surrogate_fit.png",
  "hyperparameters_over_time.png",
  "cumulative_return.png",
  "correlogram_predictions.png",
  "correlogram_performance.png",
  "combination_frontier.png"
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


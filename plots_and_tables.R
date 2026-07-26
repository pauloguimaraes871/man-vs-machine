#Load datasets
library("dplyr")
if (!requireNamespace("qs", quietly = TRUE)) {
  # qs was archived from CRAN on 2026-01-17; install the last binary from a
  # pre-archival Posit Package Manager snapshot instead of the "latest" repo.
  install.packages(
    "qs",
    repos = "https://packagemanager.posit.co/cran/2026-01-10"
  )
  renv::record("qs@0.27.3")
}

data_root <- here::here("data")
result_folders <- list.dirs(data_root, full.names = FALSE, recursive = FALSE)

for (folder in result_folders) {
  qs_file <- list.files(file.path(data_root, folder), pattern = "\\.qs$", full.names = TRUE)
  assign(folder, qs::qread(qs_file, nthreads = 4))
}

# ---- Human-manager fund benchmarks ----------------------------------------
# Source files: sheet "C0001355384", header on row 4 (skip = 3), monthly
# returns in PERCENT, missing values encoded as "-". Columns: date + funds.
funds_raw <- readxl::read_excel(here::here("data", "bases_fundos.xlsx"),
                                sheet = "C0001355384", skip = 3, na = "-")
ibov_raw  <- readxl::read_excel(here::here("data", "ibov.xlsx"),
                                sheet = "C0001355384", skip = 3, na = "-")

# Dates arrive as Portuguese "Mon-YYYY" labels (e.g. "Nov-2015", "Dez-2015").
# Parse them explicitly so the result does not depend on the session locale.
pt_months <- c(Jan = 1, Fev = 2, Mar = 3, Abr = 4, Mai = 5, Jun = 6,
               Jul = 7, Ago = 8, Set = 9, Out = 10, Nov = 11, Dez = 12)

parse_pt_month <- function(x) {
  parts <- strsplit(as.character(x), "-", fixed = TRUE)
  mon   <- vapply(parts, `[`, character(1), 1)
  year  <- vapply(parts, `[`, character(1), 2)
  out   <- as.Date(sprintf("%s-%02d-01", year, pt_months[mon]))
  stopifnot(!anyNA(out))
  out
}

dates    <- parse_pt_month(funds_raw[[1]])
fund_ret <- as.matrix(funds_raw[, -1])            # months x funds, % returns, NA = missing

# Benchmark 1 - cross-sectional average return per period (na.rm gives the
# survivorship correction: dead funds contribute only while they existed).
funds_mean <- rowMeans(fund_ret, na.rm = TRUE)

# Benchmark 2 - Best-in-Class: rank funds by full-sample mean return, keep the
# top quartile, then take their cross-sectional average per period.
fund_avg  <- colMeans(fund_ret, na.rm = TRUE)
q75       <- stats::quantile(fund_avg, 0.75, na.rm = TRUE)
top_cols  <- !is.na(fund_avg) & fund_avg >= q75
funds_q75 <- rowMeans(fund_ret[, top_cols, drop = FALSE], na.rm = TRUE)

# Active (benchmark-relative) monthly returns vs IBOV, joined on date.
ibov_ret <- ibov_raw[[2]][match(dates, parse_pt_month(ibov_raw[[1]]))]

funds_benchmarks <- tibble::tibble(
  date              = dates,
  funds_mean        = funds_mean,
  funds_q75         = funds_q75,
  ibov              = ibov_ret,
  funds_mean_active = funds_mean - ibov_ret,
  funds_q75_active  = funds_q75  - ibov_ret
)

# ---- Active performance of the human-manager benchmarks -------------------
# Restrict the benchmarks to the ML backtest window and reuse its exact index:
# summarize_performance() consumes the market proxy positionally, so both xts
# objects must share the same index, in the same order.
meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts <- 
  restore_meta_xts_dates(meta_port_ind_signals$backtest_meta_portfolios_results$
                           ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts)

ml_index <- meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts@data %>%
  zoo::index()

keep <- match(format(ml_index, "%Y-%m"), format(funds_benchmarks$date, "%Y-%m"))
stopifnot(!anyNA(keep))
funds_window <- funds_benchmarks[keep, ]

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

# ---- ML meta-portfolio performance, net of a fictitious management fee -----
# A 1% p.a. fee is monthlised and removed geometrically, putting the machine
# portfolios on the same net-of-fee footing as the human managers.
mgmt_fee_annual  <- 0.01
mgmt_fee_monthly <- (1 + mgmt_fee_annual)^(1 / 12) - 1

ml_returns_m_xts <- meta_port_ind_signals$backtest_meta_portfolios_results$
  ibov_port_backtest_cohort@port_returns_m_xts_list$net_returns_m_xts@data

# The benchmark column carries no fee; every other column is a portfolio.
bench_col <- "selected_bench_return"
port_cols <- setdiff(colnames(ml_returns_m_xts), bench_col)

# Returns are in PERCENT: rescale to decimals, remove the fee, rescale back.
ml_ports_net_m_xts <- ((1 + ml_returns_m_xts[, port_cols] / 100) /
                         (1 + mgmt_fee_monthly) - 1) * 100
ml_bench_m_xts <- ml_returns_m_xts[, bench_col]

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
      pain      = act_pain / 100,
      mr        = act_ann_martin_ratio,
      alpha     = alpha,                     # percent per month
      alpha_t   = alpha_t_stat
    )
}

tex_metric_rows <- function(tbl) {
  sprintf(
    "%s & %.3f & %.3f & %.2f & %.3f & %.3f & %.2f & %.3f & %.2f \\\\",
    gsub("_", "\\\\_", tbl$portfolio),
    tbl$ann_ret, tbl$te, tbl$ir, tbl$aes, tbl$pain, tbl$mr,
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
  "Portfolio & Ann. Act. Geom. Ret. & TE & IR & AES & Pain & MR &",
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

active_table <- dplyr::bind_rows(ml_rows, fund_rows)   # kept for console inspection

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
    "(Ann. Act. Geom. Ret.), Tracking Error (TE), Information Ratio (IR), Active ",
    "Expected Shortfall (AES), Active Pain Index (Pain), Annualized Active Martin ",
    "Ratio (MR), CAPM alpha ($\\alpha$, in percent per month) and its t-statistic ",
    "($t(\\alpha)$) are displayed. ML portfolio returns are net of transaction costs ",
    "and of a 1\\% p.a. management fee.}"
  ),
  "\\footnotesize",
  active_col_preamble,
  "\\hline",
  active_col_header,
  "\\hline",
  tex_panel_header("ML Ensemble --- average across all ML models"),
  tex_metric_rows(ml_rows),
  "\\hline",
  tex_panel_header("Benchmarks"),
  tex_metric_rows(fund_rows),
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
mb_ports_net_m_xts <- ((1 + mb_returns_m_xts[, mb_port_cols] / 100) /
                         (1 + mgmt_fee_monthly) - 1) * 100
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
    "Geometric Return (Ann. Act. Geom. Ret.), Tracking Error (TE), Information ",
    "Ratio (IR), Active Expected Shortfall (AES), Active Pain Index (Pain), ",
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
                           numeric_bins = 4L, data = NULL) {
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

# Shared journal-theme styling for the composition area charts.
journal_area_theme <- function(base_size = 11) {
  jp <- factoRverse:::.journal_palette()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = jp$background, color = NA),
      panel.background = ggplot2::element_rect(fill = jp$background, color = NA),
      panel.grid.major = ggplot2::element_line(color = jp$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text    = ggplot2::element_text(color = jp$text),
      axis.title   = ggplot2::element_text(color = jp$text),
      plot.title   = ggplot2::element_text(color = jp$text, face = "bold"),
      strip.text   = ggplot2::element_text(color = jp$text, face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = jp$text),
      legend.text  = ggplot2::element_text(color = jp$text)
    )
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
                                  bench_value_col = NULL, bench_label = "IBOV") {
  jp <- factoRverse:::.journal_palette()

  comp_all <- purrr::imap_dfr(pbr_list, function(pbr, nm) {
    d <- composition_df(pbr, variable, value_col, numeric_bins)
    d$portfolio <- pretty_meta_label(nm)
    d
  })
  port_levels <- pretty_meta_label(names(pbr_list))

  # Benchmark panel: same grouping, but weighted by the benchmark's own weights.
  if (!is.null(bench_value_col)) {
    b <- composition_df(pbr_list[[1]], variable, bench_value_col, numeric_bins)
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
    journal_area_theme(base_size = 10)
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
                sector_grid, width = 11, height = 12, dpi = 300, bg = "white")
ggplot2::ggsave(here::here("figures", "composition_liquidity_weight.png"),
                liquidity_grid, width = 11, height = 12, dpi = 300, bg = "white")


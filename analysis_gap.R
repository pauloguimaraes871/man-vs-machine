# =============================================================================
# analysis_gap.R
#
# Estimation and descriptive output for the human-machine alpha gap.
# Reads data/derived/gap_panel.qs (built by build_gap_panel.R) and writes
# figures to figures/ and LaTeX tables to tables/.
#
# Nothing here modifies the panel. Run build_gap_panel.R first.
#
# SPECIFICATIONS (DESIGN_NOTE.md S7)
#   Spec 1  headline. NO time fixed effects, two-way clustered SEs (fund and
#           period). The machine stays in the DV and market-state main effects
#           are identified off the ~59 usable periods.
#   Spec 2  robustness. Time FE. State main effects drop out BY CONSTRUCTION --
#           time FE absorb ra_ml(t) entirely -- but fund-trait coefficients
#           survive and are immune to any common shock. Stability across the two
#           is the result worth reporting.
#
# Group D variables (decay_slope, n_pos_min, rho_machine, hhi_top15) are
# MEDIATORS and are deliberately absent from the main right-hand side. They get
# their own descriptive section.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(ggplot2); library(scales); library(here); library(rlang)
})

PANEL    <- here::here("data", "derived", "gap_panel.qs")
INDUSTRY <- here::here("data", "derived", "industry_panel.qs")
FIG_DIR  <- here::here("figures")
TAB_DIR  <- here::here("tables")
dir.create(FIG_DIR, showWarnings = FALSE); dir.create(TAB_DIR, showWarnings = FALSE)

panel    <- qs::qread(PANEL)
industry <- qs::qread(INDUSTRY)

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", size = 12),
                  plot.subtitle = element_text(colour = "grey35", size = 9)))


# =============================================================================
# 1. Estimation sample, built as an explicit waterfall
# =============================================================================
# Every drop is named and counted. The waterfall IS a table in the paper: going
# from 1,938 tracked funds to a few hundred estimable ones is a fact about
# Brazilian fund data, not a detail to bury in a footnote.

waterfall <- tibble::tibble(
  step = character(), rows = integer(), funds = integer(), periods = integer()
)
add_step <- function(df, label, wf) {
  dplyr::bind_rows(wf, tibble::tibble(
    step = label, rows = nrow(df),
    funds = dplyr::n_distinct(df$code), periods = dplyr::n_distinct(df$period)
  ))
}

s0 <- panel;                                                waterfall <- add_step(s0, "All fund-periods", waterfall)
s1 <- s0 %>% dplyr::filter(!is.na(gap));                    waterfall <- add_step(s1, "Dependent variable available", waterfall)
s2 <- s1 %>% dplyr::filter(!is_feeder, !is_fof);            waterfall <- add_step(s2, "Drop feeders and funds-of-funds", waterfall)
s3 <- s2 %>% dplyr::filter(is_representative);              waterfall <- add_step(s3, "One fund per duplicate cluster", waterfall)
s4 <- s3 %>% dplyr::filter(!is.na(aum), period >= inception); waterfall <- add_step(s4, "Reporting and post-inception", waterfall)
s5 <- s4 %>% dplyr::filter(!is.na(expense_ratio), !is.na(redemption_days),
                           !is.na(log_pl_gestora));         waterfall <- add_step(s5, "Cost and scale covariates", waterfall)

# Small dependency-free replacement for forcats::fct_lump.
lump_levels <- function(x, n = 6L, other = "Other") {
  keep <- names(sort(table(x), decreasing = TRUE))[seq_len(min(n, dplyr::n_distinct(x)))]
  factor(ifelse(x %in% keep, x, other))
}

est <- s5 %>%
  dplyr::mutate(
    period_f = factor(period),
    log_age  = log1p(pmax(fund_age_yrs, 0)),
    perf_fee = as.integer(perf_fee),
    bank     = as.integer(bank_affiliated),
    efpc     = as.integer(efpc),
    retail   = as.integer(investor_type == "Geral"),
    anbima_f = lump_levels(anbima_class)
  )

readr_write <- function(tbl, file) {
  utils::write.csv(tbl, file.path(TAB_DIR, file), row.names = FALSE)
}
readr_write(waterfall, "sample_waterfall.csv")
print(waterfall)


# =============================================================================
# 2. Clustered inference (implemented here: no sandwich/fixest available)
# =============================================================================
# Cameron-Gelbach-Miller two-way clustering:
#     V = V_fund + V_period - V_(fund x period)
# Each component is the usual cluster-robust sandwich. With ~59 period clusters
# the time dimension is at the low end of what CRVE handles well, so period-level
# inference should be read as indicative rather than exact.

cluster_meat <- function(X, u, g) {
  g <- as.character(g)
  idx <- split(seq_along(g), g)
  k <- ncol(X)
  meat <- matrix(0, k, k)
  for (rows in idx) {
    xu <- crossprod(X[rows, , drop = FALSE], u[rows])   # k x 1
    meat <- meat + tcrossprod(xu)
  }
  G <- length(idx)
  meat * (G / (G - 1))
}

vcov_twoway <- function(fit, c1, c2) {
  X <- stats::model.matrix(fit)
  u <- stats::residuals(fit)
  keep <- stats::complete.cases(X) & is.finite(u)
  X <- X[keep, , drop = FALSE]; u <- u[keep]
  c1 <- c1[keep]; c2 <- c2[keep]

  bread <- chol2inv(chol(crossprod(X)))
  n <- nrow(X); k <- ncol(X)
  fin <- (n - 1) / (n - k)

  v1  <- bread %*% cluster_meat(X, u, c1) %*% bread
  v2  <- bread %*% cluster_meat(X, u, c2) %*% bread
  v12 <- bread %*% cluster_meat(X, u, paste(c1, c2, sep = "_")) %*% bread

  V <- (v1 + v2 - v12) * fin
  # Two-way CRVE is not guaranteed positive semi-definite in finite samples.
  ev <- eigen(V, symmetric = TRUE)
  if (any(ev$values < 0)) {
    rlang::warn("Two-way vcov was not PSD; negative eigenvalues truncated to zero.")
    V <- ev$vectors %*% diag(pmax(ev$values, 0)) %*% t(ev$vectors)
  }
  # `%*%` drops dimnames; without them every downstream name lookup silently
  # matches nothing and the coefficient table comes back empty.
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

tidy_clustered <- function(fit, c1, c2, keep_pattern = NULL) {
  V  <- vcov_twoway(fit, c1, c2)
  b  <- stats::coef(fit)
  b  <- b[names(b) %in% colnames(V)]
  se <- sqrt(diag(V))[names(b)]
  out <- tibble::tibble(term = names(b), estimate = as.numeric(b),
                        std_error = as.numeric(se)) %>%
    dplyr::mutate(t = estimate / std_error,
                  p = 2 * stats::pnorm(-abs(t)))
  if (!is.null(keep_pattern)) out <- dplyr::filter(out, grepl(keep_pattern, term))
  out
}


# =============================================================================
# 3. Regressions
# =============================================================================
# DV: gap = ra_ml - ra_fund, in percent per month. POSITIVE = machine ahead.
#
# NOTE ON SIZE (H2/H3): log_pl_fundo is endogenous to past performance
# (Berk-Green: flows chase alpha). These OLS coefficients are descriptive
# associations. A causal reading needs Pastor-Stambaugh-Taylor recursive
# demeaning or an instrument, and the paper must say so.

rhs_core <- c("log_pl_fundo", "log_pl_gestora", "redemption_days",
              "expense_ratio", "perf_fee", "log_age", "bank", "retail", "efpc")

f1 <- stats::as.formula(paste("gap ~", paste(rhs_core, collapse = " + ")))
f2 <- stats::as.formula(paste("gap ~", paste(c(rhs_core, "period_f"), collapse = " + ")))

m1 <- stats::lm(f1, data = est)
m2 <- stats::lm(f2, data = est)

t1 <- tidy_clustered(m1, est$code, est$period)
# Time-FE coefficients are nuisance parameters; drop them from the report.
t2 <- tidy_clustered(m2, est$code, est$period) %>%
  dplyr::filter(!grepl("^period_f", term))

reg_out <- dplyr::full_join(
  dplyr::select(t1, term, est_1 = estimate, se_1 = std_error, t_1 = t),
  dplyr::select(t2, term, est_2 = estimate, se_2 = std_error, t_2 = t),
  by = "term"
)
readr_write(reg_out, "gap_regressions.csv")
cat("\n=== Spec 1 (no time FE) / Spec 2 (time FE), two-way clustered ===\n")
print(as.data.frame(reg_out), digits = 3)


# =============================================================================
# 4. H7 -- the construction ladder
# =============================================================================
# Does the deficit depend on WHICH systematic scheme is the benchmark? If the
# gap against a simple equal-weight scheme matches the gap against MVO-box, the
# story is systematisation, not optimisation sophistication.

# Only the ML side changes across schemes, so the expensive fund-side rolling
# regression runs once and the eight ML series are cheap.
build_ladder <- function() {
  old <- options(gap_panel.run = FALSE)
  on.exit(options(old), add = TRUE)
  source(here::here("build_gap_panel.R"), local = TRUE)

  ml_index  <- read_ml_index()
  daily     <- read_all_daily()
  codes     <- reconcile_codes(daily)
  cdi_daily <- read_economatica_daily("CDI_diarios.xlsx")

  fund_m  <- compound_daily_to_windows(daily$ret, ml_index, codes)
  fac     <- build_factor_mat(ml_index, cdi_daily)
  ym      <- format(ml_index, "%Y-%m")
  fund_ra <- rolling_risk_adjusted(fund_m, ym, fac)$ra

  ml_raw <- read_ml_returns()
  keep   <- est %>% dplyr::distinct(code, period)

  purrr::map_dfr(c(ML_SCHEMES, "average"), function(s) {
    res   <- resolve_ml_benchmark(ml_raw, s)
    ml_ra <- rolling_risk_adjusted(res$mat[, res$col, drop = FALSE], ym, fac)$ra

    tibble::as_tibble(fund_ra) %>%
      dplyr::mutate(period = ml_index) %>%
      tidyr::pivot_longer(-period, names_to = "code", values_to = "ra_fund") %>%
      dplyr::left_join(tibble::tibble(period = ml_index,
                                      ra_ml = as.numeric(ml_ra[, 1L])), by = "period") %>%
      dplyr::inner_join(keep, by = c("code", "period")) %>%
      dplyr::filter(!is.na(ra_fund), !is.na(ra_ml)) %>%
      dplyr::summarise(
        scheme    = s,
        n         = dplyr::n(),
        mean_gap  = mean(ra_ml - ra_fund),
        se_gap    = stats::sd(ra_ml - ra_fund) / sqrt(dplyr::n_distinct(period)),
        pct_ahead = mean(ra_ml > ra_fund)
      )
  })
}

ladder <- tryCatch(build_ladder(), error = function(e) {
  rlang::warn(paste("H7 ladder failed:", conditionMessage(e))); NULL
})
if (!is.null(ladder)) {
  ladder <- dplyr::mutate(ladder, t = mean_gap / se_gap)
  cat("\n=== H7: gap by ML construction scheme ===\n")
  print(as.data.frame(ladder), digits = 3)
  readr_write(ladder, "gap_by_scheme.csv")

  p_ladder <- ggplot(ladder, aes(stats::reorder(scheme, mean_gap), mean_gap)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_pointrange(aes(ymin = mean_gap - 1.96 * se_gap,
                        ymax = mean_gap + 1.96 * se_gap),
                    colour = "#1b6ca8") +
    coord_flip() +
    labs(title = "Does the human deficit depend on which systematic scheme is the benchmark?",
         subtitle = "Mean gap with 95% intervals. Flat across schemes = the story is systematisation, not optimiser sophistication",
         x = NULL, y = "Mean gap (pp per month)")
  ggsave(file.path(FIG_DIR, "gap_by_scheme.png"), p_ladder,
         width = 9, height = 4, dpi = 300, bg = "white")
}


# =============================================================================
# 5. Industry descriptives
# =============================================================================

# --- 5.1 Industry size: raw vs balanced, with coverage -----------------------
# The two lines answer different questions. Raw AUM rises partly because
# Economatica covers more funds over time (n_reporting nearly doubles); the
# balanced series holds the fund set fixed, so its shape is size, not coverage.
ind_long <- industry %>%
  dplyr::select(period, Raw = industry_pl, Balanced = industry_pl_balanced) %>%
  tidyr::pivot_longer(-period, names_to = "series", values_to = "pl") %>%
  dplyr::filter(!is.na(pl))

p_size <- ggplot(ind_long, aes(period, pl / 1e6, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c(Raw = "grey55", Balanced = "#1b6ca8")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Brazilian equity fund industry: assets under management",
       subtitle = "Raw sums every reporting fund; balanced holds the fund set fixed, so it is coverage-neutral",
       x = NULL, y = "R$ billions", colour = NULL)
ggsave(file.path(FIG_DIR, "industry_size.png"), p_size, width = 9, height = 4.5, dpi = 300, bg = "white")

p_cov <- ggplot(industry, aes(period, n_reporting)) +
  geom_line(colour = "grey30", linewidth = 0.7) +
  labs(title = "Funds reporting AUM each period",
       subtitle = "Coverage grows ~97% over the window: a confound for any raw industry-size measure",
       x = NULL, y = "Funds")
ggsave(file.path(FIG_DIR, "industry_coverage.png"), p_cov, width = 9, height = 3.5, dpi = 300, bg = "white")

# --- 5.2 Flows --------------------------------------------------------------
p_flow <- ggplot(industry, aes(period, industry_net_flow / 1e3)) +
  geom_col(aes(fill = industry_net_flow > 0), width = 22, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = "#2a9d5c", `FALSE` = "#c0392b")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Net flows into Brazilian equity funds",
       subtitle = "Aggregated across all reporting funds, per 15th-to-15th window",
       x = NULL, y = "R$ millions")
ggsave(file.path(FIG_DIR, "industry_flows.png"), p_flow, width = 9, height = 4, dpi = 300, bg = "white")

# --- 5.3 Entry and exit -----------------------------------------------------
# Survivorship made visible: 756 of 1,945 funds in the cadastral file are
# already cancelled, so a live-funds-only study would be badly biased.
births <- panel %>%
  dplyr::distinct(code, inception, status) %>%
  dplyr::filter(!is.na(inception)) %>%
  dplyr::mutate(year = as.integer(format(inception, "%Y"))) %>%
  dplyr::count(year, status)

p_entry <- ggplot(dplyr::filter(births, year >= 2000), aes(year, n, fill = status)) +
  geom_col() +
  scale_fill_manual(values = c(ativo = "#1b6ca8", cancelado = "grey65")) +
  labs(title = "Fund launches by year, split by current status",
       subtitle = "Grey cohorts are already closed: the industry's survivorship problem, by vintage",
       x = NULL, y = "Funds launched", fill = NULL)
ggsave(file.path(FIG_DIR, "industry_entry_exit.png"), p_entry, width = 9, height = 4, dpi = 300, bg = "white")

# --- 5.4 Who manages the money: bank-affiliated vs independent --------------
# The captive-distribution hypothesis: where flows do not discipline managers,
# weak ones survive longer.
share <- panel %>%
  dplyr::filter(!is.na(aum)) %>%
  dplyr::group_by(period, bank_affiliated) %>%
  dplyr::summarise(pl = sum(aum), .groups = "drop_last") %>%
  dplyr::mutate(share = pl / sum(pl)) %>%
  dplyr::ungroup()

p_bank <- ggplot(share, aes(period, share, fill = bank_affiliated)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(`FALSE` = "#1b6ca8", `TRUE` = "grey65"),
                    labels = c("Independent", "Bank-affiliated")) +
  labs(title = "Share of equity-fund assets by manager type",
       subtitle = "Bank-affiliated managers distribute through captive channels, weakening flow discipline",
       x = NULL, y = "Share of AUM", fill = NULL)
ggsave(file.path(FIG_DIR, "industry_bank_share.png"), p_bank, width = 9, height = 4, dpi = 300, bg = "white")

# --- 5.5 Manager concentration ----------------------------------------------
hhi_gestora <- panel %>%
  dplyr::filter(!is.na(aum)) %>%
  dplyr::group_by(period, gestora) %>%
  dplyr::summarise(pl = sum(aum), .groups = "drop_last") %>%
  dplyr::mutate(s = pl / sum(pl)) %>%
  dplyr::summarise(hhi = sum(s^2), n_managers = dplyr::n(), .groups = "drop")

p_hhi <- ggplot(hhi_gestora, aes(period, hhi)) +
  geom_line(colour = "#1b6ca8", linewidth = 0.7) +
  labs(title = "Concentration of the asset-management industry",
       subtitle = "Herfindahl index of manager market shares by AUM",
       x = NULL, y = "HHI")
ggsave(file.path(FIG_DIR, "industry_manager_hhi.png"), p_hhi, width = 9, height = 3.5, dpi = 300, bg = "white")

# --- 5.6 The gap over time --------------------------------------------------
gap_ts <- est %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(mean_gap = mean(gap), med_gap = stats::median(gap),
                   n = dplyr::n(), .groups = "drop")

p_gap <- ggplot(gap_ts, aes(period)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(aes(y = mean_gap, fill = mean_gap > 0), width = 22, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = "#1b6ca8", `FALSE` = "#c0392b")) +
  labs(title = "Human-machine gap over time",
       subtitle = "Cross-sectional mean of ra_ml - ra_fund. Positive = the systematic benchmark is ahead",
       x = NULL, y = "Percent per month")
ggsave(file.path(FIG_DIR, "gap_over_time.png"), p_gap, width = 9, height = 4, dpi = 300, bg = "white")

# --- 5.7 Structural characteristics -----------------------------------------
chars <- est %>% dplyr::distinct(code, redemption_days, expense_ratio, decay_slope, n_pos_min)

p_chars <- ggplot(tidyr::pivot_longer(
  dplyr::select(chars, redemption_days, expense_ratio, n_pos_min),
  dplyr::everything(), names_to = "var", values_to = "value") %>% dplyr::filter(!is.na(value)),
  aes(value)) +
  geom_histogram(bins = 40, fill = "#1b6ca8", colour = NA) +
  facet_wrap(~var, scales = "free", labeller = as_labeller(c(
    redemption_days = "Redemption notice (days)",
    expense_ratio   = "Management fee (% p.a.)",
    n_pos_min       = "Minimum implied holdings"))) +
  labs(title = "Structural heterogeneity across funds",
       subtitle = "The variation that identifies the capacity and cost channels",
       x = NULL, y = "Funds")
ggsave(file.path(FIG_DIR, "fund_characteristics.png"), p_chars, width = 10, height = 3.6, dpi = 300, bg = "white")


# =============================================================================
# 6. Descriptive table of the estimation sample
# =============================================================================
desc <- est %>%
  dplyr::select(gap, aum, pl_gestora, redemption_days, expense_ratio,
                fund_age_yrs, n_pos_min, decay_slope, hhi_top15, flow_rate_lag) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "v") %>%
  dplyr::filter(is.finite(v)) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(n = dplyr::n(), mean = mean(v), sd = stats::sd(v),
                   p10 = stats::quantile(v, .10), median = stats::median(v),
                   p90 = stats::quantile(v, .90), .groups = "drop")
readr_write(desc, "estimation_sample_descriptives.csv")
cat("\n=== Estimation sample descriptives ===\n")
print(as.data.frame(desc), digits = 3)

cat("\nFigures written to figures/, tables to tables/.\n")

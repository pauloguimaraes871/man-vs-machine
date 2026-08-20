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
#           are identified off the ~84 usable periods.
#   Spec 2  robustness. Time FE. These absorb every shock COMMON to all funds in
#           a period. Fund-trait coefficients survive and are immune to any such
#           shock. Stability across the two is the result worth reporting.
#
# THE BENCHMARK IS CONDITIONAL ON MANDATE (DESIGN_NOTE.md S7.1). Each fund is
# differenced against the machine run under its own mandate -- idiv for
# Dividendos, smll for Small Caps, ibov for everything else -- so the DV no
# longer contains one common ra_ml(t). Two consequences here:
#
#   * Time FE no longer absorb the machine leg BY CONSTRUCTION, only its common
#     component. Do not describe Spec 2 as "the machine drops out".
#   * The three arms are differenced against three different benchmarks, so
#     their levels are not directly comparable. Every pooled specification below
#     carries a MANDATE fixed effect, and the descriptive cuts report by arm.
#
# TWO DEPENDENT VARIABLES, same RHS and same rows:
#   gap      ra_m(i)(t) - ra_i(t)   six-factor risk-adjusted residual difference
#   gap_raw  r_m(i)(t)  - r_i(t)    plain return difference, no risk adjustment
# The wedge between them is the machine's factor exposure. Report both: the raw
# gap is what an investor experiences, the risk-adjusted gap is what survives
# once the machine's style tilts are priced.
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

# Constants only -- gap_panel.run = FALSE loads the definitions without running
# a build. Needed for ML_MANDATES (the canonical benchmark order) and CDI_FILE;
# the H7 ladder sources the same file again inside its own frame for scoping.
local({
  old <- options(gap_panel.run = FALSE)
  on.exit(options(old), add = TRUE)
  source(here::here("build_gap_panel.R"), local = FALSE)
})

if (!all(stats::na.omit(panel$mandate) %in% ML_MANDATES)) {
  rlang::abort(paste0(
    "gap_panel.qs carries mandate(s) outside ML_MANDATES (",
    paste(setdiff(unique(stats::na.omit(panel$mandate)), ML_MANDATES), collapse = ", "),
    "). The panel and build_gap_panel.R are out of sync -- rebuild the panel."
  ))
}
for (nm in c("gap", "gap_raw", "is_redundant_feeder")) {
  if (!nm %in% names(panel)) {
    rlang::abort(sprintf(
      "gap_panel.qs has no `%s` column. The panel predates the current build_gap_panel.R -- rebuild the panel.",
      nm))
  }
}

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", size = 12),
                  plot.subtitle = element_text(colour = "grey35", size = 9)))


# =============================================================================
# 1. Estimation sample, built as an explicit waterfall
# =============================================================================
# Every drop is named and counted. The waterfall IS a table in the paper: going
# from the 701 funds tracked in the refined sample to a few hundred estimable
# ones is a fact about Brazilian fund data, not a detail to bury in a footnote.

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
s2 <- s1 %>% dplyr::filter(!is_redundant_feeder, !is_fof);  waterfall <- add_step(s2, "Drop redundant feeders and funds-of-funds", waterfall)
s3 <- s2 %>% dplyr::filter(is_representative);              waterfall <- add_step(s3, "One fund per duplicate cluster", waterfall)
s4 <- s3 %>% dplyr::filter(!is.na(aum), period >= inception); waterfall <- add_step(s4, "Reporting and post-inception", waterfall)
s5 <- s4 %>% dplyr::filter(!is.na(expense_ratio), !is.na(redemption_days),
                           !is.na(log_pl_gestora));         waterfall <- add_step(s5, "Cost and scale covariates", waterfall)

# Small dependency-free replacement for forcats::fct_lump.
lump_levels <- function(x, n = 6L, other = "Other") {
  keep <- names(sort(table(x), decreasing = TRUE))[seq_len(min(n, dplyr::n_distinct(x)))]
  factor(ifelse(x %in% keep, x, other))
}

# -----------------------------------------------------------------------------
# Named crisis windows for the Brazilian equity market.
#
# These are EVENT windows, not a data-driven regime split: the dates are fixed
# ex ante from the historical record, never chosen by looking at the gap. That
# distinction matters -- a drawdown-threshold rule fitted on this sample would
# select periods partly on the DV and manufacture its own result. The continuous
# `mkt_ret` / `bear` terms cover ordinary beta-like state dependence; these
# dummies cover named liquidity-and-uncertainty episodes, which is a different
# question.
#
# Windows are closed on both ends and matched on the panel's 15th-to-15th
# period stamp, so a period is labelled by the month its return window ENDS.
# Overlaps are impossible by construction (the windows are disjoint); the
# `dplyr::case_when` order is therefore documentation, not precedence.
CRISIS_WINDOWS <- list(
  # Dilma impeachment, the 2015-16 recession and the Lava Jato escalation.
  impeachment = c("2015-04-15", "2016-08-15"),
  # Greve dos Caminhoneiros: the nationwide truckers' strike froze logistics
  # and hit domestic equities hard over roughly six weeks.
  truckers    = c("2018-05-15", "2018-06-15"),
  # COVID crash and the recovery leg through year-end.
  covid       = c("2020-02-15", "2020-12-15"),
  # Selic tightening cycle: 2.00% to 13.75%, a sustained de-rating of equities.
  ratehike    = c("2021-03-15", "2022-08-15")
)

regime_label <- function(period) {
  p   <- as.Date(period)
  out <- rep("normal", length(p))
  for (nm in names(CRISIS_WINDOWS)) {
    w   <- as.Date(CRISIS_WINDOWS[[nm]])
    hit <- !is.na(p) & p >= w[1L] & p <= w[2L]
    if (any(out[hit] != "normal")) {
      rlang::abort(sprintf("Crisis window '%s' overlaps an earlier window.", nm))
    }
    out[hit] <- nm
  }
  # "normal" first so it is the reference level every dummy is read against.
  factor(out, levels = c("normal", names(CRISIS_WINDOWS)))
}

est <- s5 %>%
  dplyr::mutate(
    period_f  = factor(period),
    log_age   = log1p(pmax(fund_age_yrs, 0)),
    # `mandate` is kept on `est` unfactored as well -- the H7 ladder needs it to
    # join against the machine's actual cohort keys, which a factor would not match.
    mandate_f = factor(mandate, levels = ML_MANDATES),
    # Index-tracking classes are NOT separate benchmark arms: they are
    # benchmarked against the ibov machine like any other unlisted class, and
    # their distinct behaviour enters as plain RHS dummies instead. Same role as
    # mandate_fsmll / mandate_fidiv, but without carving out a benchmark.
    index_ativo = as.integer(anbima_class == "Ações Índice Ativo"),
    indexados   = as.integer(anbima_class == "Ações Indexados"),
    perf_fee  = as.integer(perf_fee),
    bank      = as.integer(bank_affiliated),
    efpc      = as.integer(efpc),
    retail    = as.integer(investor_type == "Geral"),
    anbima_f  = lump_levels(anbima_class),
    # ---- Market state (Group C: ENVIRONMENT) --------------------------------
    # `mkt_ret` is the fund's OWN mandate's benchmark index return, carried out
    # of build_gap_panel.R. Common to every fund in a mandate at t, so time FE
    # absorb it entirely: these terms are identified in Spec 3 (no time FE)
    # only, which is exactly why Spec 3 exists as a separate specification
    # rather than as extra columns on Spec 2.
    bear      = as.integer(mkt_ret < 0),
    # droplevels() is load-bearing. CRISIS_WINDOWS is defined over the full
    # historical record, but the estimation sample starts well after the
    # earliest window (the 36-month trailing beta burn-in eats the first three
    # years). A retained-but-empty level yields an all-zero dummy column, which
    # makes crossprod(X) singular and fails inside the two-way vcov's chol()
    # with an error that names a column index and nothing else.
    crisis_f  = droplevels(regime_label(period))
  )

# Say plainly which named windows the sample can actually speak to. A crisis
# dummy that silently vanished would otherwise look like a null result.
local({
  present <- levels(est$crisis_f)
  absent  <- setdiff(names(CRISIS_WINDOWS), present)
  rlang::inform(sprintf(
    "Estimation sample spans %s to %s.\n  Crisis windows IN sample: %s.\n  Crisis windows OUT of sample (no rows, dropped): %s.",
    min(est$period), max(est$period),
    if (length(setdiff(present, "normal"))) paste(setdiff(present, "normal"), collapse = ", ") else "none",
    if (length(absent)) paste(absent, collapse = ", ") else "none"))
})

# -----------------------------------------------------------------------------
# Market VOLATILITY REGIME (Group C) -- the primary regime definition.
#
# `mkt_vol` arrives from build_gap_panel.R as the trailing 12-month standard
# deviation of the mandate's own benchmark return, computed strictly through
# t-1 (see MKT_VOL_WINDOW / trailing_sd_prior there for why t is excluded, and
# why it is computed upstream where the full ML index is still in scope).
#
# `vol_rel` divides by the mandate's OWN full-sample mean, so 1.0 reads as
# "typical volatility for this universe" and SMLL's structurally higher
# volatility does not masquerade as a permanent high-volatility regime.
#
# NOTE ON THE FULL-SAMPLE MEAN. For a CONTINUOUS term this is harmless: it is
# division by a constant, so the coefficient rescales and the t-statistic is
# numerically identical to one on raw mkt_vol. It matters for the REGIME
# CUTS below, whose boundaries genuinely use full-sample information. The
# volatility regime is therefore a descriptive classification, not a tradeable
# timing rule, and the paper must not present it as one.
#
# Two cuts are built because they answer different questions:
#   vol_regime_f  terciles of vol_rel within mandate -- balanced cells, no
#                 arbitrary threshold, and the form used in the regression;
#   high_vol      a single split at the full-sample mean -- the literal
#                 "above or below normal volatility" reading, kept for the
#                 descriptive table where a two-way split is what a reader wants.
vol_tab <- est %>%
  dplyr::distinct(mandate, period, mkt_vol) %>%
  dplyr::group_by(mandate) %>%
  dplyr::mutate(
    vol_rel     = mkt_vol / mean(mkt_vol, na.rm = TRUE),
    high_vol    = as.integer(vol_rel > 1),
    vol_tercile = dplyr::if_else(is.na(vol_rel), NA_integer_,
                                 dplyr::ntile(vol_rel, 3))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # "mid" is the reference level, so LOW and HIGH each read as a deviation
    # from ordinary market conditions rather than from the calmest third.
    vol_regime_f = factor(dplyr::case_when(vol_tercile == 1L ~ "vol_low",
                                           vol_tercile == 3L ~ "vol_high",
                                           !is.na(vol_tercile) ~ "vol_mid"),
                          levels = c("vol_mid", "vol_low", "vol_high"))
  ) %>%
  dplyr::select(mandate, period, vol_rel, high_vol, vol_tercile, vol_regime_f)

est <- dplyr::left_join(est, vol_tab, by = c("mandate", "period"))

local({
  v <- vol_tab[!is.na(vol_tab$vol_rel), ]
  rlang::inform(sprintf(
    "Market volatility regime: %d of %d mandate-periods defined (first %s).\n  vol_rel range %.2f-%.2f | terciles: %s",
    nrow(v), nrow(vol_tab),
    if (nrow(v)) as.character(min(v$period)) else "n/a",
    if (nrow(v)) min(v$vol_rel) else NA_real_,
    if (nrow(v)) max(v$vol_rel) else NA_real_,
    paste(sprintf("%s=%d", levels(v$vol_regime_f),
                  as.integer(table(v$vol_regime_f))), collapse = ", ")))
})

readr_write <- function(tbl, file) {
  utils::write.csv(tbl, file.path(TAB_DIR, file), row.names = FALSE)
}
readr_write(waterfall, "sample_waterfall.csv")
print(waterfall)

# Composition of the estimation sample by benchmark arm. Report this next to the
# waterfall: the arms are very unequal, and a coefficient driven entirely by
# ibov should not be read as a statement about mandated funds. `factor_wedge` is
# the part of the raw return gap that six-factor adjustment removes.
by_mandate <- est %>%
  dplyr::group_by(mandate) %>%
  dplyr::summarise(rows = dplyr::n(), funds = dplyr::n_distinct(code),
                   periods = dplyr::n_distinct(period),
                   mean_gap = mean(gap), sd_gap = stats::sd(gap),
                   mean_gap_raw = mean(gap_raw),
                   factor_wedge = mean(gap_raw - gap),
                   pct_machine_ahead = mean(gap > 0), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(rows))
readr_write(by_mandate, "gap_by_mandate.csv")
cat("\n=== Estimation sample by benchmark arm ===\n")
print(as.data.frame(by_mandate), digits = 3)

# The same cut at ANBIMA-class grain. This is where the index-tracking classes
# become visible WITHOUT giving them their own benchmark: they are still
# differenced against the ibov machine, this table just reports them separately.
by_class <- est %>%
  dplyr::group_by(anbima_class) %>%
  dplyr::summarise(rows = dplyr::n(), funds = dplyr::n_distinct(code),
                   mean_gap = mean(gap), mean_gap_raw = mean(gap_raw),
                   pct_machine_ahead = mean(gap > 0), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(rows))
readr_write(by_class, "gap_by_class.csv")
cat("\n=== Estimation sample by ANBIMA class ===\n")
print(as.data.frame(by_class), digits = 3)

# A regressor with no variation in the estimation sample makes crossprod(X)
# singular and chol() fails inside the two-way vcov -- an opaque linear-algebra
# error a long way from its cause. Drop such columns here, loudly, so the model
# that runs is the model that is reported.
#
# This is not hypothetical: the refined sample has `perf_fee` FALSE for all
# 21,662 candidate rows (the export carries no "Sim" for Cobra taxa
# performance), and `leveraged` is "N" for all 703 funds.
drop_degenerate <- function(vars, data) {
  n_unique <- vapply(vars, function(v) dplyr::n_distinct(data[[v]][!is.na(data[[v]])]),
                     integer(1))
  dead <- vars[n_unique < 2L]
  if (length(dead) > 0L) {
    rlang::warn(paste0(
      "Dropping regressor(s) with no variation in the estimation sample: ",
      paste(dead, collapse = ", "), "."
    ))
  }
  setdiff(vars, dead)
}


# =============================================================================
# 2. Clustered inference (implemented here: no sandwich/fixest available)
# =============================================================================
# Cameron-Gelbach-Miller two-way clustering:
#     V = V_fund + V_period - V_(fund x period)
# Each component is the usual cluster-robust sandwich. With ~84 period clusters
# the time dimension is adequate but not generous, so period-level inference
# should still be read as indicative rather than exact.

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

  # lm() SILENTLY DROPS rows with NA in any model variable, so a cluster vector
  # taken from the full estimation frame is longer than X and no longer aligned
  # with it. Left uncorrected this either errors out of bounds in cluster_meat()
  # or -- worse -- groups the wrong rows together and returns a plausible-looking
  # standard error. Align to the rows the fit actually used.
  omit <- stats::na.action(fit)
  if (!is.null(omit)) {
    c1 <- c1[-as.integer(omit)]
    c2 <- c2[-as.integer(omit)]
  }
  if (length(c1) != nrow(X) || length(c2) != nrow(X)) {
    rlang::abort(sprintf(
      "Cluster vectors (%d, %d) do not align with the model matrix (%d rows).",
      length(c1), length(c2), nrow(X)))
  }

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

# Fit with every RHS column -- continuous AND dummy, including FE indicators --
# centered at its in-sample mean, so the intercept reads as "the DV at average
# sample characteristics" rather than at log_pl_fundo = log_pl_gestora =
# expense_ratio = ... = 0, a fund that does not exist and is nowhere near the
# data. Centering a design that includes an intercept is a pure
# reparametrisation: coefficients, fitted values and residuals on every other
# term are IDENTICAL to the uncentered fit (verified 2026-08-19 against the
# unconditional mean gap_raw, which the centered intercept must recover and
# does: 0.503 vs 0.503). Only the intercept -- and hence its clustered SE --
# differs, which is exactly the number that was misleading: reported
# uncentered, Spec 1's gap_raw intercept was 0.03 (se 0.48, p = 0.95) and read
# as "no alpha after controlling for costs." It was never alpha; it was a
# linear extrapolation to a zero-everything fund. Centered, it is 0.50 (p =
# 0.001), matching the unconditional mean gap_raw exactly.
lm_centered <- function(formula, data) {
  mf <- stats::model.frame(formula, data, na.action = stats::na.omit)
  y  <- stats::model.response(mf)
  X  <- stats::model.matrix(formula, mf)
  is_int <- colnames(X) == "(Intercept)"
  X[, !is_int] <- scale(X[, !is_int, drop = FALSE], center = TRUE, scale = FALSE)

  df <- as.data.frame(X[, !is_int, drop = FALSE], check.names = FALSE)
  # model.matrix() column names for a Date-valued factor (period_f) contain
  # hyphens ("period_f2016-01-15"), not valid as bare data.frame/formula names.
  # make.names() only mangles what follows the "period_f" prefix, so the
  # downstream `!grepl("^period_f", term)` filter in run_specs() still matches.
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  df$.y <- y

  fit <- stats::lm(stats::reformulate(setdiff(colnames(df), ".y"), response = ".y"),
                   data = df)
  # vcov_twoway() reads stats::na.action(fit) to align the (fund, period)
  # cluster vectors -- taken from the full `est` frame -- with the model
  # matrix. Fitting on the pre-NA-dropped `df` above means lm() itself has
  # nothing left to omit, so na.action(fit) would come back NULL and the
  # alignment step would silently no-op. Reattach what model.frame() actually
  # dropped so that alignment still happens.
  fit$na.action <- attr(mf, "na.action")
  fit
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

# On `log_age`: the panel is a full cross-join, so pre-inception rows exist by
# design and carry a negative fund_age_yrs. Those rows never reach `est` -- the
# waterfall's `period >= inception` step removes them, and inside the estimation
# sample fund_age_yrs runs 3.0..30.5 years (median 10.4) with no negatives.
# Verified 2026-08-18 against actual return availability: 0 of 701 funds have a
# return predating their cadastral inception. Do not "fix" the age field on the
# strength of negatives seen in the raw panel; measure it on `est`.
rhs_core <- drop_degenerate(
  c("log_pl_fundo", "log_pl_gestora", "redemption_days",
    "expense_ratio", "perf_fee", "log_age", "bank", "retail", "efpc"),
  est
)

# MANDATE FE in both specs. Without it the intercept averages arms that are
# differenced against different machines, and any fund trait that correlates
# with mandate (small caps are smaller, dividend funds cheaper) would pick up
# the difference between benchmarks rather than a fund effect. Only added when
# more than one mandate is present, so the code still runs if a future sample
# collapses to a single arm.
#
# The index-tracking dummies sit alongside it. They do NOT select a benchmark --
# both classes are differenced against the ibov machine -- so they are ordinary
# characteristic controls and cannot reintroduce a universe effect.
rhs_class <- drop_degenerate(c("index_ativo", "indexados"), est)
rhs_fe <- c(rhs_core,
            if (dplyr::n_distinct(est$mandate) > 1L) "mandate_f",
            rhs_class)

# Both DVs, same RHS and same rows: `gap` is the six-factor residual difference,
# `gap_raw` the plain return difference. Reporting them together separates "the
# machine earned more" from "the machine earned more after its factor tilts are
# stripped out" -- the wedge between the two is the factor-exposure story.
run_specs <- function(dv) {
  f1 <- stats::as.formula(paste(dv, "~", paste(rhs_fe, collapse = " + ")))
  f2 <- stats::as.formula(paste(dv, "~", paste(c(rhs_fe, "period_f"), collapse = " + ")))

  m1 <- lm_centered(f1, est)
  m2 <- lm_centered(f2, est)

  t1 <- tidy_clustered(m1, est$code, est$period)
  # Time-FE coefficients are nuisance parameters; drop them from the report.
  t2 <- tidy_clustered(m2, est$code, est$period) %>%
    dplyr::filter(!grepl("^period_f", term))

  dplyr::full_join(
    dplyr::select(t1, term, est_1 = estimate, se_1 = std_error, t_1 = t),
    dplyr::select(t2, term, est_2 = estimate, se_2 = std_error, t_2 = t),
    by = "term"
  ) %>%
    dplyr::mutate(dv = dv, .before = 1)
}

# -----------------------------------------------------------------------------
# Spec 3 -- MARKET STATE. Group C in the panel taxonomy: `mkt_ret`, `bear` and
# the crisis dummies are common to every fund in a mandate at t, so Spec 2's
# time FE absorb them completely and they can only be identified WITHOUT time
# FE. That is the whole reason this is a third specification instead of extra
# columns on Spec 2.
#
# Everything from rhs_fe is retained, so the market-state coefficients are read
# holding the fund traits fixed. Centering applies here too, which matters more
# than usual: with `bear` and the crisis dummies uncentered, the intercept would
# be "the gap for an average fund in a NON-crisis UP month", not the sample
# average, and would not be comparable to Spec 1's.
#
# `mkt_ret` and `bear` are deliberately BOTH included. `mkt_ret` picks up smooth
# beta-like state dependence; `bear` picks up an asymmetry at zero that a linear
# term cannot represent. They are correlated but not collinear, and dropping
# either one changes the question rather than tightening the answer.
# Spec 3 (direction + crisis) and Spec 4 (adds volatility) are kept SEPARATE
# rather than merged, and the reason is sample loss, not taste. `vol_rel` needs
# 12 complete prior months, and the estimation sample starts 2019-01, so the
# volatility term is undefined until 2020-02. Folding it into Spec 3 would cost
# ~11% of rows AND the entire pre-COVID baseline -- which is precisely the
# comparison the COVID dummy is measured against. Merged, the COVID coefficient
# degrades from t = -2.59 to t = -2.25 on gap_raw for no analytical gain.
#
# `high_vol` is deliberately NOT in either regression: it is a coarsening of
# `vol_rel`, and including both would split one effect across two near-collinear
# terms. The dummy earns its keep in the descriptive cell means below.
rhs_regime <- drop_degenerate(c("mkt_ret", "bear"), est)
if (dplyr::n_distinct(est$crisis_f) > 1L) rhs_regime <- c(rhs_regime, "crisis_f")
# Volatility enters Spec 4 as the REGIME FACTOR (vol_low / vol_high against the
# vol_mid reference), not as the continuous vol_rel: the question is whether the
# gap differs by volatility regime, and a factor answers it without imposing
# linearity in volatility -- which the tercile means below give no reason to
# assume.
rhs_vol <- if (dplyr::n_distinct(est$vol_regime_f[!is.na(est$vol_regime_f)]) > 1L) {
  "vol_regime_f"
} else character(0)

run_regime_spec <- function(dv, extra = character(0), spec = "Spec 3") {
  terms <- c(rhs_regime, extra)
  if (length(terms) == 0L) return(NULL)
  f3 <- stats::as.formula(
    paste(dv, "~", paste(c(rhs_fe, terms), collapse = " + "))
  )
  m3 <- lm_centered(f3, est)
  # `n` is reported because these specs drop any row with a missing regime
  # variable and so need not run on exactly the Spec 1/2 sample. Spec 3 should
  # match Spec 1 (mkt_ret is complete); Spec 4 is expected to be smaller.
  tidy_clustered(m3, est$code, est$period) %>%
    dplyr::transmute(spec = spec, dv = dv, term, estimate, std_error, t, p,
                     n = stats::nobs(m3))
}

regime_reg <- dplyr::bind_rows(
  purrr::map_dfr(c("gap", "gap_raw"), run_regime_spec),
  if (length(rhs_vol)) {
    purrr::map_dfr(c("gap", "gap_raw"), run_regime_spec,
                   extra = rhs_vol, spec = "Spec 4")
  }
)

reg_out <- purrr::map_dfr(c("gap", "gap_raw"), run_specs)
readr_write(reg_out, "gap_regressions.csv")
for (d in c("gap", "gap_raw")) {
  cat("\n=== DV =", d,
      "| Spec 1 (no time FE) / Spec 2 (time FE), two-way clustered ===\n",
      "    (RHS centered at sample means: (Intercept) = DV at an AVERAGE",
      "fund/gestora, not an all-zero one; all other coefficients unchanged",
      "by centering)\n")
  print(as.data.frame(dplyr::select(dplyr::filter(reg_out, dv == d), -dv)), digits = 3)
}

# The unconditional gap, which is the number the abstract quotes. Reported per
# benchmark arm as well as pooled, because pooling averages differently-mandated
# contests -- and for both DVs, since the raw and risk-adjusted headlines carry
# different significance. Loop variable is `arm_lbl`, not `mandate`, so it does
# not shadow the column inside dplyr::filter().
uncond <- purrr::map_dfr(c("pooled", ML_MANDATES), function(arm_lbl) {
  d <- if (identical(arm_lbl, "pooled")) est else dplyr::filter(est, mandate == arm_lbl)
  if (nrow(d) == 0L) return(NULL)
  purrr::map_dfr(c("gap", "gap_raw"), function(dv) {
    fit <- stats::lm(stats::as.formula(paste(dv, "~ 1")), data = d)
    tidy_clustered(fit, d$code, d$period) %>%
      dplyr::transmute(arm = arm_lbl, dv = dv, n = nrow(d),
                       funds = dplyr::n_distinct(d$code),
                       mean_gap = estimate, se = std_error, t = t, p = p)
  })
})
readr_write(uncond, "gap_unconditional.csv")
cat("\n=== Unconditional gap (pp/month), two-way clustered ===\n")
print(as.data.frame(uncond), digits = 3)


# =============================================================================
# 3b. Regime analysis
# =============================================================================
# Two cuts of the same question, both descriptive-then-conditional:
#   (i)  UP vs DOWN months, plus terciles of the market return -- ordinary
#        state dependence, and the test of whether the gap is a disguised
#        beta bet that only pays in rising markets;
#   (ii) named crisis windows -- whether the machine's edge survives episodes
#        where discretionary managers claim their advantage lies.
#
# Both are reported on the SAME estimation sample and with the same two-way
# clustering as the headline, so the numbers are directly comparable to the
# unconditional table above.

# Cell means with clustered SEs, so a "the machine wins in both regimes" claim
# carries an inference and not just two point estimates.
regime_cell <- function(d, label_col, label_val) {
  purrr::map_dfr(c("gap", "gap_raw"), function(dv) {
    fit <- stats::lm(stats::as.formula(paste(dv, "~ 1")), data = d)
    tidy_clustered(fit, d$code, d$period) %>%
      dplyr::transmute(cut = label_col, regime = label_val, dv = dv,
                       n = nrow(d), funds = dplyr::n_distinct(d$code),
                       periods = dplyr::n_distinct(d$period),
                       mean_mkt = mean(d$mkt_ret, na.rm = TRUE),
                       mean_vol_rel = mean(d$vol_rel, na.rm = TRUE),
                       mean_gap = estimate, se = std_error, t = t, p = p)
  })
}

est_mkt <- dplyr::filter(est, !is.na(mkt_ret))
if (nrow(est_mkt) == 0L) {
  rlang::warn(paste("No non-missing mkt_ret on the estimation sample --",
                    "rebuild gap_panel.qs with the current build_gap_panel.R",
                    "to populate the market return. Skipping regime tables."))
  by_regime <- tibble::tibble()
} else {
  # Terciles are cut on the PERIOD-level market return, not on fund-rows, so a
  # month with many surviving funds does not drag the breakpoints toward itself.
  mkt_by_period <- est_mkt %>%
    dplyr::distinct(mandate, period, mkt_ret) %>%
    dplyr::group_by(mandate) %>%
    dplyr::mutate(mkt_tercile = dplyr::ntile(mkt_ret, 3)) %>%
    dplyr::ungroup()
  est_mkt <- dplyr::left_join(est_mkt, mkt_by_period,
                              by = c("mandate", "period", "mkt_ret"))

  by_regime <- dplyr::bind_rows(
    purrr::map_dfr(c(0L, 1L), function(b) {
      d <- dplyr::filter(est_mkt, bear == b)
      if (nrow(d) == 0L) return(NULL)
      regime_cell(d, "direction", if (b == 1L) "DOWN" else "UP")
    }),
    purrr::map_dfr(sort(unique(est_mkt$mkt_tercile)), function(k) {
      d <- dplyr::filter(est_mkt, mkt_tercile == k)
      if (nrow(d) == 0L) return(NULL)
      regime_cell(d, "mkt_tercile", paste0("T", k))
    }),
    purrr::map_dfr(levels(est_mkt$crisis_f), function(g) {
      d <- dplyr::filter(est_mkt, crisis_f == g)
      if (nrow(d) == 0L) return(NULL)
      regime_cell(d, "crisis", g)
    }),
    # Volatility split. Rows before the trailing window fills have no regime and
    # are excluded rather than lumped into "low", so the two cells together are
    # smaller than the full sample by construction.
    purrr::map_dfr(c(0L, 1L), function(h) {
      d <- dplyr::filter(est_mkt, !is.na(high_vol), high_vol == h)
      if (nrow(d) == 0L) return(NULL)
      regime_cell(d, "volatility", if (h == 1L) "HIGH" else "LOW")
    }),
    purrr::map_dfr(levels(est_mkt$vol_regime_f), function(g) {
      d <- dplyr::filter(est_mkt, !is.na(vol_regime_f), vol_regime_f == g)
      if (nrow(d) == 0L) return(NULL)
      regime_cell(d, "vol_regime", g)
    })
  )

  readr_write(by_regime, "gap_by_regime.csv")
  cat("\n=== Gap by market regime (pp/month), two-way clustered ===\n")
  print(as.data.frame(by_regime), digits = 3)

  # Machine vs average fund inside each regime. This is the number that shows
  # WHERE the gap comes from: winning more on the way up, or losing less on the
  # way down. Averaged over fund-rows so it matches the gap decomposition.
  capture <- est_mkt %>%
    dplyr::group_by(direction = ifelse(bear == 1L, "DOWN", "UP")) %>%
    dplyr::summarise(periods = dplyr::n_distinct(period),
                     mean_mkt = mean(mkt_ret, na.rm = TRUE),
                     avg_fund = mean(r_fund, na.rm = TRUE),
                     machine  = mean(r_ml, na.rm = TRUE),
                     diff     = mean(r_ml - r_fund, na.rm = TRUE),
                     .groups = "drop")
  readr_write(capture, "gap_updown_capture.csv")
  cat("\n=== Up/down capture: machine vs fund raw returns (pp/month) ===\n")
  print(as.data.frame(capture), digits = 3)
}

if (nrow(regime_reg) > 0L) {
  readr_write(regime_reg, "gap_regime_regressions.csv")
  cat("\n=== Spec 3 (direction + crisis) / Spec 4 (adds volatility),",
      "two-way clustered ===\n",
      "    (no time FE -- Group C terms are absorbed by them by construction;\n",
      "     RHS centered, so (Intercept) is the DV at average characteristics\n",
      "     AND average market state, comparable to Spec 1's intercept.\n",
      "     Spec 4 starts 12 months later: vol_rel needs a filled window.)\n")
  for (sp in unique(regime_reg$spec)) {
    for (d in c("gap", "gap_raw")) {
      cat("\n--", sp, "| DV =", d, "--\n")
      print(as.data.frame(dplyr::select(
        dplyr::filter(regime_reg, spec == sp, dv == d), -spec, -dv)), digits = 3)
    }
  }
}


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
  cdi_daily <- read_economatica_daily(CDI_FILE)

  fund_m  <- compound_daily_to_windows(daily$ret, ml_index, codes)
  fac     <- build_factor_mat(ml_index, cdi_daily)
  ym      <- format(ml_index, "%Y-%m")
  fund_ra <- rolling_risk_adjusted(fund_m, ym, fac)$ra

  ml_raw <- read_ml_returns()
  # The mandate map must come from the SAME panel the estimation sample uses,
  # so the ladder is computed on exactly the rows the regressions see.
  keep <- est %>% dplyr::distinct(code, period, mandate)

  purrr::map_dfr(c(ML_SCHEMES, "average"), function(s) {
    # One benchmark column per mandate, so the ladder holds the conditional
    # design fixed and varies only the construction scheme.
    res   <- resolve_ml_benchmark(ml_raw, s)
    ml_ra <- rolling_risk_adjusted(res$mat[, unname(res$cols), drop = FALSE], ym, fac)$ra

    ml_long <- tibble::as_tibble(ml_ra) %>%
      dplyr::mutate(period = ml_index) %>%
      tidyr::pivot_longer(-period, names_to = "ml_benchmark", values_to = "ra_ml") %>%
      dplyr::left_join(
        tibble::tibble(mandate = names(res$cols), ml_benchmark = unname(res$cols)),
        by = "ml_benchmark"
      ) %>%
      dplyr::select(period, mandate, ra_ml)

    tibble::as_tibble(fund_ra) %>%
      dplyr::mutate(period = ml_index) %>%
      tidyr::pivot_longer(-period, names_to = "code", values_to = "ra_fund") %>%
      dplyr::inner_join(keep, by = c("code", "period")) %>%
      dplyr::left_join(ml_long, by = c("period", "mandate")) %>%
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
# Survivorship made visible: cancelled funds are retained in the refined
# cadastral, so a live-funds-only study would be badly biased. The split below
# reports the actual counts rather than asserting a stale one.
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
# Pooled first, then split by benchmark arm. The pooled series is dominated by
# ibov (roughly 90% of rows), so the split is what shows whether the mandated
# arms move with it or against it.
gap_ts <- est %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(mean_gap = mean(gap), med_gap = stats::median(gap),
                   n = dplyr::n(), .groups = "drop")

p_gap <- ggplot(gap_ts, aes(period)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(aes(y = mean_gap, fill = mean_gap > 0), width = 22, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = "#1b6ca8", `FALSE` = "#c0392b")) +
  labs(title = "Human-machine gap over time",
       subtitle = "Cross-sectional mean of ra_m(i) - ra_fund. Positive = the systematic benchmark is ahead",
       x = NULL, y = "Percent per month")
ggsave(file.path(FIG_DIR, "gap_over_time.png"), p_gap, width = 9, height = 4, dpi = 300, bg = "white")

gap_ts_mandate <- est %>%
  dplyr::group_by(period, mandate) %>%
  dplyr::summarise(mean_gap = mean(gap), mean_gap_raw = mean(gap_raw),
                   n = dplyr::n(), .groups = "drop")
readr_write(gap_ts_mandate, "gap_over_time_by_mandate.csv")

p_gap_m <- ggplot(gap_ts_mandate, aes(period, mean_gap)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(aes(colour = mandate), linewidth = 0.6, show.legend = FALSE) +
  facet_wrap(~mandate, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c(ibov = "#1b6ca8", smll = "#2a9d5c",
                                 idiv = "#b8860b")) +
  labs(title = "Human-machine gap over time, by benchmark arm",
       subtitle = "Each arm is differenced against the machine run under its own mandate, so levels are not comparable across panels",
       x = NULL, y = "Percent per month")
ggsave(file.path(FIG_DIR, "gap_over_time_by_mandate.png"), p_gap_m,
       width = 9, height = 6, dpi = 300, bg = "white")

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

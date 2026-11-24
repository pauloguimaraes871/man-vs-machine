# =============================================================================
# analysis_gap.R
#
# Estimation and descriptive output for the human-machine alpha gap.
# Reads data/derived/gap_panel.qs (built by build_gap_panel.R) and writes
# figures to figures/ and LaTeX tables to tables/.
#
# Nothing here modifies the panel. Run build_gap_panel.R first.
#
# REGRESSION DESIGN
#   Core and flow/scale blocks report pooled OLS, return-window FE OLS,
#   fund-plus-return-window FE OLS, and PST-style recursive-demeaning IV. The RD
#   columns are sensitivity estimates whose first-stage diagnostics are shown in
#   the tables; they are not described as causal corrections when identification
#   is weak. Market-state models omit return-window FE, which would absorb their
#   common time variation, and instead compare pooled and fund-FE estimates.
#   Mediator models are explicitly descriptive and do not receive an RD column.
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
# Group D variables (rho_machine and holdings concentration) are mediators and
# remain absent from the core right-hand side. They enter only the descriptive
# mediator block. Lagged flow and flow volatility similarly enter a separate
# flow-and-scale block rather than redefining the core model.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(ggplot2); library(scales); library(here); library(rlang)
})

source(here::here("R", "recursive_demeaning_iv.R"), local = FALSE)

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

# Trailing residual volatility MUST be computed here, on the complete
# (code, period) grid, and never on the filtered estimation sample. `panel`
# carries every fund-period, so consecutive rows are consecutive months. `est`
# does not: the waterfall drops fund-periods with missing AUM, expense ratio,
# redemption terms, or manager scale, and 213 of its 242 funds end up with at
# least one hole. Applied downstream, trailing_sd_prior() would silently take
# the last twelve AVAILABLE ROWS rather than the last twelve MONTHS -- which for
# 86% of estimation rows spans more than a year. The failure is invisible
# because every scored row has a non-missing ra_fund, so the all-12-present
# guard inside trailing_sd_prior() never fires. Computed here instead, a window
# straddling an unscored month correctly returns NA. Same reasoning as mkt_vol,
# which build_gap_panel.R computes upstream for exactly this reason.
panel <- panel %>%
  dplyr::arrange(code, period) %>%
  dplyr::group_by(code) %>%
  dplyr::mutate(resid_vol_12 = trailing_sd_prior(ra_fund, window = 12L)) %>%
  dplyr::ungroup()

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
    fund_f    = factor(code),
    log_age   = log1p(pmax(fund_age_yrs, 0)),
    # `mandate` is kept on `est` unfactored as well -- the H7 ladder needs it to
    # join against the machine's actual cohort keys, which a factor would not match.
    mandate_f = factor(mandate, levels = ML_MANDATES),
    # Index-tracking classes are NOT separate benchmark arms: they are
    # benchmarked against the ibov machine like any other unlisted class, and
    # their distinct behaviour enters as plain RHS dummies instead. Same role as
    # mandate_fsmll / mandate_fidiv, but without carving out a benchmark.
    # Match on the unaccented suffix so the controls remain reproducible when a
    # Windows R session cannot activate a UTF-8 locale. Exact comparisons with
    # "Ações Índice Ativo" silently returned all FALSE in that environment.
    index_ativo = as.integer(grepl("ndice Ativo$", anbima_class)),
    indexados   = as.integer(grepl("Indexados$", anbima_class)),
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

# Variables used only in the descriptive extensions below. All return-based
# measures are known before the return window whose gap they are used to
# describe. This timing prevents a mechanical contemporaneous relation, but it
# does not make the measures exogenous: flows can respond to persistent past
# performance, and risk/concentration are choices through which skill may act.
est <- est %>%
  dplyr::mutate(
    # Rescale coefficients to economically readable changes while preserving
    # the underlying variables and regression fit.
    flow_rate_lag_10 = flow_rate_lag / 0.10,
    flow_vol_12_10   = flow_vol_12 / 0.10,
    rho_machine_10   = rho_machine / 0.10,
    hhi_top15_01     = hhi_top15 / 0.01
  )

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

# Console-only diagnostic requested by the paper owner. `name_quant` is a thin,
# exploratory label (six funds in the current estimation sample), so these
# results are deliberately neither written to disk nor used in any regression.
quant_return_diagnostic <- est %>%
  dplyr::mutate(process = dplyr::if_else(name_quant, "Quant", "Other")) %>%
  dplyr::group_by(process) %>%
  dplyr::summarise(
    rows = dplyr::n(),
    funds = dplyr::n_distinct(code),
    mean_fund_return = mean(r_fund, na.rm = TRUE),
    mean_fund_residual = mean(ra_fund, na.rm = TRUE),
    mean_raw_gap = mean(gap_raw, na.rm = TRUE),
    mean_adjusted_gap = mean(gap, na.rm = TRUE),
    .groups = "drop"
  )
cat("\n=== Exploratory quant-versus-other diagnostic (pp/month; not tabulated) ===\n")
print(as.data.frame(quant_return_diagnostic), digits = 3)

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

  # lm() marks exactly aliased coefficients as NA but retains their columns in
  # model.matrix(). The reduced flow/holdings samples can make a class or date
  # dummy redundant even when it varied in the full estimation sample. Remove
  # only columns already declared non-estimable by lm(); attempting to invert
  # them would otherwise fail inside chol(crossprod(X)).
  estimable <- !is.na(stats::coef(fit))
  if (any(!estimable)) {
    rlang::warn(paste0(
      "Omitting aliased coefficient(s) from clustered covariance: ",
      paste(names(estimable)[!estimable], collapse = ", "), "."
    ))
    X <- X[, names(estimable)[estimable], drop = FALSE]
  }

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
  mf <- stats::model.frame(
    formula, data, na.action = stats::na.omit, drop.unused.levels = TRUE
  )
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
# 3. Regression families
# =============================================================================
# The main estimand is a conditional association with the human-machine gap, not
# a structural return-to-scale parameter. Pooled and time-FE OLS therefore remain
# the descriptive headline. Fund + time FE isolate within-fund variation, while
# RD-IV is reported as an endogeneity robustness estimator with its first-stage
# diagnostics visible. No estimator is described as causal without qualification.

rhs_core <- drop_degenerate(
  c("log_pl_fundo", "log_pl_gestora", "redemption_days",
    "expense_ratio", "perf_fee", "log_age", "bank", "retail"),
  est
)
rhs_fe <- c(
  rhs_core,
  if (dplyr::n_distinct(est$mandate) > 1L) "mandate_f"
)
# Only genuinely time-varying core variables survive fund fixed effects.
rhs_within_core <- intersect(c("log_pl_fundo", "log_pl_gestora", "log_age"),
                             rhs_core)

rhs_regime <- drop_degenerate(c("mkt_ret", "bear"), est)
if (dplyr::n_distinct(est$crisis_f) > 1L) rhs_regime <- c(rhs_regime, "crisis_f")
rhs_vol <- if (dplyr::n_distinct(est$vol_regime_f[!is.na(est$vol_regime_f)]) > 1L) {
  "vol_regime_f"
} else character()
rhs_market <- c(rhs_regime, rhs_vol)
rhs_flow <- drop_degenerate(c("flow_rate_lag_10", "flow_vol_12_10"), est)
rhs_mechanism <- drop_degenerate(
  c("resid_vol_12", "rho_machine_10", "hhi_top15_01"), est
)

fit_gap_ols <- function(dv, extra = character(), family,
                        model = c("Pooled OLS", "Time FE OLS",
                                  "Fund + time FE OLS", "Fund FE OLS")) {
  model <- match.arg(model)
  rhs <- switch(
    model,
    "Pooled OLS" = c(rhs_fe, extra),
    "Time FE OLS" = c(rhs_fe, extra, "period_f"),
    "Fund + time FE OLS" = c(rhs_within_core, extra, "fund_f", "period_f"),
    "Fund FE OLS" = c(rhs_within_core, extra, "fund_f")
  )
  rhs <- unique(rhs)
  f <- stats::as.formula(paste(dv, "~", paste(rhs, collapse = " + ")))
  fit <- lm_centered(f, est)
  tidy_clustered(fit, est$code, est$period) %>%
    dplyr::filter(!grepl("^(period_f|fund_f)", term)) %>%
    dplyr::transmute(
      family = family, model = model, dv = dv, term, estimate, std_error, t, p,
      n = stats::nobs(fit),
      fund_fe = model %in% c("Fund + time FE OLS", "Fund FE OLS"),
      time_fe = model %in% c("Time FE OLS", "Fund + time FE OLS")
    )
}

core_ols <- purrr::map_dfr(c("gap", "gap_raw"), function(dv) {
  purrr::map_dfr(
    c("Pooled OLS", "Time FE OLS", "Fund + time FE OLS"),
    ~ fit_gap_ols(dv, family = "Core", model = .x)
  )
})

# Market states are intentionally estimated without return-window FE. Fund FE
# are informative here because the state variables vary through time and the
# within-fund model removes persistent differences in skill. Return-window FE
# would instead absorb the common component of bear, crisis, and volatility.
market_reg <- purrr::map_dfr(c("gap", "gap_raw"), function(dv) {
  purrr::map_dfr(
    c("Pooled OLS", "Fund FE OLS"),
    ~ fit_gap_ols(dv, extra = rhs_market, family = "Market states", model = .x)
  )
})

flow_ols <- purrr::map_dfr(c("gap", "gap_raw"), function(dv) {
  purrr::map_dfr(
    c("Pooled OLS", "Time FE OLS", "Fund + time FE OLS"),
    ~ fit_gap_ols(dv, extra = rhs_flow, family = "Flow and scale", model = .x)
  )
})

mechanism_reg <- purrr::map_dfr(c("gap", "gap_raw"), function(dv) {
  purrr::map_dfr(
    c("Pooled OLS", "Time FE OLS", "Fund + time FE OLS"),
    ~ fit_gap_ols(dv, extra = rhs_mechanism, family = "Mediators", model = .x)
  )
})

# Recursive-demeaning IV robustness. Static fund characteristics are absorbed by
# the fund transformation. Core RD treats opening fund AUM as endogenous. The
# flow-and-scale RD treats both opening AUM and lagged flow as endogenous and
# projects each on the full pair of backward-demeaned instruments. Manager AUM,
# log age, flow volatility, and return-window indicators are maintained as
# exogenous controls; the paper explicitly qualifies that assumption.
run_gap_rd_iv <- function(dv, family, endogenous, exogenous) {
  model_columns <- unique(c("code", "period", dv, endogenous, exogenous))
  rd_data <- est %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(model_columns), ~ !is.na(.x)))
  if (nrow(rd_data) < nrow(est)) {
    rlang::inform(sprintf(
      "RD-IV %s / %s: excluded %d row(s) with incomplete model inputs.",
      family, dv, nrow(est) - nrow(rd_data)
    ))
  }
  fit <- recursive_demeaning_iv(
    data = rd_data,
    id = "code",
    time = "period",
    outcome = dv,
    endogenous = endogenous,
    exogenous = exogenous,
    cluster = c("code", "period"),
    keep_transformed = FALSE
  )
  model <- "RD-IV + time FE"
  list(
    fit = fit,
    coefficient = fit$coefficients %>%
      dplyr::filter(!grepl("^period_f", term)) %>%
      dplyr::transmute(
        family = family, model = model, dv = dv, term, estimate, std_error, t, p,
        n = fit$diagnostics$transformed_rows,
        fund_fe = TRUE, time_fe = TRUE
      ),
    first_stage = fit$first_stage %>%
      dplyr::mutate(family = family, model = model, dv = dv, .before = 1),
    diagnostics = fit$diagnostics %>%
      dplyr::mutate(family = family, model = model, dv = dv, .before = 1),
    fund_first_stage = fit$fund_first_stage %>%
      dplyr::mutate(family = family, model = model, dv = dv, .before = 1)
  )
}

rd_runs <- list(
  core_gap = run_gap_rd_iv(
    "gap", "Core", "log_pl_fundo",
    intersect(c("log_pl_gestora", "log_age", "period_f"), names(est))
  ),
  core_raw = run_gap_rd_iv(
    "gap_raw", "Core", "log_pl_fundo",
    intersect(c("log_pl_gestora", "log_age", "period_f"), names(est))
  ),
  flow_gap = run_gap_rd_iv(
    "gap", "Flow and scale", c("log_pl_fundo", "flow_rate_lag_10"),
    intersect(c("log_pl_gestora", "log_age", "flow_vol_12_10", "period_f"),
              names(est))
  ),
  flow_raw = run_gap_rd_iv(
    "gap_raw", "Flow and scale", c("log_pl_fundo", "flow_rate_lag_10"),
    intersect(c("log_pl_gestora", "log_age", "flow_vol_12_10", "period_f"),
              names(est))
  )
)

rd_coefficients <- purrr::map_dfr(rd_runs, "coefficient")
rd_first_stage <- purrr::map_dfr(rd_runs, "first_stage")
rd_diagnostics <- purrr::map_dfr(rd_runs, "diagnostics")
rd_fund_first_stage <- purrr::map_dfr(rd_runs, "fund_first_stage")

core_reg <- dplyr::bind_rows(
  core_ols, dplyr::filter(rd_coefficients, family == "Core")
)
flow_reg <- dplyr::bind_rows(
  flow_ols, dplyr::filter(rd_coefficients, family == "Flow and scale")
)
extension_reg <- dplyr::bind_rows(flow_reg, mechanism_reg)
reg_out <- core_reg
regime_reg <- market_reg

readr_write(core_reg, "gap_regressions.csv")
readr_write(market_reg, "gap_regime_regressions.csv")
readr_write(extension_reg, "gap_extension_regressions.csv")
readr_write(rd_coefficients, "gap_rd_iv_estimates.csv")
readr_write(rd_first_stage, "gap_rd_iv_first_stage.csv")
readr_write(rd_diagnostics, "gap_rd_iv_sample.csv")
readr_write(rd_fund_first_stage, "gap_rd_iv_fund_first_stage.csv")

for (family_name in c("Core", "Market states", "Flow and scale", "Mediators")) {
  family_results <- dplyr::bind_rows(core_reg, market_reg, flow_reg, mechanism_reg) %>%
    dplyr::filter(family == family_name)
  cat("\n===", family_name, "models; two-way clustered ===\n")
  print(as.data.frame(family_results), digits = 3)
}
cat("\n=== RD-IV first-stage diagnostics ===\n")
print(as.data.frame(rd_first_stage), digits = 3)

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
  cat("\n=== Market-state model (direction, events, and volatility),",
      "two-way clustered ===\n",
      "    (pooled and fund-FE estimates; no return-window FE)\n")
  for (model_name in unique(regime_reg$model)) {
    for (d in c("gap", "gap_raw")) {
      cat("\n--", model_name, "| DV =", d, "--\n")
      print(as.data.frame(dplyr::select(
        dplyr::filter(regime_reg, model == model_name, dv == d),
        -family, -model, -dv)), digits = 3)
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
  keep_adjusted <- est %>% dplyr::distinct(code, period, mandate)
  fund_meta <- est %>%
    dplyr::distinct(code, mandate, inception)
  if (anyDuplicated(fund_meta$code)) {
    rlang::abort("The adjusted-gap fund universe has non-unique mandate metadata.")
  }

  fund_adjusted_long <- tibble::as_tibble(fund_ra) %>%
    dplyr::mutate(period = ml_index) %>%
    tidyr::pivot_longer(-period, names_to = "code", values_to = "ra_fund") %>%
    dplyr::inner_join(keep_adjusted, by = c("code", "period"))

  # The raw figure uses the SAME fund universe as the adjusted analysis but
  # does not inherit the 36-month factor-adjustment burn-in. This extends the
  # time axis without changing the set of managers being compared.
  fund_raw_long <- tibble::as_tibble(fund_m) %>%
    dplyr::mutate(period = ml_index) %>%
    tidyr::pivot_longer(-period, names_to = "code", values_to = "r_fund") %>%
    dplyr::inner_join(fund_meta, by = "code") %>%
    dplyr::filter(period >= inception) %>%
    dplyr::select(-inception)

  summarise_scheme_gap <- function(rows, scheme, gap_type) {
    fit <- stats::lm(gap ~ 1, data = rows)
    inf <- tidy_clustered(fit, rows$code, rows$period)
    tibble::tibble(
      gap_type  = gap_type,
      scheme    = scheme,
      n         = nrow(rows),
      funds     = dplyr::n_distinct(rows$code),
      periods   = dplyr::n_distinct(rows$period),
      mean_gap  = inf$estimate[[1L]],
      se_gap    = inf$std_error[[1L]],
      pct_ahead = mean(rows$gap > 0),
      t         = inf$t[[1L]],
      p         = inf$p[[1L]]
    )
  }

  scheme_results <- purrr::map(c(ML_SCHEMES, "average"), function(s) {
    # One benchmark column per mandate, so the ladder holds the conditional
    # design fixed and varies only the construction scheme.
    res   <- resolve_ml_benchmark(ml_raw, s)
    ml_ra <- rolling_risk_adjusted(res$mat[, unname(res$cols), drop = FALSE], ym, fac)$ra

    ml_adjusted_long <- tibble::as_tibble(ml_ra) %>%
      dplyr::mutate(period = ml_index) %>%
      tidyr::pivot_longer(-period, names_to = "ml_benchmark", values_to = "ra_ml") %>%
      dplyr::left_join(
        tibble::tibble(mandate = names(res$cols), ml_benchmark = unname(res$cols)),
        by = "ml_benchmark"
      ) %>%
      dplyr::select(period, mandate, ra_ml)

    ml_raw_long <- tibble::as_tibble(
      res$mat[, unname(res$cols), drop = FALSE]
    ) %>%
      dplyr::mutate(period = ml_index) %>%
      tidyr::pivot_longer(
        -period,
        names_to = "ml_benchmark",
        values_to = "r_ml"
      ) %>%
      dplyr::left_join(
        tibble::tibble(
          mandate = names(res$cols),
          ml_benchmark = unname(res$cols)
        ),
        by = "ml_benchmark"
      ) %>%
      dplyr::select(period, mandate, r_ml)

    adjusted_rows <- fund_adjusted_long %>%
      dplyr::left_join(ml_adjusted_long, by = c("period", "mandate")) %>%
      dplyr::filter(!is.na(ra_fund), !is.na(ra_ml)) %>%
      dplyr::mutate(gap = ra_ml - ra_fund)

    raw_rows <- fund_raw_long %>%
      dplyr::left_join(ml_raw_long, by = c("period", "mandate")) %>%
      dplyr::filter(!is.na(r_fund), !is.na(r_ml)) %>%
      dplyr::mutate(gap = r_ml - r_fund)

    list(
      summary = dplyr::bind_rows(
        summarise_scheme_gap(adjusted_rows, s, "Adjusted"),
        summarise_scheme_gap(raw_rows, s, "Raw")
      ),
      raw_time = if (identical(s, "average")) {
        raw_rows %>%
          dplyr::group_by(period) %>%
          dplyr::summarise(
            mean_gap = mean(gap),
            med_gap = stats::median(gap),
            n = dplyr::n(),
            .groups = "drop"
          )
      } else {
        NULL
      },
      raw_panel = if (identical(s, "average")) {
        raw_rows %>%
          dplyr::select(period, code, mandate, r_ml, r_fund, gap) %>%
          dplyr::arrange(code, period)
      } else {
        NULL
      }
    )
  })

  list(
    ladder = purrr::map_dfr(scheme_results, "summary"),
    gap_ts_raw = purrr::map_dfr(scheme_results, "raw_time"),
    gap_raw_panel = purrr::map_dfr(scheme_results, "raw_panel")
  )
}

ladder_result <- tryCatch(build_ladder(), error = function(e) {
  rlang::warn(paste("H7 ladder failed:", conditionMessage(e))); NULL
})
ladder <- if (is.null(ladder_result)) NULL else ladder_result$ladder
gap_ts_raw <- if (is.null(ladder_result)) NULL else ladder_result$gap_ts_raw
gap_raw_panel <- if (is.null(ladder_result)) NULL else ladder_result$gap_raw_panel
if (!is.null(ladder)) {
  cat("\n=== H7: gap by ML construction scheme ===\n")
  print(as.data.frame(ladder), digits = 3)
  readr_write(ladder, "gap_by_scheme.csv")
  readr_write(gap_ts_raw, "gap_raw_over_time_full.csv")
  qs::qsave(
    gap_raw_panel,
    here::here("data", "derived", "gap_raw_fund_month_full.qs"),
    preset = "high"
  )

  scheme_labels <- c(rp = "RP", rp_box = "RP_BOX", hrp = "HRP", cs = "CS",
                     sw = "SW", ew = "EW", mvo = "MVO",
                     mvo_box = "MVO_BOX", average = "Average")
  ladder <- dplyr::mutate(
    ladder,
    scheme_label = unname(scheme_labels[scheme])
  )
  scheme_gap_range <- range(
    c(0, ladder$mean_gap - 1.96 * ladder$se_gap,
      ladder$mean_gap + 1.96 * ladder$se_gap),
    finite = TRUE
  )
  plot_ladder <- function(data, title) {
    ggplot(data, aes(stats::reorder(scheme_label, mean_gap), mean_gap)) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
      geom_pointrange(
        aes(ymin = mean_gap - 1.96 * se_gap,
            ymax = mean_gap + 1.96 * se_gap),
        colour = "#1b6ca8"
      ) +
      coord_flip(ylim = scheme_gap_range) +
      labs(
        title = title,
        subtitle = "Means and 95% intervals with fund and period clustering",
        x = NULL,
        y = "Mean gap (pp per month)"
      )
  }
  p_ladder_adjusted <- plot_ladder(
    dplyr::filter(ladder, gap_type == "Adjusted"),
    "Adjusted gap by construction scheme"
  )
  p_ladder_raw <- plot_ladder(
    dplyr::filter(ladder, gap_type == "Raw"),
    "Raw gap by construction scheme"
  )
  ggsave(file.path(FIG_DIR, "gap_by_scheme.png"), p_ladder_adjusted,
         width = 9, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(FIG_DIR, "gap_raw_by_scheme.png"), p_ladder_raw,
         width = 9, height = 4, dpi = 300, bg = "white")
}


# =============================================================================
# 5. Industry descriptives
# =============================================================================

# --- 5.1 Industry size, omitting missing opening-AUM snapshots ---------------
# `sum_or_zero()` makes an all-missing period explicit in the stored industry
# panel. That policy is useful for flow aggregation but is not an economic AUM
# observation. Twelve endpoints have n_reporting == 0; plotting them as zero
# creates artificial industry collapses. Treat them as missing in descriptives.
industry_aum <- industry %>%
  dplyr::filter(.data$n_reporting > 0L,
                is.finite(.data$industry_pl), .data$industry_pl > 0)

p_size <- ggplot(industry_aum, aes(period, industry_pl / 1e6)) +
  geom_line(linewidth = 0.75, colour = "#1b6ca8") +
  geom_point(data = industry_aum[c(1L, which.max(industry_aum$industry_pl),
                                   nrow(industry_aum)), ],
             size = 1.4, colour = "#1b6ca8") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sample assets under management",
       subtitle = "Dates with no reported opening AUM are treated as missing",
       x = NULL, y = "BRL billions")
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
  labs(title = "Net flows",
       subtitle = "Aggregated over each 15th-to-15th window",
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
  dplyr::filter(!is.na(aum), aum > 0) %>%
  dplyr::group_by(period, bank_affiliated) %>%
  dplyr::summarise(pl = sum(aum), .groups = "drop_last") %>%
  dplyr::mutate(share = pl / sum(pl)) %>%
  dplyr::ungroup()

bank_share_summary <- share %>%
  dplyr::group_by(bank_affiliated) %>%
  dplyr::summarise(mean_share = mean(share), .groups = "drop")
cat("\n=== Mean AUM share by manager affiliation ===\n")
print(as.data.frame(bank_share_summary), digits = 3)

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
  dplyr::filter(!is.na(aum), aum > 0, !is.na(gestora)) %>%
  dplyr::group_by(period, gestora) %>%
  dplyr::summarise(pl = sum(aum), .groups = "drop_last") %>%
  dplyr::mutate(s = pl / sum(pl)) %>%
  dplyr::summarise(hhi = sum(s^2), n_managers = dplyr::n(), .groups = "drop")

p_hhi <- ggplot(hhi_gestora, aes(period, hhi)) +
  geom_line(colour = "#1b6ca8", linewidth = 0.7) +
  labs(title = "Manager concentration",
       subtitle = "HHI of manager-level AUM shares",
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
readr_write(gap_ts, "gap_over_time.csv")

if (!is.null(gap_ts_raw) && nrow(gap_ts_raw) > 0L) {
  time_gap_range <- range(
    c(0, gap_ts$mean_gap, gap_ts_raw$mean_gap),
    finite = TRUE
  )
  plot_gap_time <- function(data, title, subtitle) {
    ggplot(data, aes(period)) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
      geom_col(
        aes(y = mean_gap, fill = mean_gap > 0),
        width = 22,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = c(`TRUE` = "#1b6ca8", `FALSE` = "#c0392b")) +
      coord_cartesian(ylim = time_gap_range) +
      labs(title = title, subtitle = subtitle,
           x = NULL, y = "Percent per month")
  }
  p_gap_raw <- plot_gap_time(
    gap_ts_raw,
    "Raw gap over time",
    paste0(
      "Mandate-matched return difference, ",
      format(min(gap_ts_raw$period), "%b %Y"), "--",
      format(max(gap_ts_raw$period), "%b %Y")
    )
  )
  p_gap_adjusted <- plot_gap_time(
    gap_ts,
    "Adjusted gap over time",
    paste0(
      "Long-only FF6 residual difference, ",
      format(min(gap_ts$period), "%b %Y"), "--",
      format(max(gap_ts$period), "%b %Y")
    )
  )
  ggsave(file.path(FIG_DIR, "gap_over_time.png"), p_gap_adjusted,
         width = 9, height = 4, dpi = 300, bg = "white")
  ggsave(file.path(FIG_DIR, "gap_raw_over_time.png"), p_gap_raw,
         width = 9, height = 4, dpi = 300, bg = "white")
}

if (exists("p_ladder_adjusted", inherits = FALSE) &&
    exists("p_ladder_raw", inherits = FALSE) &&
    exists("p_gap_adjusted", inherits = FALSE) &&
    exists("p_gap_raw", inherits = FALSE)) {
  gap_summary_plot <- cowplot::plot_grid(
    p_gap_raw, p_gap_adjusted, p_ladder_raw, p_ladder_adjusted,
    labels = c("(a)", "(b)", "(c)", "(d)"),
    label_fontface = "bold", label_size = 11,
    ncol = 2, align = "hv", axis = "tblr"
  )
  gap_summary_file <- file.path(FIG_DIR, "gap_summary.png")
  ggsave(gap_summary_file, gap_summary_plot,
         width = 12, height = 8, dpi = 320, bg = "white")

  latex_gap_figure_dir <- here::here("latex_paper", "figures")
  dir.create(latex_gap_figure_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.copy(gap_summary_file,
                 file.path(latex_gap_figure_dir, basename(gap_summary_file)),
                 overwrite = TRUE)) {
    rlang::abort("Could not copy the combined gap figure to latex_paper/figures.")
  }

  gap_summary_figure_tex <- c(
    "\\begin{figure}[p]",
    "\\centering",
    "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
    paste0(
      "\\caption[Human-machine performance gap]",
      "{\\footnotesize \\textbf{: Human-machine performance gap.} ",
      "Panels (a) and (b) plot the average mandate-matched machine-minus-fund gap ",
      "before and after long-only factor adjustment. Panels (c) and (d) report the ",
      "corresponding averages by machine portfolio-construction scheme. Positive ",
      "values favor the machine. Intervals use standard errors clustered by fund ",
      "and return window.}"
    ),
    "\\includegraphics[width=\\linewidth,height=0.80\\textheight,keepaspectratio]{figures/gap_summary.png}",
    "\\label{fig:gap_summary}",
    "\\end{figure}"
  )
  writeLines(gap_summary_figure_tex,
             file.path(FIG_DIR, "gap_summary_figure.tex"))
}

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
# Redemption notice and fee are static fund attributes. Holdings breadth varies
# through time, so use each fund's median valid minimum-implied-holdings bound;
# otherwise funds with more holdings snapshots would receive more histogram
# weight than funds with shorter histories.
median_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else stats::median(x)
}

chars <- est %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(
    redemption_days = dplyr::first(redemption_days),
    expense_ratio   = dplyr::first(expense_ratio),
    n_pos_min       = median_or_na(n_pos_min),
    .groups = "drop"
  )

p_chars <- ggplot(tidyr::pivot_longer(
  dplyr::select(chars, redemption_days, expense_ratio, n_pos_min),
  dplyr::everything(), names_to = "var", values_to = "value") %>% dplyr::filter(!is.na(value)),
  aes(value)) +
  geom_histogram(bins = 30, fill = "#1b6ca8", colour = NA) +
  facet_wrap(~var, ncol = 1, scales = "free", labeller = as_labeller(c(
    redemption_days = "Redemption notice (days)",
    expense_ratio   = "Management fee (% p.a.)",
    n_pos_min       = "Minimum implied holdings"))) +
  labs(title = "Fund characteristics",
       subtitle = "One observation per fund; holdings use the time-series median",
       x = NULL, y = "Funds") +
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        plot.subtitle = element_text(size = 8))
ggsave(file.path(FIG_DIR, "fund_characteristics.png"), p_chars, width = 10, height = 3.6, dpi = 300, bg = "white")

# --- 5.8 Combined fund-industry figure --------------------------------------
fund_industry_plot <- cowplot::plot_grid(
  p_size, p_flow, p_chars, p_hhi,
  labels = c("(a)", "(b)", "(c)", "(d)"),
  label_fontface = "bold", label_size = 11,
  ncol = 2, align = "hv", axis = "tblr",
  rel_heights = c(1, 1.12)
)

fund_industry_file <- file.path(FIG_DIR, "fund_industry_descriptives.png")
ggsave(fund_industry_file, fund_industry_plot,
       width = 11, height = 8.8, dpi = 320, bg = "white")

latex_fund_figure_dir <- here::here("latex_paper", "figures")
dir.create(latex_fund_figure_dir, showWarnings = FALSE, recursive = TRUE)
if (!file.copy(fund_industry_file,
               file.path(latex_fund_figure_dir, basename(fund_industry_file)),
               overwrite = TRUE)) {
  rlang::abort("Could not copy the combined fund-industry figure to latex_paper/figures.")
}

fund_industry_figure_tex <- c(
  "\\begin{figure}[p]",
  "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  "\\caption[Fund sample and industry descriptives]{\\footnotesize \\textbf{: Fund sample and industry descriptives.} Panel (a) reports aggregate opening assets and panel (b) aggregate net subscriptions. Panel (c) summarizes redemption notice, management fees, and the minimum number of holdings implied by partial disclosures. Panel (d) reports the concentration of assets across management companies. Missing asset reports are not treated as zero.}",
  "\\includegraphics[width=\\linewidth,height=0.82\\textheight,keepaspectratio]{figures/fund_industry_descriptives.png}",
  "\\label{fig:fund_industry}",
  "\\end{figure}"
)
writeLines(fund_industry_figure_tex,
           file.path(FIG_DIR, "fund_industry_descriptives_figure.tex"))


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


# =============================================================================
# 7. LaTeX tables for the gap subsection
# =============================================================================
# Tables report t-statistics rather than significance stars. The distinction is
# intentional: these are observational associations with a short time dimension
# and several endogenous or mediating covariates.
tex_cell <- function(estimate, t) {
  if (length(estimate) != 1L || !is.finite(estimate) || !is.finite(t)) return("--")
  sprintf("\\shortstack{%.3f\\\\(%.2f)}", estimate, t)
}

find_reg_cell <- function(tbl, model, dv, term) {
  z <- dplyr::filter(tbl, .data$model == .env$model,
                     .data$dv == .env$dv, .data$term == .env$term)
  if (nrow(z) != 1L) return("--")
  tex_cell(z$estimate[[1L]], z$t[[1L]])
}

make_model_rows <- function(tbl, models, dv, terms, labels) {
  vapply(seq_along(terms), function(i) {
    cells <- vapply(models, function(model) {
      find_reg_cell(tbl, model, dv, terms[[i]])
    }, character(1))
    paste(c(labels[[i]], cells), collapse = " & ") |> paste0(" \\\\")
  }, character(1))
}

make_cross_rows <- function(tbl, columns, terms, labels) {
  vapply(seq_along(terms), function(i) {
    cells <- vapply(columns, function(column) {
      find_reg_cell(tbl, column[[1L]], column[[2L]], terms[[i]])
    }, character(1))
    paste(c(labels[[i]], cells), collapse = " & ") |> paste0(" \\\\")
  }, character(1))
}

make_obs_row <- function(tbl, models, dv) {
  vals <- vapply(models, function(model) {
    z <- dplyr::filter(tbl, .data$model == .env$model, .data$dv == .env$dv)
    if (nrow(z) == 0L || !"n" %in% names(z)) return("--")
    format(z$n[[1L]], big.mark = ",", scientific = FALSE)
  }, character(1))
  paste(c("Observations", vals), collapse = " & ") |> paste0(" \\\\")
}

make_cross_obs_row <- function(tbl, columns) {
  vals <- vapply(columns, function(column) {
    z <- dplyr::filter(tbl, .data$model == column[[1L]],
                       .data$dv == column[[2L]])
    if (nrow(z) == 0L || !"n" %in% names(z)) return("--")
    format(z$n[[1L]], big.mark = ",", scientific = FALSE)
  }, character(1))
  paste(c("Observations", vals), collapse = " & ") |> paste0(" \\\\")
}

first_stage_cell <- function(family, dv, endogenous) {
  z <- dplyr::filter(
    rd_first_stage,
    .data$family == .env$family,
    .data$dv == .env$dv,
    .data$endogenous == .env$endogenous
  )
  if (nrow(z) != 1L || !is.finite(z$f_clustered_wald[[1L]])) return("--")
  sprintf("%.2f%s", z$f_clustered_wald[[1L]],
          if (isTRUE(z$weak_by_f10[[1L]])) "$^{\\dagger}$" else "")
}

first_stage_coefficient_cell <- function(family, dv, endogenous) {
  z <- dplyr::filter(
    rd_first_stage,
    .data$family == .env$family,
    .data$dv == .env$dv,
    .data$endogenous == .env$endogenous
  )
  if (nrow(z) != 1L) return("--")
  tex_cell(z$estimate[[1L]], z$t[[1L]])
}

first_stage_partial_r2_cell <- function(family, dv, endogenous) {
  z <- dplyr::filter(
    rd_first_stage,
    .data$family == .env$family,
    .data$dv == .env$dv,
    .data$endogenous == .env$endogenous
  )
  if (nrow(z) != 1L || !is.finite(z$partial_r2[[1L]])) return("--")
  sprintf("%.4f", z$partial_r2[[1L]])
}

# Unconditional means by benchmark arm.
arm_order <- c("pooled", "ibov", "smll", "idiv")
arm_labels <- c(pooled = "Pooled", ibov = "Broad market",
                smll = "Small cap", idiv = "Dividend")
uncond_rows <- vapply(arm_order, function(a) {
  adj <- dplyr::filter(uncond, arm == a, dv == "gap")
  raw <- dplyr::filter(uncond, arm == a, dv == "gap_raw")
  d <- if (a == "pooled") est else dplyr::filter(est, mandate == a)
  wedge <- mean(d$gap_raw - d$gap)
  paste0(
    arm_labels[[a]], " & ", format(raw$n[[1L]], big.mark = ","), " & ",
    raw$funds[[1L]], " & ", tex_cell(raw$mean_gap[[1L]], raw$t[[1L]]), " & ",
    tex_cell(adj$mean_gap[[1L]], adj$t[[1L]]), " & ", sprintf("%.3f", wedge),
    " \\\\"
  )
}, character(1))

gap_unconditional_tex <- c(
  "\\begin{table}[p]",
  "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  paste0(
    "\\caption[Unconditional human-machine gap]",
    "{\\footnotesize \\textbf{: Unconditional human-machine gap.} ",
    "The raw gap is the mandate-matched machine return minus the fund return. The ",
    "adjusted gap removes exposures estimated from the preceding factor window, and ",
    "the factor wedge is raw minus adjusted. Values are percentage points per month; ",
    "parentheses contain t-statistics clustered by fund and return window.}"
  ),
  "\\footnotesize",
  "\\begin{tabular}{lrrrrr}",
  "\\hline",
  "Benchmark arm & Rows & Funds & Raw gap & Adjusted gap & Factor wedge \\\\",
  "\\hline",
  uncond_rows,
  "\\hline",
  "\\end{tabular}",
  "\\label{gapmeans}",
  "\\end{table}"
)
writeLines(gap_unconditional_tex, file.path(TAB_DIR, "gap_unconditional.tex"))

# Referee-facing tables use estimator names instead of implying that models with
# different identifying variation are interchangeable sequential specifications.
robust_models <- c("Pooled OLS", "Time FE OLS", "Fund + time FE OLS",
                   "RD-IV + time FE")
core_terms <- c("(Intercept)", "log_pl_fundo", "log_pl_gestora",
                "redemption_days", "expense_ratio", "log_age", "bank", "retail")
core_labels <- c("Mean-characteristic intercept", "Log fund AUM",
                 "Log manager AUM", "Redemption notice (days)",
                 "Management fee (pp p.a.)", "Log fund age",
                 "Bank affiliated", "General-investor audience")

core_table <- c(
  "\\begin{table}[p]", "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  paste0(
    "\\caption[Core human-machine gap regressions]",
    "{\\footnotesize \\textbf{: Core human-machine gap regressions.} ",
    "Panels report pooled OLS, return-window fixed effects, fund and return-window ",
    "fixed effects, and recursive-demeaning IV estimates for adjusted and raw gaps. ",
    "Static fund controls are absorbed by fund fixed effects. The reported first-stage ",
    "Wald statistic is a clustered diagnostic rather than a conventional weak-IV ",
    "test. Parentheses contain t-statistics clustered by fund and return window.}"
  ),
  "\\fontsize{7}{8}\\selectfont", "\\begin{tabular}{lrrrr}", "\\hline",
  " & Pooled OLS & Time FE & Fund + time FE & RD-IV + time FE \\\\",
  "\\hline", "\\multicolumn{5}{l}{\\textit{Panel A: Adjusted gap}} \\\\",
  make_model_rows(core_reg, robust_models, "gap", core_terms, core_labels),
  "Fund fixed effects & No & No & Yes & Implicit \\\\",
  "Return-window fixed effects & No & Yes & Yes & Yes \\\\",
  "Static fund controls & Yes & Yes & Absorbed & Absorbed \\\\",
  paste0("First-stage coefficient: fund AUM & -- & -- & -- & ",
         first_stage_coefficient_cell("Core", "gap", "log_pl_fundo"), " \\\\"),
  paste0("First-stage partial $R^2$: fund AUM & -- & -- & -- & ",
         first_stage_partial_r2_cell("Core", "gap", "log_pl_fundo"), " \\\\"),
  paste0("First-stage Wald-F proxy: fund AUM & -- & -- & -- & ",
         first_stage_cell("Core", "gap", "log_pl_fundo"), " \\\\"),
  make_obs_row(core_reg, robust_models, "gap"),
  "\\hline", "\\multicolumn{5}{l}{\\textit{Panel B: Raw gap}} \\\\",
  make_model_rows(core_reg, robust_models, "gap_raw", core_terms, core_labels),
  "Fund fixed effects & No & No & Yes & Implicit \\\\",
  "Return-window fixed effects & No & Yes & Yes & Yes \\\\",
  "Static fund controls & Yes & Yes & Absorbed & Absorbed \\\\",
  paste0("First-stage coefficient: fund AUM & -- & -- & -- & ",
         first_stage_coefficient_cell("Core", "gap_raw", "log_pl_fundo"), " \\\\"),
  paste0("First-stage partial $R^2$: fund AUM & -- & -- & -- & ",
         first_stage_partial_r2_cell("Core", "gap_raw", "log_pl_fundo"), " \\\\"),
  paste0("First-stage Wald-F proxy: fund AUM & -- & -- & -- & ",
         first_stage_cell("Core", "gap_raw", "log_pl_fundo"), " \\\\"),
  make_obs_row(core_reg, robust_models, "gap_raw"),
  "\\hline", "\\end{tabular}", "\\label{gapregs}", "\\end{table}"
)
writeLines(core_table, file.path(TAB_DIR, "gap_core_regressions.tex"))

market_columns <- list(c("Pooled OLS", "gap"), c("Fund FE OLS", "gap"),
                       c("Pooled OLS", "gap_raw"), c("Fund FE OLS", "gap_raw"))
market_terms <- c("mkt_ret", "bear", "crisis_fcovid", "crisis_fratehike",
                  "vol_regime_fvol_low", "vol_regime_fvol_high")
market_labels <- c("Mandate-index return", "Down-market indicator",
                   "COVID window", "Rate-hike window",
                   "Low-volatility tercile", "High-volatility tercile")
market_table <- c(
  "\\begin{table}[p]", "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  paste0(
    "\\caption[Gap regressions by market state]",
    "{\\footnotesize \\textbf{: Gap regressions by market state.} ",
    "The common specification includes the mandate-index return, a down-market ",
    "indicator, event windows, and mandate-specific volatility terciles. Pooled and ",
    "fund-fixed-effects estimates are shown for adjusted and raw gaps; return-window ",
    "fixed effects are omitted because they would absorb the common state variables. ",
    "Parentheses contain t-statistics clustered by fund and return window.}"
  ),
  "\\scriptsize", "\\begin{tabular}{lrrrr}", "\\hline",
  " & \\multicolumn{2}{c}{Adjusted gap} & \\multicolumn{2}{c}{Raw gap} \\\\",
  " & Pooled OLS & Fund FE & Pooled OLS & Fund FE \\\\", "\\hline",
  make_cross_rows(market_reg, market_columns, market_terms, market_labels),
  "Fund fixed effects & No & Yes & No & Yes \\\\",
  "Return-window fixed effects & No & No & No & No \\\\",
  "Static fund controls & Yes & Absorbed & Yes & Absorbed \\\\",
  make_cross_obs_row(market_reg, market_columns),
  "\\hline", "\\end{tabular}", "\\label{gapstates}", "\\end{table}"
)
writeLines(market_table, file.path(TAB_DIR, "gap_market_states.tex"))
writeLines(c("\\input{../tables/gap_core_regressions.tex}",
             "\\input{../tables/gap_market_states.tex}"),
           file.path(TAB_DIR, "gap_regressions.tex"))

flow_terms <- c("(Intercept)", "flow_rate_lag_10", "flow_vol_12_10",
                "log_pl_fundo", "log_pl_gestora", "expense_ratio", "retail")
flow_labels <- c("Mean-characteristic intercept", "Lagged net flow (10 pp)",
                 "Flow volatility (10 pp)", "Log fund AUM", "Log manager AUM",
                 "Management fee (pp p.a.)", "General-investor audience")
flow_table <- c(
  "\\begin{table}[p]", "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  paste0(
    "\\caption[Flow and scale gap regressions]",
    "{\\footnotesize \\textbf{: Flow and scale gap regressions.} ",
    "Reports pooled, fixed-effects, and recursive-demeaning IV estimates with opening ",
    "fund assets, lagged net flow, manager assets, and flow volatility. The IV column ",
    "treats fund assets and lagged flow as jointly endogenous and reports clustered ",
    "first-stage diagnostics. Parentheses contain t-statistics clustered by fund ",
    "and return window.}"
  ),
  "\\fontsize{7}{8}\\selectfont", "\\begin{tabular}{lrrrr}", "\\hline",
  " & Pooled OLS & Time FE & Fund + time FE & RD-IV + time FE \\\\",
  "\\hline", "\\multicolumn{5}{l}{\\textit{Panel A: Adjusted gap}} \\\\",
  make_model_rows(flow_reg, robust_models, "gap", flow_terms, flow_labels),
  "Fund fixed effects & No & No & Yes & Implicit \\\\",
  "Return-window fixed effects & No & Yes & Yes & Yes \\\\",
  "Static fund controls & Yes & Yes & Absorbed & Absorbed \\\\",
  paste0("First-stage coefficient: fund AUM & -- & -- & -- & ",
         first_stage_coefficient_cell("Flow and scale", "gap", "log_pl_fundo"),
         " \\\\"),
  paste0("First-stage partial $R^2$: fund AUM & -- & -- & -- & ",
         first_stage_partial_r2_cell("Flow and scale", "gap", "log_pl_fundo"),
         " \\\\"),
  paste0("First-stage Wald-F proxy: fund AUM & -- & -- & -- & ",
         first_stage_cell("Flow and scale", "gap", "log_pl_fundo"), " \\\\"),
  paste0("First-stage coefficient: lagged flow & -- & -- & -- & ",
         first_stage_coefficient_cell("Flow and scale", "gap", "flow_rate_lag_10"),
         " \\\\"),
  paste0("First-stage partial $R^2$: lagged flow & -- & -- & -- & ",
         first_stage_partial_r2_cell("Flow and scale", "gap", "flow_rate_lag_10"),
         " \\\\"),
  paste0("First-stage Wald-F proxy: lagged flow & -- & -- & -- & ",
         first_stage_cell("Flow and scale", "gap", "flow_rate_lag_10"), " \\\\"),
  make_obs_row(flow_reg, robust_models, "gap"),
  "\\hline", "\\multicolumn{5}{l}{\\textit{Panel B: Raw gap}} \\\\",
  make_model_rows(flow_reg, robust_models, "gap_raw", flow_terms, flow_labels),
  "Fund fixed effects & No & No & Yes & Implicit \\\\",
  "Return-window fixed effects & No & Yes & Yes & Yes \\\\",
  "Static fund controls & Yes & Yes & Absorbed & Absorbed \\\\",
  paste0("First-stage coefficient: fund AUM & -- & -- & -- & ",
         first_stage_coefficient_cell("Flow and scale", "gap_raw", "log_pl_fundo"),
         " \\\\"),
  paste0("First-stage partial $R^2$: fund AUM & -- & -- & -- & ",
         first_stage_partial_r2_cell("Flow and scale", "gap_raw", "log_pl_fundo"),
         " \\\\"),
  paste0("First-stage Wald-F proxy: fund AUM & -- & -- & -- & ",
         first_stage_cell("Flow and scale", "gap_raw", "log_pl_fundo"), " \\\\"),
  paste0("First-stage coefficient: lagged flow & -- & -- & -- & ",
         first_stage_coefficient_cell("Flow and scale", "gap_raw", "flow_rate_lag_10"),
         " \\\\"),
  paste0("First-stage partial $R^2$: lagged flow & -- & -- & -- & ",
         first_stage_partial_r2_cell("Flow and scale", "gap_raw", "flow_rate_lag_10"),
         " \\\\"),
  paste0("First-stage Wald-F proxy: lagged flow & -- & -- & -- & ",
         first_stage_cell("Flow and scale", "gap_raw", "flow_rate_lag_10"), " \\\\"),
  make_obs_row(flow_reg, robust_models, "gap_raw"),
  "\\hline", "\\end{tabular}", "\\label{gapextensions}", "\\end{table}"
)
writeLines(flow_table, file.path(TAB_DIR, "gap_flow_scale.tex"))

mediator_models <- c("Pooled OLS", "Time FE OLS", "Fund + time FE OLS")
mediator_terms <- c("(Intercept)", "resid_vol_12", "rho_machine_10",
                    "hhi_top15_01", "expense_ratio", "retail")
mediator_labels <- c("Mean-characteristic intercept",
                     "Trailing residual volatility (1 pp)",
                     "Residual correlation with machine (0.10)",
                     "Top-holdings HHI (0.01)", "Management fee (pp p.a.)",
                     "General-investor audience")
mediator_table <- c(
  "\\begin{table}[p]", "\\centering",
  "\\captionsetup{justification=raggedright,labelsep=none,singlelinecheck=false,labelformat=boldlabel}",
  paste0(
    "\\caption[Descriptive mediator regressions]",
    "{\\footnotesize \\textbf{: Descriptive mediator regressions.} ",
    "Reports pooled and fixed-effects gap regressions with lagged residual volatility, ",
    "residual correlation with the matched machine, and concentration of disclosed ",
    "holdings. These variables may be outcomes of portfolio choice, so coefficients ",
    "are interpreted descriptively. Parentheses contain t-statistics clustered by ",
    "fund and return window.}"
  ),
  "\\scriptsize", "\\begin{tabular}{lrrr}", "\\hline",
  " & Pooled OLS & Time FE & Fund + time FE \\\\",
  "\\hline", "\\multicolumn{4}{l}{\\textit{Panel A: Adjusted gap}} \\\\",
  make_model_rows(mechanism_reg, mediator_models, "gap", mediator_terms,
                  mediator_labels),
  "Fund fixed effects & No & No & Yes \\\\",
  "Return-window fixed effects & No & Yes & Yes \\\\",
  make_obs_row(mechanism_reg, mediator_models, "gap"),
  "\\hline", "\\multicolumn{4}{l}{\\textit{Panel B: Raw gap}} \\\\",
  make_model_rows(mechanism_reg, mediator_models, "gap_raw", mediator_terms,
                  mediator_labels),
  "Fund fixed effects & No & No & Yes \\\\",
  "Return-window fixed effects & No & Yes & Yes \\\\",
  make_obs_row(mechanism_reg, mediator_models, "gap_raw"),
  "\\hline", "\\end{tabular}", "\\label{gapmediators}", "\\end{table}"
)
writeLines(mediator_table, file.path(TAB_DIR, "gap_mediators.tex"))
writeLines(c("\\input{../tables/gap_flow_scale.tex}",
             "\\input{../tables/gap_mediators.tex}"),
           file.path(TAB_DIR, "gap_extensions.tex"))

cat("\nFigures written to figures/, tables to tables/.\n")

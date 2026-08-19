# Integration checks on the BUILT panel.
#
# These are not unit tests of individual helpers -- they assert properties the
# assembled artefact must satisfy no matter how it was produced. Every check
# below corresponds to a defect actually observed in the 2026-08-04 build, or
# to an identity that cannot be violated without a real bug upstream.
#
# The panel is read from data/derived/gap_panel.qs if present. When it is
# absent the file skips rather than fails, so the unit suite still runs on a
# machine that has never done a full build.

read_built_panel <- function() {
  path <- file.path(project_root, "data", "derived", "gap_panel.qs")
  if (!file.exists(path)) testthat::skip("data/derived/gap_panel.qs not built yet.")
  if (!requireNamespace("qs", quietly = TRUE)) testthat::skip("qs not installed.")
  qs::qread(path)
}

read_built_industry <- function() {
  path <- file.path(project_root, "data", "derived", "industry_panel.qs")
  if (!file.exists(path)) testthat::skip("data/derived/industry_panel.qs not built yet.")
  qs::qread(path)
}

# Report the offending rows rather than just a count -- a bare FALSE tells you
# nothing about which fund broke the invariant.
expect_none <- function(flag, label, keys = NULL) {
  bad <- which(isTRUE_vec(flag))
  if (length(bad) && !is.null(keys)) {
    msg <- paste(utils::capture.output(print(utils::head(keys[bad, ], 5))), collapse = "\n")
    testthat::expect(FALSE, sprintf("%s: %d row(s). First offenders:\n%s",
                                    label, length(bad), msg))
  } else {
    testthat::expect(length(bad) == 0L, sprintf("%s: %d row(s).", label, length(bad)))
  }
  invisible(TRUE)
}

# NA is never a violation here: missing is a legitimate state, impossible is not.
isTRUE_vec <- function(x) !is.na(x) & x


test_that("panel keys are unique and well formed", {
  p <- read_built_panel()
  keys <- p[, c("code", "period")]

  expect_equal(sum(duplicated(keys)), 0L)
  expect_false(any(is.na(p$code)))
  expect_false(any(is.na(p$period)))
  expect_true(all(format(p$period, "%d") == "15"))
})


test_that("strictly non-negative quantities are non-negative", {
  p <- read_built_panel()
  keys <- p[, c("code", "period")]

  expect_none(p$aum < 0,             "aum < 0", keys)
  expect_none(p$n_investors < 0,     "n_investors < 0", keys)
  expect_none(p$pl_gestora < 0,      "pl_gestora < 0", keys)
  expect_none(p$redemption_days < 0, "redemption_days < 0", keys)
  expect_none(p$n_ranks < 1,         "n_ranks < 1", keys)
  expect_none(p$gestora_n_funds < 1, "gestora_n_funds < 1", keys)
  expect_none(p$slope_vol_12 < 0,    "slope_vol_12 < 0", keys)
  expect_none(p$flow_vol_12 < 0,     "flow_vol_12 < 0", keys)
})


test_that("screened variables respect their documented bounds", {
  p <- read_built_panel()
  keys <- p[, c("code", "period")]

  # SCREEN 2 -- 750% fees must be gone.
  expect_none(p$expense_ratio > MAX_EXPENSE_RATIO, "expense_ratio above cap", keys)
  expect_none(p$expense_ratio < 0,                 "expense_ratio < 0", keys)

  # SCREEN 1 -- the 3.4e10 blowup must be gone, in both level and volatility.
  expect_none(abs(p$flow_rate) > FLOW_RATE_CAP,     "flow_rate beyond cap", keys)
  expect_none(abs(p$flow_rate_lag) > FLOW_RATE_CAP, "flow_rate_lag beyond cap", keys)
  expect_none(p$flow_vol_12 > FLOW_RATE_CAP,        "flow_vol_12 beyond cap", keys)

  # SCREEN 3 -- HHI cannot exceed 1 for a book whose weights sum to NAV, but the
  # NAV gate deliberately tolerates TOPX_NAV_TOLERANCE_PCT of rounding/stale
  # pricing, so the reachable ceiling is (1 + tol/100)^2. 56 fund-periods sit in
  # that band with top-15 long exposure of 100.4-101.4%. Asserting the true
  # ceiling keeps the tolerance honest instead of renormalising the weights,
  # which would silently change what the measure means.
  hhi_ceiling <- (1 + TOPX_NAV_TOLERANCE_PCT / 100)^2
  expect_none(p$hhi_top15 > hhi_ceiling, "hhi_top15 above tolerance ceiling", keys)
  expect_none(p$hhi_top15 < 0, "hhi_top15 < 0", keys)

  expect_none(p$n_ranks > TOPX_MAX, "n_ranks above TOPX_MAX", keys)
  expect_none(p$resid_weight < 0,   "resid_weight < 0", keys)
  expect_none(p$resid_weight > 100, "resid_weight > 100", keys)
})


test_that("cross-variable identities hold", {
  p <- read_built_panel()
  keys <- p[, c("code", "period")]

  # log transforms must agree with their levels, and must be NA (never a
  # sentinel like log(1e-8) = -18.42) when the level is zero.
  ok <- !is.na(p$aum) & p$aum > 0
  expect_equal(p$log_pl_fundo[ok], log(p$aum[ok]), tolerance = 1e-9)
  expect_none(!is.na(p$log_pl_fundo) & p$aum <= 0,
              "log_pl_fundo defined at non-positive aum", keys)

  okg <- !is.na(p$pl_gestora) & p$pl_gestora > 0
  expect_equal(p$log_pl_gestora[okg], log(p$pl_gestora[okg]), tolerance = 1e-9)

  # Cumulative top-k weights are monotone in k ONLY when every weight is
  # positive. The gate is has_negative_line (ANY negative), not has_short
  # (negatives at or beyond SHORT_MATERIALITY_PCT): a -0.27% bookkeeping entry
  # is immaterial for the concentration bounds but still breaks monotonicity by
  # exactly that amount. Zero breaches occur among strictly positive books.
  m <- !is.na(p$w_top1) & !is.na(p$w_top5) & !is.na(p$w_top10) &
    !is.na(p$w_cum_15) & !p$has_negative_line
  expect_none(m & p$w_top5   < p$w_top1  - 1e-6, "w_top5 < w_top1 (long-only)", keys)
  expect_none(m & p$w_top10  < p$w_top5  - 1e-6, "w_top10 < w_top5 (long-only)", keys)
  expect_none(m & p$w_cum_15 < p$w_top10 - 1e-6, "w_cum_15 < w_top10 (long-only)", keys)

  # A single squared weight cannot exceed the sum of squares.
  h <- !is.na(p$hhi_top15) & !is.na(p$w_top1)
  expect_none(h & p$hhi_top15 < (p$w_top1 / 100)^2 - 1e-9,
              "hhi_top15 below its own largest term", keys)

  # eff_bets_max is defined as 1/HHI. Its floor is the reciprocal of the HHI
  # ceiling, not 1, for the tolerance reason documented above.
  e <- !is.na(p$eff_bets_max) & !is.na(p$hhi_top15)
  expect_equal(p$eff_bets_max[e], 1 / p$hhi_top15[e], tolerance = 1e-9)
  expect_none(p$eff_bets_max < 1 / (1 + TOPX_NAV_TOLERANCE_PCT / 100)^2,
              "eff_bets_max below tolerance floor", keys)

  # The minimum-position bound cannot be below the ranks actually observed.
  n <- !is.na(p$n_pos_min) & !is.na(p$n_ranks)
  expect_none(n & p$n_pos_min < p$n_ranks, "n_pos_min below n_ranks", keys)

  # A firm cannot be smaller than one of its own funds.
  g <- !is.na(p$pl_gestora) & !is.na(p$aum)
  expect_none(g & p$pl_gestora < p$aum - 1e-6, "pl_gestora below member fund aum", keys)

  # Reporting funds are a subset of the firm's funds.
  expect_none(p$n_funds_reporting > p$gestora_n_funds,
              "n_funds_reporting exceeds gestora_n_funds", keys)
})


test_that("the estimation sample is temporally coherent", {
  p <- read_built_panel()
  keys <- p[, c("code", "period")]
  s <- p$in_main_sample

  # The panel is a full cross-join, so pre-inception rows exist by design --
  # but none of them may reach the estimation sample, and no age may be
  # negative there.
  expect_none(isTRUE_vec(s) & p$period < p$inception,
              "pre-inception row inside in_main_sample", keys)
  expect_none(isTRUE_vec(s) & p$fund_age_yrs < 0,
              "negative fund age inside in_main_sample", keys)
  expect_none(isTRUE_vec(s) & p$gestora_age_yrs < 0,
              "negative gestora age inside in_main_sample", keys)

  # in_main_sample must mean exactly what it says.
  expect_none(isTRUE_vec(s) & p$is_feeder, "feeder inside in_main_sample", keys)
  expect_none(isTRUE_vec(s) & p$is_fof,    "fund-of-funds inside in_main_sample", keys)
  expect_none(isTRUE_vec(s) & is.na(p$aum), "missing aum inside in_main_sample", keys)
})


test_that("the dependent variable is present, consistent, and free of look-ahead", {
  p <- read_built_panel()
  keys <- p[, c("code", "period")]

  expect_true(all(c("gap", "ra_fund", "ra_ml") %in% names(p)))

  d <- !is.na(p$gap)
  expect_gt(sum(d), 0L)
  expect_equal(p$gap[d], (p$ra_ml - p$ra_fund)[d], tolerance = 1e-9)

  # Betas are fitted on a strictly trailing window, so no DV can exist before
  # ROLL_WINDOW periods have elapsed.
  first_period <- min(p$period)
  earliest_ok  <- sort(unique(p$period))[ROLL_WINDOW + 1L]
  expect_none(d & p$period < earliest_ok,
              "DV present before the trailing window completes", keys)

  # CONDITIONAL BENCHMARK. The gap is no longer common within a period: each
  # fund is differenced against the machine run under its own mandate, so
  # ml_benchmark takes one value PER MANDATE and ra_ml is common only within
  # (period, mandate). A single distinct benchmark here would mean the mandate
  # routing silently collapsed back to one machine.
  expect_true(all(stats::na.omit(p$mandate) %in% ML_MANDATES))
  expect_equal(sort(unique(stats::na.omit(p$ml_benchmark[d]))),
               sort(unique(paste0("average_", p$mandate[d]))))

  # ml_benchmark must be a function of mandate alone -- one column per mandate,
  # and no mandate served by two different columns.
  route <- unique(data.frame(mandate = p$mandate[d], col = p$ml_benchmark[d]))
  expect_equal(sum(duplicated(route$mandate)), 0L)
  expect_equal(sum(duplicated(route$col)), 0L)

  # The machine residual is common within (period, mandate).
  per_cell <- tapply(p$ra_ml[d], paste(p$period[d], p$mandate[d]),
                     function(x) length(unique(round(x, 12))))
  expect_true(all(per_cell == 1L))

  # ...and genuinely differs ACROSS mandates. If every mandate produced the same
  # ra_ml the conditional design would be doing no work, which would point at
  # four identical cohorts rather than four mandates.
  if (dplyr::n_distinct(p$mandate[d]) > 1L) {
    spread <- tapply(p$ra_ml[d], p$period[d], function(x) diff(range(x)))
    expect_true(max(spread, na.rm = TRUE) > 1e-8)
  }

  # rho_machine is a correlation.
  expect_none(abs(p$rho_machine) > 1 + 1e-9, "rho_machine outside [-1, 1]", keys)
})


test_that("industry aggregates are coherent and coverage is carried", {
  ind <- read_built_industry()

  expect_equal(sum(duplicated(ind$period)), 0L)
  expect_true(all(ind$industry_pl >= 0, na.rm = TRUE))
  expect_true(all(ind$n_reporting >= 0))

  # SCREEN 4 -- the balanced series must exist and must never exceed the full
  # one, since it sums a strict subset of the same funds.
  expect_true("industry_pl_balanced" %in% names(ind))
  b <- !is.na(ind$industry_pl_balanced)
  expect_true(all(ind$industry_pl_balanced[b] <= ind$industry_pl[b] + 1e-6))
  expect_true(all(ind$n_balanced[b] <= ind$n_reporting[b]))

  # Cumulative flow is a running sum of the per-period flow.
  expect_equal(ind$industry_flow_cum,
               cumsum(dplyr::coalesce(ind$industry_net_flow, 0)),
               tolerance = 1e-6)
})

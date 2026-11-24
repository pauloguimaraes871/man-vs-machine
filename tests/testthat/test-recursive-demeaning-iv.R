testthat::local_edition(3)

source(file.path(project_root, "R", "recursive_demeaning_iv.R"), local = TRUE)

make_rd_panel <- function(unbalanced = FALSE) {
  panel <- data.frame(
    fund = rep(c("A", "B", "C"), each = 4L),
    period = rep(as.Date("2020-01-01") + 0:3, times = 3L),
    size = c(1, 3, 2, 5, 5, 2, 4, 1, 2, 6, 3, 7),
    stringsAsFactors = FALSE
  )
  panel$control <- rep(c(0, 1, 0, 1), times = 3L)
  panel$fund_effect <- rep(c(10, -4, 7), each = 4L)
  panel$return <- panel$fund_effect + 2 * panel$size + 0.5 * panel$control
  panel$cluster_time <- panel$period
  if (unbalanced) panel <- panel[-c(2L, 11L), ]
  panel
}

suppress_weak_first_stage <- function(expr) {
  withCallingHandlers(
    expr,
    rd_iv_weak_first_stage = function(warning) invokeRestart("muffleWarning")
  )
}

testthat::test_that("recursive transformations match hand calculations", {
  x <- matrix(c(1, 3, 2, 5), ncol = 1L)
  testthat::expect_equal(
    as.numeric(rd_iv_backward_demean(x)),
    c(NA, 2, 0, 3),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    as.numeric(rd_iv_forward_demean(x)),
    c(-1.75, -1 / 3, -1.5, 0),
    tolerance = 1e-12
  )
})

testthat::test_that("the just-identified estimate equals equation 10", {
  panel <- make_rd_panel()
  panel$return <- panel$fund_effect + 2 * panel$size
  fit <- suppress_weak_first_stage(
    recursive_demeaning_iv(
      panel, id = "fund", time = "period", outcome = "return",
      endogenous = "size", cluster = c("fund", "cluster_time")
    )
  )
  transformed <- fit$transformed
  numerator <- sum(transformed$return__fd * transformed$size__bd)
  denominator <- sum(transformed$size__fd * transformed$size__bd)

  testthat::expect_equal(
    fit$coefficients$estimate[fit$coefficients$term == "size"],
    numerator / denominator,
    tolerance = 1e-12
  )
  testthat::expect_equal(numerator / denominator, 2, tolerance = 1e-12)
  testthat::expect_false("(Intercept)" %in% colnames(fit$design$regressors))
  testthat::expect_false("(Intercept)" %in% colnames(fit$design$instruments))
})

testthat::test_that("forward-demeaned exogenous controls instrument themselves", {
  panel <- make_rd_panel()
  fit <- suppress_weak_first_stage(
    recursive_demeaning_iv(
      panel, id = "fund", time = "period", outcome = "return",
      endogenous = "size", exogenous = "control",
      cluster = c("fund", "cluster_time")
    )
  )

  estimates <- stats::setNames(fit$coefficients$estimate, fit$coefficients$term)
  testthat::expect_equal(unname(estimates[["size"]]), 2, tolerance = 1e-10)
  testthat::expect_equal(unname(estimates[["control"]]), 0.5, tolerance = 1e-10)
  testthat::expect_true("control" %in% colnames(fit$design$regressors))
  testthat::expect_true("control" %in% colnames(fit$design$instruments))
})

testthat::test_that("unbalanced panels use each fund's observed history", {
  panel <- make_rd_panel(unbalanced = TRUE)
  fit <- suppress_weak_first_stage(
    recursive_demeaning_iv(
      panel, id = "fund", time = "period", outcome = "return",
      endogenous = "size", exogenous = "control",
      cluster = c("fund", "cluster_time")
    )
  )

  testthat::expect_true(fit$diagnostics$unbalanced_panel)
  testthat::expect_equal(fit$diagnostics$transformed_rows,
                         nrow(panel) - length(unique(panel$fund)))
  testthat::expect_equal(fit$coefficients$estimate[fit$coefficients$term == "size"],
                         2, tolerance = 1e-10)
})

testthat::test_that("unsorted inputs are explicitly reordered", {
  panel <- make_rd_panel()
  unsorted <- panel[c(12:1), ]
  testthat::expect_message(
    fit <- suppress_weak_first_stage(
      recursive_demeaning_iv(
        unsorted, id = "fund", time = "period", outcome = "return",
        endogenous = "size", cluster = c("fund", "cluster_time")
      )
    ),
    "reordered explicitly"
  )
  testthat::expect_false(fit$diagnostics$input_was_sorted)
  testthat::expect_true(all(order(fit$transformed$fund, fit$transformed$period) ==
                            seq_len(nrow(fit$transformed))))
})

testthat::test_that("invalid panel inputs fail early", {
  panel <- make_rd_panel()
  duplicate <- rbind(panel, panel[1L, ])
  testthat::expect_error(
    recursive_demeaning_iv(duplicate, "fund", "period", "return", "size"),
    "duplicated id-time"
  )

  missing <- panel
  missing$size[2L] <- NA_real_
  testthat::expect_error(
    recursive_demeaning_iv(missing, "fund", "period", "return", "size"),
    "must not contain missing"
  )

  nonfinite <- panel
  nonfinite$return[2L] <- Inf
  testthat::expect_error(
    recursive_demeaning_iv(nonfinite, "fund", "period", "return", "size"),
    "only finite"
  )
})

testthat::test_that("one-period funds are reported and excluded", {
  panel <- make_rd_panel()
  singleton <- panel[1L, ]
  singleton$fund <- "D"
  panel <- rbind(panel, singleton)

  testthat::expect_warning(
    fit <- suppress_weak_first_stage(
      recursive_demeaning_iv(
        panel, id = "fund", time = "period", outcome = "return",
        endogenous = "size", cluster = c("fund", "cluster_time")
      )
    ),
    "fewer than two observations"
  )
  testthat::expect_equal(fit$diagnostics$dropped_short_ids, 1L)
  testthat::expect_false("D" %in% fit$transformed$fund)
})

testthat::test_that("lack of transformed instrument variation is rejected", {
  panel <- make_rd_panel()
  panel$size <- rep(c(1, 1, 1, 1), times = 3L)
  testthat::expect_error(
    recursive_demeaning_iv(
      panel, id = "fund", time = "period", outcome = "return",
      endogenous = "size", cluster = c("fund", "cluster_time")
    ),
    "rank deficient"
  )
})

testthat::test_that("fund-level first-stage diagnostics are complete", {
  panel <- make_rd_panel()
  fit <- suppress_weak_first_stage(
    recursive_demeaning_iv(
      panel, id = "fund", time = "period", outcome = "return",
      endogenous = "size", cluster = c("fund", "cluster_time")
    )
  )
  testthat::expect_setequal(fit$fund_first_stage$id, unique(panel$fund))
  testthat::expect_named(
    fit$fund_first_stage,
    c("id", "observations", "slope_no_intercept",
      "intercept_with_intercept", "slope_with_intercept", "endogenous")
  )
  testthat::expect_true(all(is.finite(fit$first_stage$f_clustered_wald)))
  testthat::expect_true(all(is.finite(fit$first_stage$partial_r2)))
  testthat::expect_equal(fit$first_stage$fund_slopes_available, 3L)
})

testthat::test_that("redundant transformed factor controls are dropped explicitly", {
  panel <- make_rd_panel()
  panel$period_factor <- factor(panel$period)
  testthat::expect_warning(
    fit <- suppress_weak_first_stage(
      recursive_demeaning_iv(
        panel, id = "fund", time = "period", outcome = "return",
        endogenous = "size", exogenous = "period_factor",
        cluster = c("fund", "cluster_time")
      )
    ),
    "exactly collinear"
  )
  testthat::expect_true(nzchar(fit$diagnostics$dropped_collinear_exogenous))
  testthat::expect_equal(qr(fit$design$regressors)$rank,
                         ncol(fit$design$regressors))
  testthat::expect_equal(qr(fit$design$instruments)$rank,
                         ncol(fit$design$instruments))
})

testthat::test_that("multiple endogenous regressors are projected jointly", {
  panel <- make_rd_panel()
  panel$flow <- c(2, 1, 4, 3, 1, 5, 2, 6, 4, 2, 7, 1)
  panel$return <- panel$fund_effect + 2 * panel$size - 3 * panel$flow +
    0.5 * panel$control

  fit <- suppressWarnings(
    suppress_weak_first_stage(
      recursive_demeaning_iv(
        panel, id = "fund", time = "period", outcome = "return",
        endogenous = c("size", "flow"), exogenous = "control",
        cluster = c("fund", "cluster_time")
      )
    )
  )

  estimates <- stats::setNames(fit$coefficients$estimate, fit$coefficients$term)
  testthat::expect_equal(unname(estimates[["size"]]), 2, tolerance = 1e-10)
  testthat::expect_equal(unname(estimates[["flow"]]), -3, tolerance = 1e-10)
  testthat::expect_equal(unname(estimates[["control"]]), 0.5, tolerance = 1e-10)
  testthat::expect_setequal(fit$first_stage$endogenous, c("size", "flow"))
  testthat::expect_equal(ncol(fit$design$projected_regressors),
                         ncol(fit$design$regressors))
  testthat::expect_true(all(c("size__backward_instrument",
                              "flow__backward_instrument") %in%
                            colnames(fit$design$instruments)))

  z <- fit$design$instruments
  x <- fit$design$regressors
  x_hat <- fit$design$projected_regressors
  for (endogenous_index in seq_len(2L)) {
    first_stage_lm <- stats::lm.fit(x = z, y = x[, endogenous_index])
    testthat::expect_equal(
      unname(x_hat[, endogenous_index]),
      unname(first_stage_lm$fitted.values),
      tolerance = 1e-10
    )
  }
  # Because fitted values are projections onto Z, Xhat'X and Xhat'Xhat are
  # numerically identical. The former is the 2SLS moment-equation notation;
  # neither expression substitutes the original endogenous variables for their
  # first-stage fitted values on the instrument side of the normal equations.
  testthat::expect_equal(
    base::crossprod(x_hat, x), base::crossprod(x_hat), tolerance = 1e-10
  )
})

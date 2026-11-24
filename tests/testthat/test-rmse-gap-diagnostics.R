testthat::local_edition(3)

source(file.path(project_root, "R", "newey_west.R"), local = TRUE)
source(file.path(project_root, "R", "rmse_gap_diagnostics.R"), local = TRUE)

testthat::test_that("horizon returns compound fund by fund on consecutive months", {
  raw_panel <- tibble::tibble(
    period = as.Date(c(
      "2020-01-15", "2020-02-15", "2020-03-15", "2020-04-15",
      "2020-01-15", "2020-03-15", "2020-04-15"
    )),
    code = c(rep("A", 4L), rep("B", 3L)),
    r_ml = c(10, 0, 0, 0, 5, 5, 5),
    r_fund = c(0, 0, 0, 0, 1, 1, 1)
  )

  result <- build_horizon_performance(raw_panel, horizons = c(1L, 3L))
  fund_3m <- dplyr::filter(
    result$fund_level,
    .data$horizon_months == 3L
  )

  testthat::expect_equal(nrow(fund_3m), 2L)
  testthat::expect_true(all(fund_3m$code == "A"))
  testthat::expect_equal(fund_3m$machine_return, c(10, 0), tolerance = 1e-12)
  testthat::expect_equal(fund_3m$fund_return, c(0, 0), tolerance = 1e-12)
  testthat::expect_equal(
    fund_3m$gap,
    fund_3m$machine_return - fund_3m$fund_return,
    tolerance = 1e-12
  )
})

testthat::test_that("RMSE alignment adds the date-matched base-learner mean", {
  dates <- as.Date(c("2020-03-15", "2020-04-15", "2020-05-15"))
  rmse <- tibble::tibble(
    dates = rep(dates, 3L),
    model = rep(c("A", "B", "Meta-model"), each = 3L),
    model_type = rep(c("Base learner", "Base learner", "Meta-model"), each = 3L),
    rmse = c(1:3, 3:5, 2:4)
  )
  performance <- tibble::tibble(
    period = dates,
    horizon_months = 3L,
    n_funds = 10L,
    machine_return = c(1, 2, 3),
    fund_return = c(0, 1, 2),
    gap = 1
  )

  aligned <- align_rmse_performance(rmse, performance)
  base_mean <- dplyr::filter(aligned, .data$model == "Base-learner mean")

  testthat::expect_equal(base_mean$rmse, c(2, 3, 4))
  testthat::expect_equal(base_mean$period, dates)
  testthat::expect_equal(dplyr::n_distinct(aligned$model), 4L)
})

testthat::test_that("lead-lag correlations use positive leads for later outcomes", {
  dates <- seq.Date(as.Date("2020-01-15"), by = "month", length.out = 8L)
  rmse <- tibble::tibble(
    dates = dates,
    model = "Meta-model",
    model_type = "Meta-model",
    rmse = seq_along(dates)
  )
  performance <- tibble::tibble(
    period = dates,
    horizon_months = 3L,
    n_funds = 10L,
    machine_return = c(NA, seq_len(7L)),
    fund_return = 0,
    gap = c(NA, seq_len(7L))
  ) |>
    dplyr::filter(is.finite(.data$gap))

  result <- compute_lead_lag_correlations(
    rmse,
    performance,
    models = "Meta-model",
    leads = 1L,
    outcomes = "gap"
  )

  testthat::expect_equal(result$outcome_lead_months, 1L)
  testthat::expect_equal(result$correlation, 1, tolerance = 1e-12)
})

testthat::test_that("diagnostics reject duplicated fund-period keys", {
  duplicated_panel <- tibble::tibble(
    period = as.Date(c("2020-01-15", "2020-01-15")),
    code = c("A", "A"),
    r_ml = c(1, 1),
    r_fund = c(0, 0)
  )

  testthat::expect_error(
    build_horizon_performance(duplicated_panel),
    "unique fund-period keys",
    fixed = TRUE
  )
})

testthat::test_that("local Newey-West covariance is finite and symmetric", {
  fit <- stats::lm(
    y ~ x,
    data = tibble::tibble(x = 1:20, y = 2 + 0.5 * (1:20) + sin(1:20))
  )
  covariance <- newey_west_covariance(fit, lag = 2L)

  testthat::expect_equal(dim(covariance), c(2L, 2L))
  testthat::expect_true(all(is.finite(covariance)))
  testthat::expect_equal(covariance, t(covariance), tolerance = 1e-12)
  testthat::expect_true(all(diag(covariance) >= 0))
})

testthat::test_that("automatic Newey-West lag follows the monthly sample-size rule", {
  testthat::expect_equal(newey_west_lag(100L), 4L)
  testthat::expect_equal(newey_west_lag(128L), 4L)
  testthat::expect_error(
    newey_west_lag(0L),
    "one positive integer",
    fixed = TRUE
  )
})

testthat::test_that("CAPM alpha t-statistics use Newey-West covariance by ticker", {
  x <- seq_len(128L)
  fit_a <- stats::lm(sin(x / 5) + 0.01 * x ~ cos(x / 7))
  fit_b <- stats::lm(cos(x / 6) + 0.02 * x ~ sin(x / 8))
  fits <- list(B = fit_b, A = fit_a)
  original_alpha <- c(stats::coef(fit_a)[[1L]], stats::coef(fit_b)[[1L]])
  summary_input <- list(
    signal_universe_m_d_ref = tibble::tibble(
      tickers = c("A", "B"),
      alpha = original_alpha,
      alpha_t_stat = c(999, 999)
    ),
    frequentist_fit_results_list = fits
  )

  result <- replace_capm_alpha_t_statistics(summary_input)$signal_universe_m_d_ref
  expected <- c(
    newey_west_coefficient_t_statistic(fit_a),
    newey_west_coefficient_t_statistic(fit_b)
  )

  testthat::expect_equal(result$alpha, original_alpha, tolerance = 0)
  testthat::expect_equal(result$alpha_t_stat, expected, tolerance = 1e-12)
})

testthat::test_that("CAPM Newey-West replacement rejects unmatched fit names", {
  fit <- stats::lm(y ~ x, data = data.frame(x = 1:20, y = sin(1:20)))
  malformed <- list(
    signal_universe_m_d_ref = data.frame(
      tickers = "A",
      alpha = stats::coef(fit)[[1L]],
      alpha_t_stat = 0
    ),
    frequentist_fit_results_list = list(B = fit)
  )

  testthat::expect_error(
    replace_capm_alpha_t_statistics(malformed),
    "uniquely match",
    fixed = TRUE
  )
})

testthat::test_that("RMSE normalization uses contemporaneous target dispersion", {
  dates <- as.Date(c("2020-01-15", "2020-02-15"))
  targets <- tibble::tibble(
    tickers = rep(c("A", "B", "C"), each = 2L),
    dates = rep(dates, 3L),
    target = c(0, 0, 2, 4, 4, 8)
  )
  difficulty <- summarise_target_difficulty(targets)
  rmse <- tibble::tibble(
    dates = dates,
    model = "Meta-model",
    model_type = "Meta-model",
    rmse = c(2, 4)
  )
  normalized <- normalize_rmse_by_target_dispersion(rmse, difficulty)

  testthat::expect_equal(difficulty$n_assets, c(3L, 3L))
  testthat::expect_equal(
    normalized$rmse,
    rmse$rmse / difficulty$target_sd,
    tolerance = 1e-12
  )
})

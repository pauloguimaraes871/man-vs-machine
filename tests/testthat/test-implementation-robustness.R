testthat::local_edition(3)

source(
  file.path(project_root, "R", "implementation_robustness.R"),
  local = TRUE
)

testthat::test_that("meta-model robustness metrics are complete and ordered", {
  metrics <- tibble::tibble(
    metric = c("hr", "mpe", "rmse", "mphe", "mae"),
    cons_oos = c(0.52, 7.5, 22.8, 14.1, 15.0)
  )

  result <- extract_meta_robustness_metrics(metrics, "Grid search / RMSE")

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_identical(
    names(result),
    c("implementation", "rmse", "mphe", "mpe")
  )
  testthat::expect_equal(
    unname(unlist(result[1, IMPLEMENTATION_ROBUSTNESS_METRICS])),
    c(22.8, 14.1, 7.5)
  )
})

testthat::test_that("meta-model robustness metrics reject incomplete panels", {
  missing_metric <- tibble::tibble(
    metric = c("rmse", "mphe", "hr"),
    cons_oos = c(22.8, 14.1, 0.52)
  )
  testthat::expect_error(
    extract_meta_robustness_metrics(missing_metric, "MPE"),
    "Missing consolidated metrics: mpe",
    fixed = TRUE
  )

  duplicated_metric <- dplyr::bind_rows(
    missing_metric,
    tibble::tibble(metric = c("mpe", "mpe"), cons_oos = c(7.5, 7.6))
  )
  testthat::expect_error(
    extract_meta_robustness_metrics(duplicated_metric, "MPE"),
    "duplicated",
    fixed = TRUE
  )
})

testthat::test_that("implementation returns preserve the benchmark and deduct only the fee", {
  dates <- as.Date(c("2020-01-15", "2020-02-15", "2020-03-15"))
  returns <- xts::xts(
    matrix(
      c(1, 2, 3, 4, 5, 6),
      nrow = 3L,
      dimnames = list(NULL, c("meta_rp", "selected_bench_return"))
    ),
    order.by = dates
  )
  monthly_fee <- (1.01)^(1 / 12) - 1

  result <- extract_implementation_returns(
    returns_xts = returns,
    implementation = "Bayesian optimization / RMSE",
    expected_dates = dates,
    expected_columns = colnames(returns),
    monthly_fee = monthly_fee
  )

  testthat::expect_equal(as.numeric(result$benchmark), c(4, 5, 6))
  testthat::expect_equal(
    as.numeric(result$portfolios),
    ((1 + c(1, 2, 3) / 100) / (1 + monthly_fee) - 1) * 100
  )
})

testthat::test_that("implementation returns reject a changed sample or scheme set", {
  dates <- as.Date(c("2020-01-15", "2020-02-15"))
  returns <- xts::xts(
    matrix(
      1:4,
      nrow = 2L,
      dimnames = list(NULL, c("meta_rp", "selected_bench_return"))
    ),
    order.by = dates
  )

  testthat::expect_error(
    extract_implementation_returns(
      returns,
      "MPHE",
      expected_dates = dates + 1L,
      expected_columns = colnames(returns),
      monthly_fee = 0
    ),
    "return dates do not match"
  )
  testthat::expect_error(
    extract_implementation_returns(
      returns,
      "MPHE",
      expected_dates = dates,
      expected_columns = c("meta_rp", "meta_hrp", "selected_bench_return"),
      monthly_fee = 0
    ),
    "return columns do not match"
  )
})

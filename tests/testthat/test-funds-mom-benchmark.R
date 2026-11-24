source(here::here("R", "funds_mom_benchmark.R"), local = TRUE)

testthat::test_that("FUNDS_MOM ranks funds using only preceding returns", {
  fund_panel <- matrix(
    c(
      10, 0, 0, 0,
      10, 0, 0, 0,
      0, 100, 0, 0,
      1, 2, 0, 0
    ),
    nrow = 4L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C", "D"))
  )

  result <- build_funds_mom(
    fund_panel,
    lookback_months = 2L,
    min_history_months = 2L,
    top_share = 0.25
  )

  testthat::expect_true(all(is.na(result[1:2])))
  testthat::expect_equal(result[3:4], c(0, 2))
})

testthat::test_that("FUNDS_MOM preserves missingness when no fund is eligible", {
  fund_panel <- matrix(
    c(1, NA, NA, 2, 3, 4),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B"))
  )

  result <- build_funds_mom(
    fund_panel,
    lookback_months = 2L,
    min_history_months = 2L,
    top_share = 0.5
  )

  testthat::expect_true(all(is.na(result)))
})

testthat::test_that("FUNDS_MOM reallocates a missing selected fund", {
  fund_panel <- matrix(
    c(
      3, 2,
      3, 2,
      NA, 4
    ),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(NULL, c("A", "B"))
  )

  result <- build_funds_mom(
    fund_panel,
    lookback_months = 2L,
    min_history_months = 2L,
    top_share = 1
  )

  testthat::expect_equal(result[[3L]], 4)
})

testthat::test_that("FUNDS_MOM validates its panel and tuning parameters", {
  valid_panel <- matrix(
    1:8,
    nrow = 4L,
    dimnames = list(NULL, c("A", "B"))
  )

  testthat::expect_error(
    build_funds_mom(as.data.frame(valid_panel), lookback_months = 2L),
    "numeric matrix"
  )
  duplicated_names <- valid_panel
  colnames(duplicated_names) <- c("A", "A")
  testthat::expect_error(
    build_funds_mom(duplicated_names, lookback_months = 2L),
    "unique fund identifiers"
  )
  testthat::expect_error(
    build_funds_mom(valid_panel, lookback_months = 1.5),
    "positive integer"
  )
  testthat::expect_error(
    build_funds_mom(valid_panel, lookback_months = 2L, min_history_months = 3L),
    "between 1 and lookback_months"
  )
  testthat::expect_error(
    build_funds_mom(
      valid_panel,
      lookback_months = 2L,
      min_history_months = 2L,
      top_share = 0
    ),
    "top_share"
  )
})

testthat::test_that("wealth compounding retains an explicit common baseline", {
  result <- compound_wealth_from_baseline(c(10, -5, 20))

  testthat::expect_equal(result, c(1, 1.1, 1.045, 1.254))
  testthat::expect_length(result, 4L)
})

testthat::test_that("wealth compounding rejects unusable return paths", {
  testthat::expect_error(
    compound_wealth_from_baseline(numeric()),
    "non-empty numeric vector"
  )
  testthat::expect_error(
    compound_wealth_from_baseline(c(1, NA_real_)),
    "truncate_at_first_missing"
  )
  testthat::expect_error(
    compound_wealth_from_baseline(c(1, -101)),
    "no value below -100"
  )
})

testthat::test_that("wealth compounding ends a fund path at its first missing return", {
  trailing <- compound_wealth_from_baseline(
    c(10, -5, NA_real_, NA_real_),
    truncate_at_first_missing = TRUE
  )

  testthat::expect_equal(trailing, c(1, 1.1, 1.045, NA_real_, NA_real_))
  resumed <- compound_wealth_from_baseline(
    c(10, NA_real_, 5),
    truncate_at_first_missing = TRUE
  )
  testthat::expect_equal(resumed, c(1, 1.1, NA_real_, NA_real_))
})

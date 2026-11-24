source(testthat::test_path("..", "..", "R", "variable_definitions.R"))

testthat::test_that("variable dictionary covers the current 108-signal universe", {
  dictionary <- variable_definition_dictionary()

  testthat::expect_s3_class(dictionary, "data.frame")
  testthat::expect_identical(names(dictionary), c("signal", "definition", "source"))
  testthat::expect_equal(nrow(dictionary), 108L)
  testthat::expect_false(anyDuplicated(dictionary$signal) > 0L)
  testthat::expect_true(all(nzchar(dictionary$signal)))
  testthat::expect_true(all(nzchar(dictionary$definition)))
  testthat::expect_true(all(nzchar(dictionary$source)))
})

testthat::test_that("LaTeX escaping protects table-sensitive characters", {
  escaped <- escape_variable_table_tex("a_b & 10% #1")

  testthat::expect_identical(escaped, "a\\_b \\& 10\\% \\#1")
})

testthat::test_that("renderer creates one longtable with themed panels", {
  theme_order <- c(
    "momentum", "value", "profitability", "growth", "quality", "investment",
    "accruals", "low_fin_risk", "low_mkt_risk", "trading_frictions",
    "lottery_demand", "lt_reversal", "seasonality", "sentiment", "others"
  )
  fixture <- data.frame(
    signal = paste0("signal_", seq_along(theme_order)),
    definition = rep("Definition", length(theme_order)),
    source = rep("Source (2000)", length(theme_order)),
    position = rep(c("long", "short", "force"), length.out = length(theme_order)),
    theme = theme_order,
    stringsAsFactors = FALSE
  )

  tex <- render_variable_definitions_table(fixture)

  testthat::expect_equal(sum(grepl("^\\\\begin\\{longtable\\}", tex)), 1L)
  testthat::expect_equal(sum(grepl("^\\\\caption\\[Variable definitions\\]", tex)), 1L)
  testthat::expect_true(any(grepl("Panel A. Momentum", tex, fixed = TRUE)))
  testthat::expect_true(any(grepl("Panel O. Sector controls", tex, fixed = TRUE)))
})

testthat::test_that("feature mapping fails on a stale dictionary", {
  positions <- c(not_a_current_signal = "long")
  themes <- data.frame(
    tickers = "not_a_current_signal",
    theme = "momentum",
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    build_variable_definition_data(positions, themes),
    "Variable dictionary and selected feature universe differ"
  )
})

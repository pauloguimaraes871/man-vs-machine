# `testthat::test_dir()` loads helper files automatically, but a direct
# `source()` of this test file does not. Load the panel helpers on demand so
# both workflows exercise the same code.
if (!exists("QUANT_VALIDATION_CODES", inherits = TRUE)) {
  test_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
                        error = function(e) NA_character_)
  root_candidates <- unique(c(
    Sys.getenv("GAP_PANEL_PROJECT_ROOT", unset = ""),
    dirname(dirname(dirname(test_file))),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  ))
  valid_roots <- root_candidates[
    !is.na(root_candidates) & nzchar(root_candidates) &
      file.exists(file.path(root_candidates, "build_gap_panel.R"))
  ]
  project_root <- valid_roots[1L]
  if (is.na(project_root) || !nzchar(project_root)) {
    stop("Could not locate build_gap_panel.R for tests.", call. = FALSE)
  }
  source(file.path(project_root, "tests", "testthat", "helper-gap-panel.R"),
         local = .GlobalEnv)
}

testthat::local_edition(3)

testthat::test_that("code reconciliation uses fund identifiers rather than source order", {
  daily <- list(
    ret = list(codes = c("fund_b", "fund_a", "fund_only_ret")),
    aum = list(codes = c("fund_a", "fund_b", "fund_only_aum")),
    net_flow = list(codes = c("fund_b", "fund_a")),
    n_investors = list(codes = c("fund_a", "fund_b"))
  )

  testthat::expect_identical(reconcile_codes(daily), c("fund_a", "fund_b"))
})

testthat::test_that("owner-supplied quant validation labels resolve by unique ANBIMA code", {
  static <- tibble::tibble(
    code = QUANT_VALIDATION_CODES,
    fund_name = paste("known quant fund", seq_along(QUANT_VALIDATION_CODES))
  )
  labelled <- add_quant_validation_label(static)

  testthat::expect_equal(sum(labelled$quant_validation_label),
                         length(QUANT_VALIDATION_CODES))
  testthat::expect_silent(assert_quant_validation_labels(labelled))
  testthat::expect_error(
    assert_quant_validation_labels(labelled[-1L, ]),
    "Curated quant validation code"
  )
})

testthat::test_that("opening AUM and prior-window flows are aligned to the labelled return", {
  period_ends <- as.Date(c("2020-01-15", "2020-02-15"))
  dates <- as.Date(c("2019-12-15", "2020-01-15", "2020-02-15"))
  daily <- list(
    aum = daily_panel(dates, c(100, 999, 888)),
    n_investors = daily_panel(dates, c(10, 20, 30)),
    net_flow = daily_panel(dates, c(999, 10, 20))
  )

  panel <- build_fund_timevarying(daily, period_ends, "A")

  testthat::expect_identical(window_openings(period_ends),
                              as.Date(c("2019-12-15", "2020-01-15")))
  testthat::expect_identical(shift_month(as.Date("2019-11-01"), 2L),
                              as.Date("2020-01-01"))
  testthat::expect_equal(panel$aum, c(100, 999))
  testthat::expect_equal(panel$net_flow, c(10, 20))
  testthat::expect_equal(panel$flow_rate_lag[2L], 10 / 100)
})

testthat::test_that("stale levels and unreported flow windows are not silently used", {
  stale <- daily_panel(as.Date("2020-01-01"), 100)
  testthat::expect_error(
    sample_at(stale, as.Date("2020-01-15"), "A", max_age_days = 7L),
    "Latest daily level is 14 days old"
  )

  missing_flow <- daily_panel(
    as.Date(c("2019-12-16", "2020-01-15")), c(NA_real_, NA_real_)
  )
  testthat::expect_true(is.na(sum_over_window(missing_flow, as.Date("2020-01-15"), "A")[1L, 1L]))
})

testthat::test_that("all-missing manager aggregates are explicit zeros", {
  fund_tv <- tibble::tibble(
    code = c("A", "B"),
    period = as.Date(c("2020-01-15", "2020-01-15")),
    aum = c(NA_real_, NA_real_),
    net_flow = c(NA_real_, NA_real_)
  )
  static <- tibble::tibble(code = c("A", "B"), gestora = c("G", "G"))

  aggregated <- build_gestora_timevarying(fund_tv, static)
  testthat::expect_equal(aggregated$pl_gestora, 0)
  testthat::expect_equal(aggregated$gestora_net_flow, 0)
  testthat::expect_equal(aggregated$n_funds_reporting, 0)
})

testthat::test_that("duplicate clusters are transitive but retain every legal fund", {
  u <- c(-1, 1, 1, -1)
  v <- c(-1, -1, 1, 1)
  theta <- acos(0.9995)
  returns <- cbind(
    fund_a = u,
    fund_b = cos(theta) * u + sin(theta) * v,
    fund_c = cos(2 * theta) * u + sin(2 * theta) * v
  )

  clusters <- find_duplicate_clusters(returns, min_overlap = 4L, rho_cut = 0.999)
  testthat::expect_equal(length(unique(clusters$dup_cluster)), 1L)
  testthat::expect_equal(clusters$cluster_size, rep(3L, 3L))
  testthat::expect_true(all(clusters$is_duplicate))
  testthat::expect_identical(clusters$code, colnames(returns))
})

testthat::test_that("concentration validation rejects malformed ranks and preserves leverage diagnostics", {
  bad_order <- tibble::tibble(
    code = rep("A", 3L), month = rep(as.Date("2020-01-01"), 3L),
    rank = 1:3, w = c(40, 45, 15)
  )
  testthat::expect_error(summarise_concentration(bad_order), "non-increasing")

  levered <- tibble::tibble(
    code = rep("A", 3L), month = rep(as.Date("2020-01-01"), 3L),
    rank = 1:3, w = c(60, 40, 20)
  )
  summary <- NULL
  testthat::expect_warning(
    summary <- summarise_concentration(levered), "above 110% of NAV"
  )
  testthat::expect_true(summary$weight_over_nav)
  testthat::expect_true(is.na(summary$resid_weight))
  testthat::expect_true(is.na(summary$n_pos_min))
})

testthat::test_that("rolling gaps retain only dates with a full trailing 36-month fit", {
  n_periods <- 38L
  period <- seq.Date(as.Date("2018-01-15"), by = "month", length.out = n_periods)
  t <- seq_len(n_periods)
  factors <- cbind(
    sin(t / 2), cos(t / 3), sin(t / 5), cos(t / 7), sin(t / 11), cos(t / 13)
  )
  colnames(factors) <- FF_PREDICTORS
  factor_mat <- tibble::as_tibble(factors)
  factor_mat <- dplyr::mutate(
    factor_mat, ym = format(period, "%Y-%m"), rf = 0, .before = 1L
  )
  beta <- c(0.2, -0.1, 0.3, -0.2, 0.1, 0.05)
  fund_ret_mat <- matrix(0.4 + as.numeric(factors %*% beta), ncol = 1L,
                         dimnames = list(NULL, "fund_a"))
  ml_ret_mat <- matrix(1 + as.numeric(factors %*% beta), ncol = 1L,
                       dimnames = list(NULL, "ml_a"))

  gap <- build_dv(fund_ret_mat, ml_ret_mat, period, factor_mat, "ml_a")

  testthat::expect_identical(gap$period, period[37:38])
  testthat::expect_equal(gap$n_obs_beta, c(36L, 36L))
  testthat::expect_equal(gap$gap, c(0.6, 0.6), tolerance = 1e-10)
  testthat::expect_error(
    build_dv(fund_ret_mat, ml_ret_mat, period, factor_mat[-1L, ], "ml_a"),
    "factor_mat is missing"
  )
})

testthat::test_that("ML index validation requires consecutive calendar 15ths", {
  valid <- as.Date(c("2020-01-15", "2020-02-15", "2020-03-15"))
  testthat::expect_identical(validate_ml_index(valid), valid)
  testthat::expect_error(
    validate_ml_index(as.Date(c("2020-01-15", "2020-03-15"))),
    "consecutive calendar 15ths"
  )
  testthat::expect_error(
    validate_ml_index(as.Date(c("2020-01-15", "2020-02-14"))),
    "consecutive calendar 15ths"
  )
})

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
  # A curated code the snapshot no longer carries is a sample-vintage change,
  # not a fault: warn, do not abort. The refined sample dropped 10 of the 20.
  testthat::expect_warning(
    assert_quant_validation_labels(labelled[-1L, ]),
    "Curated quant validation code"
  )
  # A label that does not resolve one-to-one against the codes actually present
  # IS a fault -- that means a join or de-dup problem.
  broken <- labelled
  broken$quant_validation_label[1L] <- FALSE
  testthat::expect_error(
    assert_quant_validation_labels(broken),
    "not one-to-one"
  )
  # A snapshot carrying none of the curated codes means the wrong file is being
  # read. (Blanking the labels while the codes are still present trips the
  # one-to-one check first, which is the stronger error.)
  none <- add_quant_validation_label(
    tibble::tibble(code = paste0("zz", seq_along(QUANT_VALIDATION_CODES)),
                   fund_name = "unrelated fund")
  )
  testthat::expect_error(
    assert_quant_validation_labels(none),
    "No curated quant validation code"
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

testthat::test_that("top-X duplicate headers retain only an unambiguous populated column", {
  blank <- tibble::tibble(month = as.Date("2020-01-01"), left = NA_real_, right = NA_real_)
  resolved_blank <- resolve_topx_duplicate_columns(blank, c("A", "A"), rank = 2L)
  testthat::expect_identical(resolved_blank$codes, character())

  one_populated <- tibble::tibble(
    month = as.Date("2020-01-01"), left = NA_real_, right = 15
  )
  resolved_one <- resolve_topx_duplicate_columns(one_populated, c("A", "A"), rank = 2L)
  testthat::expect_identical(resolved_one$codes, "A")
  testthat::expect_equal(resolved_one$raw[[2L]], 15)

  two_populated <- tibble::tibble(
    month = as.Date("2020-01-01"), left = 10, right = 15
  )
  testthat::expect_error(
    resolve_topx_duplicate_columns(two_populated, c("A", "A"), rank = 2L),
    "multiple populated columns"
  )
})

testthat::test_that("concentration validation handles leverage, shorts, and corrupted snapshots", {
  bad_order <- tibble::tibble(
    code = rep("A", 3L), month = rep(as.Date("2020-01-01"), 3L),
    rank = 1:3, w = c(40, 45, 15)
  )
  testthat::expect_error(summarise_concentration(bad_order), "malformed")

  levered <- tibble::tibble(
    code = rep("A", 3L), month = rep(as.Date("2020-01-01"), 3L),
    rank = 1:3, w = c(60, 40, 20)
  )
  summary <- NULL
  testthat::expect_warning(
    summary <- summarise_concentration(levered), "positive top-15 exposure above 110%"
  )
  testthat::expect_true(summary$weight_over_nav)
  testthat::expect_true(is.na(summary$resid_weight))
  testthat::expect_true(is.na(summary$n_pos_min))

  short_book <- tibble::tibble(
    code = rep("A", 3L), month = rep(as.Date("2020-01-01"), 3L),
    rank = 1:3, w = c(60, 20, -5)
  )
  short_summary <- summarise_concentration(short_book)
  testthat::expect_true(short_summary$has_short)
  testthat::expect_equal(short_summary$gross_exposure_top15, 85)
  testthat::expect_true(is.na(short_summary$resid_weight))
  testthat::expect_true(is.na(short_summary$eff_bets_max))

  corrupted <- tibble::tibble(
    code = rep("A", 3L), month = rep(as.Date("2020-01-01"), 3L),
    rank = 1:3, w = c(1001, 5, 1)
  )
  testthat::expect_error(summarise_concentration(corrupted), "malformed")
  testthat::expect_warning(
    excluded <- summarise_concentration(corrupted, invalid_snapshot = "exclude"),
    "Excluding their concentration measures"
  )
  testthat::expect_equal(nrow(excluded), 0L)
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

  ml_cols   <- c(ibov = "ml_a")
  one_fund  <- tibble::tibble(code = "fund_a", mandate = "ibov")

  gap <- build_dv(fund_ret_mat, ml_ret_mat, period, factor_mat,
                  ml_cols = ml_cols, fund_mandate = one_fund)

  testthat::expect_identical(gap$period, period[37:38])
  testthat::expect_equal(gap$n_obs_beta, c(36L, 36L))
  testthat::expect_equal(gap$gap, c(0.6, 0.6), tolerance = 1e-10)
  testthat::expect_identical(unique(gap$ml_benchmark), "ml_a")
  # `mandate` belongs to `static`; the DV must not emit a second copy or the
  # assembly join produces mandate.x / mandate.y.
  testthat::expect_false("mandate" %in% names(gap))
  testthat::expect_error(
    build_dv(fund_ret_mat, ml_ret_mat, period, factor_mat[-1L, ],
             ml_cols = ml_cols, fund_mandate = one_fund),
    "factor_mat is missing"
  )
  # A fund with no mandate must abort rather than fall back to ibov.
  testthat::expect_error(
    build_dv(fund_ret_mat, ml_ret_mat, period, factor_mat,
             ml_cols = ml_cols,
             fund_mandate = tibble::tibble(code = "other", mandate = "ibov")),
    "no mandate assignment"
  )
  # A mandate the machine never ran must abort too.
  testthat::expect_error(
    build_dv(fund_ret_mat, ml_ret_mat, period, factor_mat,
             ml_cols = ml_cols,
             fund_mandate = tibble::tibble(code = "fund_a", mandate = "smll")),
    "no benchmark column"
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

# -----------------------------------------------------------------------------
# Conditional (per-mandate) benchmark
# -----------------------------------------------------------------------------

testthat::test_that("mandate assignment follows the declared ANBIMA class", {
  cls <- c("Ações Dividendos", "Ações Small Caps", "Ações Livre",
           "Ações Índice Ativo", "Ações Indexados", "Ações Valor/Crescimento")
  testthat::expect_identical(
    assign_mandate(cls),
    c("idiv", "smll", "ibov", "ibov", "ibov", "ibov")
  )

  # isee was dropped as a benchmark arm: ESG-mandated funds -- whether ANBIMA
  # classifies them that way or their name declares it -- go to ibov like any
  # other fund. A stray "isee" here means the arm crept back in.
  testthat::expect_identical(
    assign_mandate(c("Ações Sustentabilidade/Governança", "Ações Livre")),
    c("ibov", "ibov")
  )
  testthat::expect_false(any(assign_mandate(cls) == "isee"))

  # An unknown class falls back rather than producing NA.
  testthat::expect_identical(assign_mandate("Ações Exterior"), "ibov")
  testthat::expect_identical(assign_mandate(NA_character_), "ibov")

  # Every mandate produced must be one the machine is actually read for.
  testthat::expect_true(all(assign_mandate(cls) %in% ML_MANDATES))
})

testthat::test_that("index-tracking classes stay on the ibov mandate", {
  # Índice Ativo / Indexados are deliberately NOT a separate benchmark arm:
  # they fall back to ibov like any other unlisted class, and their behavioural
  # difference is handled by RHS dummies in analysis_gap.R. A separate mandate
  # here would need a machine cohort that was never run.
  testthat::expect_identical(
    assign_mandate(c("Ações Índice Ativo", "Ações Indexados")),
    c("ibov", "ibov")
  )
  testthat::expect_false(exists("assign_arm"))
})

testthat::test_that("the cadastral header row is detected, not assumed", {
  make_raw <- function(code_col) {
    raw <- as.data.frame(matrix("", nrow = length(code_col), ncol = 30L),
                         stringsAsFactors = FALSE)
    raw[[CAD_COLS[["code"]]]] <- code_col
    raw
  }
  # Refined export: two blank rows, then the header.
  testthat::expect_identical(
    cadastral_header_row(make_raw(c(NA, NA, "Código\r\nAnbima", "213705"))), 3L
  )
  # Original export: header on row 1.
  testthat::expect_identical(
    cadastral_header_row(make_raw(c("Código\nAnbima", "603511"))), 1L
  )
  testthat::expect_error(
    cadastral_header_row(make_raw(c("603511", "213705"))),
    "could not locate"
  )
  testthat::expect_error(
    cadastral_header_row(make_raw(c("Código\nAnbima", "Código\nAnbima"))),
    "look like the header row"
  )
})

testthat::test_that("resolve_ml_benchmark returns one column per mandate", {
  mandates <- c("ibov", "smll")
  cols <- c(outer(ML_SCHEMES, mandates,
                  function(s, m) sprintf("c__meta_%s_%s_s__x", s, m)))
  mat <- matrix(seq_len(2L * length(cols)), nrow = 2L,
                dimnames = list(NULL, cols))

  avg <- resolve_ml_benchmark(mat, "average", mandates)
  testthat::expect_identical(avg$cols,
                             c(ibov = "average_ibov", smll = "average_smll"))
  # Averaging happens WITHIN a mandate, never across.
  ibov_cols <- sprintf("c__meta_%s_ibov_s__x", ML_SCHEMES)
  testthat::expect_equal(unname(avg$mat[, "average_ibov"]),
                         unname(rowMeans(mat[, ibov_cols, drop = FALSE])))

  one <- resolve_ml_benchmark(mat, "mvo_box", mandates)
  testthat::expect_identical(
    one$cols,
    c(ibov = "c__meta_mvo_box_ibov_s__x", smll = "c__meta_mvo_box_smll_s__x")
  )

  testthat::expect_error(
    resolve_ml_benchmark(mat, "average", c("ibov", "idiv")),
    "mandate 'idiv' matched 0 columns"
  )
})

testthat::test_that("the conditional gap differences each fund against its own mandate", {
  n_periods <- 40L
  period <- seq(as.Date("2015-01-15"), by = "month", length.out = n_periods)
  t <- seq_len(n_periods)
  factors <- cbind(sin(t / 2), cos(t / 3), sin(t / 5),
                   cos(t / 7), sin(t / 11), cos(t / 13))
  colnames(factors) <- FF_PREDICTORS
  factor_mat <- dplyr::mutate(tibble::as_tibble(factors),
                              ym = format(period, "%Y-%m"), rf = 0, .before = 1L)
  beta <- c(0.2, -0.1, 0.3, -0.2, 0.1, 0.05)
  common <- as.numeric(factors %*% beta)

  fund_ret_mat <- cbind(big = 0.4 + common, small = 0.4 + common)
  # Two machines with deliberately different means: 1.0 and 3.0 above the funds'
  # 0.4, so a mis-routed fund is unmissable.
  ml_ret_mat <- cbind(m_ibov = 1.0 + common, m_smll = 3.0 + common)

  gap <- build_dv(
    fund_ret_mat, ml_ret_mat, period, factor_mat,
    ml_cols      = c(ibov = "m_ibov", smll = "m_smll"),
    fund_mandate = tibble::tibble(code = c("big", "small"),
                                  mandate = c("ibov", "smll"))
  )

  # Compare against the constant, not unique(): the residuals differ in the last
  # bit or two across periods, so unique() would report several "distinct" gaps.
  testthat::expect_equal(gap$gap[gap$code == "big"],
                         rep(0.6, sum(gap$code == "big")), tolerance = 1e-10)
  testthat::expect_equal(gap$gap[gap$code == "small"],
                         rep(2.6, sum(gap$code == "small")), tolerance = 1e-10)
  testthat::expect_identical(unique(gap$ml_benchmark[gap$code == "small"]), "m_smll")
  testthat::expect_identical(unique(gap$ml_benchmark[gap$code == "big"]), "m_ibov")
})

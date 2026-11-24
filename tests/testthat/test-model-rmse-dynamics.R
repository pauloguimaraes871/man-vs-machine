testthat::local_edition(3)

source(file.path(project_root, "R", "model_rmse_dynamics.R"), local = TRUE)

if (!methods::isClass("test_rmse_metrics")) {
  methods::setClass("test_rmse_metrics", slots = c(data = "ANY"))
}
if (!methods::isClass("test_rmse_result")) {
  methods::setClass(
    "test_rmse_result",
    slots = c(oos_testing_eval_metrics_m_xts = "ANY")
  )
}
if (!methods::isClass("test_rmse_grid")) {
  methods::setClass(
    "test_rmse_grid",
    slots = c(
      meta_sb_backtest_results = "ANY",
      base_sb_backtest_results_list = "list"
    )
  )
}

make_test_rmse_result <- function(dates, values, metric_name = "rmse") {
  metric_data <- xts::xts(values, order.by = dates)
  colnames(metric_data) <- metric_name
  metrics <- methods::new("test_rmse_metrics", data = metric_data)
  methods::new(
    "test_rmse_result",
    oos_testing_eval_metrics_m_xts = metrics
  )
}

make_test_rmse_grid <- function() {
  all_dates <- as.Date("2020-01-15") + c(0L, 31L, 60L, 91L, 121L)
  base_names <- c(
    "c__ols_f__signals",
    "c__glmnet_grid_f__signals",
    "c__rf_grid_f__signals",
    "c__xgb_grid_f__signals",
    "c__nn1_grid_f__signals",
    "c__nn2_grid_f__signals",
    "c__nn3_grid_f__signals"
  )
  base_results <- stats::setNames(
    lapply(seq_along(base_names), function(i) {
      make_test_rmse_result(all_dates, seq_len(5L) + i)
    }),
    base_names
  )
  methods::new(
    "test_rmse_grid",
    meta_sb_backtest_results = make_test_rmse_result(
      all_dates[2:4],
      c(4.5, 5.5, 6.5)
    ),
    base_sb_backtest_results_list = base_results
  )
}

testthat::test_that("RMSE data use the complete common testing window", {
  rmse_data <- build_model_rmse_data(make_test_rmse_grid())

  testthat::expect_s3_class(rmse_data, "tbl_df")
  testthat::expect_equal(nrow(rmse_data), 24L)
  testthat::expect_equal(
    levels(rmse_data$model),
    c("OLS", "ENET", "RF", "XGB", "NN1", "NN2", "NN3", "Meta-model")
  )
  testthat::expect_equal(
    range(rmse_data$dates),
    as.Date("2020-01-15") + c(31L, 91L)
  )
  testthat::expect_equal(
    dplyr::filter(rmse_data, .data$model == "Meta-model")$rmse,
    c(4.5, 5.5, 6.5)
  )
  testthat::expect_s3_class(
    plot_model_rmse_dynamics(rmse_data),
    "ggplot"
  )
})

testthat::test_that("RMSE extraction rejects missing and invalid values", {
  grid <- make_test_rmse_grid()
  grid@meta_sb_backtest_results <- make_test_rmse_result(
    as.Date("2020-01-15") + 0:2,
    c(1, 2, 3),
    metric_name = "mae"
  )
  testthat::expect_error(
    build_model_rmse_data(grid),
    "required 'rmse' column",
    fixed = TRUE
  )

  grid <- make_test_rmse_grid()
  grid@meta_sb_backtest_results <- make_test_rmse_result(
    as.Date("2020-01-15") + 0:2,
    c(1, NA_real_, 3)
  )
  testthat::expect_error(
    build_model_rmse_data(grid),
    "finite, non-negative value",
    fixed = TRUE
  )
})

IMPLEMENTATION_ROBUSTNESS_METRICS <- c("rmse", "mphe", "mpe")

extract_meta_robustness_metrics <- function(
    consolidated_metrics,
    implementation,
    required_metrics = IMPLEMENTATION_ROBUSTNESS_METRICS) {
  if (!is.data.frame(consolidated_metrics)) {
    rlang::abort("consolidated_metrics must be a data frame.")
  }
  if (!is.character(implementation) || length(implementation) != 1L ||
      is.na(implementation) || !nzchar(implementation)) {
    rlang::abort("implementation must be one non-empty string.")
  }
  if (!is.character(required_metrics) || length(required_metrics) == 0L ||
      anyNA(required_metrics) || any(!nzchar(required_metrics)) ||
      anyDuplicated(required_metrics)) {
    rlang::abort("required_metrics must contain unique, non-empty metric names.")
  }

  required_columns <- c("metric", "cons_oos")
  missing_columns <- setdiff(required_columns, names(consolidated_metrics))
  if (length(missing_columns) > 0L) {
    rlang::abort(paste0(
      "Missing consolidated metric columns: ",
      paste(missing_columns, collapse = ", "), "."
    ))
  }

  selected <- consolidated_metrics |>
    dplyr::filter(.data$metric %in% .env$required_metrics) |>
    dplyr::transmute(
      metric = as.character(.data$metric),
      value = as.numeric(.data$cons_oos)
    )

  missing_metrics <- setdiff(required_metrics, selected$metric)
  if (length(missing_metrics) > 0L) {
    rlang::abort(paste0(
      "Missing consolidated metrics: ",
      paste(missing_metrics, collapse = ", "), "."
    ))
  }
  if (anyDuplicated(selected$metric)) {
    rlang::abort("Consolidated robustness metrics are duplicated.")
  }
  if (any(!is.finite(selected$value))) {
    rlang::abort("Consolidated robustness metrics must be finite.")
  }

  selected |>
    dplyr::mutate(
      implementation = .env$implementation,
      metric = factor(.data$metric, levels = .env$required_metrics)
    ) |>
    dplyr::arrange(.data$metric) |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value") |>
    dplyr::select("implementation", dplyr::all_of(required_metrics))
}

extract_implementation_returns <- function(
    returns_xts,
    implementation,
    expected_dates,
    expected_columns,
    benchmark_column = "selected_bench_return",
    monthly_fee) {
  if (!xts::is.xts(returns_xts)) {
    rlang::abort("returns_xts must be an xts object.")
  }
  if (!is.character(implementation) || length(implementation) != 1L ||
      is.na(implementation) || !nzchar(implementation)) {
    rlang::abort("implementation must be one non-empty string.")
  }
  if (!inherits(expected_dates, "Date") || anyNA(expected_dates) ||
      anyDuplicated(expected_dates) || is.unsorted(expected_dates, strictly = TRUE)) {
    rlang::abort("expected_dates must contain unique, sorted Date values.")
  }
  if (!is.character(expected_columns) || length(expected_columns) < 2L ||
      anyNA(expected_columns) || anyDuplicated(expected_columns)) {
    rlang::abort("expected_columns must contain unique return-column names.")
  }
  if (!benchmark_column %in% expected_columns) {
    rlang::abort("benchmark_column must be included in expected_columns.")
  }
  if (!is.numeric(monthly_fee) || length(monthly_fee) != 1L ||
      !is.finite(monthly_fee) || monthly_fee < 0) {
    rlang::abort("monthly_fee must be one finite, non-negative number.")
  }

  actual_dates <- as.Date(zoo::index(returns_xts))
  same_dates <- length(actual_dates) == length(expected_dates) &&
    all(actual_dates == expected_dates)
  if (!same_dates) {
    rlang::abort(paste0(
      "The ", implementation,
      " return dates do not match the primary implementation."
    ))
  }
  if (!setequal(colnames(returns_xts), expected_columns)) {
    rlang::abort(paste0(
      "The ", implementation,
      " return columns do not match the primary implementation."
    ))
  }

  ordered_returns <- returns_xts[, expected_columns]
  if (any(!is.finite(as.numeric(ordered_returns)))) {
    rlang::abort(paste0(
      "The ", implementation, " return panel contains non-finite values."
    ))
  }

  portfolio_columns <- setdiff(expected_columns, benchmark_column)
  portfolio_returns <- ordered_returns[, portfolio_columns]
  portfolio_returns <- (
    (1 + portfolio_returns / 100) / (1 + monthly_fee) - 1
  ) * 100

  list(
    implementation = implementation,
    dates = actual_dates,
    portfolios = portfolio_returns,
    benchmark = ordered_returns[, benchmark_column]
  )
}

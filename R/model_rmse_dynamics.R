required_s4_slot <- function(object, slot_name, object_label) {
  if (!slot_name %in% methods::slotNames(object)) {
    rlang::abort(paste0(
      object_label, " must be an S4 object with slot '", slot_name, "'."
    ))
  }
  methods::slot(object, slot_name)
}

rmse_model_label <- function(backtest_name) {
  model <- backtest_name |>
    sub(pattern = "^c__", replacement = "") |>
    sub(pattern = "_f__.*$", replacement = "") |>
    sub(pattern = "_(grid|bayes)$", replacement = "") |>
    toupper()

  display_names <- c(
    OLS = "OLS",
    GLMNET = "ENET",
    RF = "RF",
    XGB = "XGB",
    NN1 = "NN1",
    NN2 = "NN2",
    NN3 = "NN3"
  )
  if (!model %in% names(display_names)) {
    rlang::abort(paste0(
      "Cannot infer a displayed model name from backtest '", backtest_name, "'."
    ))
  }
  unname(display_names[[model]])
}

rmse_index_to_date <- function(index, model_label) {
  dates <- if (inherits(index, "Date")) {
    index
  } else if (inherits(index, "POSIXt")) {
    as.Date(index, tz = "UTC")
  } else if (is.numeric(index)) {
    as.Date(as.POSIXct(index, origin = "1970-01-01", tz = "UTC"))
  } else {
    rlang::abort(paste0(
      "The RMSE index for ", model_label,
      " must contain Date, POSIXt, or numeric timestamps."
    ))
  }

  if (anyNA(dates) || anyDuplicated(dates) ||
      is.unsorted(dates, strictly = TRUE)) {
    rlang::abort(paste0(
      "The RMSE dates for ", model_label,
      " must be finite, unique, and strictly increasing."
    ))
  }
  dates
}

extract_result_rmse <- function(result, model_label, model_type) {
  metrics <- required_s4_slot(
    result,
    "oos_testing_eval_metrics_m_xts",
    paste0(model_label, " backtest result")
  )
  metric_data <- required_s4_slot(
    metrics,
    "data",
    paste0(model_label, " OOS metric object")
  )
  if (!xts::is.xts(metric_data)) {
    rlang::abort(paste0(
      "The OOS metric data for ", model_label, " must be an xts object."
    ))
  }
  if (!"rmse" %in% colnames(metric_data)) {
    rlang::abort(paste0(
      "The OOS metric data for ", model_label,
      " do not contain the required 'rmse' column."
    ))
  }

  rmse <- as.numeric(metric_data[, "rmse"])
  dates <- rmse_index_to_date(zoo::index(metric_data), model_label)
  if (length(rmse) != length(dates) || any(!is.finite(rmse)) || any(rmse < 0)) {
    rlang::abort(paste0(
      "The RMSE series for ", model_label,
      " must have one finite, non-negative value per date."
    ))
  }

  tibble::tibble(
    dates = dates,
    model = model_label,
    model_type = model_type,
    rmse = rmse
  )
}

build_model_rmse_data <- function(mb_results) {
  meta_result <- required_s4_slot(
    mb_results,
    "meta_sb_backtest_results",
    "Meta-backtest result"
  )
  base_results <- required_s4_slot(
    mb_results,
    "base_sb_backtest_results_list",
    "Meta-backtest result"
  )
  if (!is.list(base_results) || is.null(names(base_results)) ||
      anyNA(names(base_results)) || any(names(base_results) == "") ||
      anyDuplicated(names(base_results))) {
    rlang::abort("Base backtest results must be a uniquely named list.")
  }

  base_labels <- vapply(names(base_results), rmse_model_label, character(1))
  model_order <- c("OLS", "ENET", "RF", "XGB", "NN1", "NN2", "NN3")
  if (!setequal(base_labels, model_order) || anyDuplicated(base_labels)) {
    rlang::abort(
      "The meta-backtest must contain exactly OLS, ENET, RF, XGB, and NN1--NN3."
    )
  }

  base_data <- purrr::imap_dfr(base_results, function(result, result_name) {
    extract_result_rmse(
      result = result,
      model_label = rmse_model_label(result_name),
      model_type = "Base learner"
    )
  })
  meta_data <- extract_result_rmse(
    result = meta_result,
    model_label = "Meta-model",
    model_type = "Meta-model"
  )
  all_data <- dplyr::bind_rows(base_data, meta_data)

  date_sets <- split(all_data$dates, all_data$model)
  common_dates <- Reduce(intersect, date_sets)
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  if (length(common_dates) < 2L) {
    rlang::abort("The model RMSE series do not share at least two testing dates.")
  }

  displayed_order <- c(model_order, "Meta-model")
  common_data <- all_data |>
    dplyr::filter(.data$dates %in% common_dates) |>
    dplyr::mutate(
      model = factor(.data$model, levels = displayed_order),
      model_type = factor(
        .data$model_type,
        levels = c("Base learner", "Meta-model")
      )
    ) |>
    dplyr::arrange(.data$model, .data$dates)

  if (anyDuplicated(common_data[c("model", "dates")]) ||
      nrow(common_data) != length(displayed_order) * length(common_dates)) {
    rlang::abort("The common-date RMSE panel is incomplete or duplicated.")
  }
  attr(common_data, "common_start") <- min(common_dates)
  attr(common_data, "common_end") <- max(common_dates)
  common_data
}

plot_model_rmse_dynamics <- function(model_rmse_data, base_size = 16) {
  required_columns <- c("dates", "model", "model_type", "rmse")
  if (!inherits(model_rmse_data, "data.frame") ||
      !all(required_columns %in% names(model_rmse_data))) {
    rlang::abort(paste0(
      "model_rmse_data must be a data frame containing ",
      paste(required_columns, collapse = ", "), "."
    ))
  }
  if (nrow(model_rmse_data) == 0L || any(!is.finite(model_rmse_data$rmse))) {
    rlang::abort("model_rmse_data must contain finite RMSE observations.")
  }

  model_colors <- c(
    OLS = "#7A7A7A",
    ENET = "#0072B2",
    RF = "#E69F00",
    XGB = "#009E73",
    NN1 = "#CC79A7",
    NN2 = "#56B4E9",
    NN3 = "#D55E00",
    `Meta-model` = "#000000"
  )
  observed_models <- unique(as.character(model_rmse_data$model))
  if (!setequal(observed_models, names(model_colors))) {
    rlang::abort("model_rmse_data must contain the seven base learners and meta-model.")
  }

  base_data <- dplyr::filter(
    model_rmse_data,
    .data$model_type == "Base learner"
  )
  meta_data <- dplyr::filter(
    model_rmse_data,
    .data$model_type == "Meta-model"
  )
  common_start <- min(model_rmse_data$dates)
  common_end <- max(model_rmse_data$dates)
  journal_palette <- factoRverse:::.journal_palette()

  ggplot2::ggplot(
    model_rmse_data,
    ggplot2::aes(x = .data$dates, y = .data$rmse, group = .data$model)
  ) +
    ggplot2::geom_line(
      data = base_data,
      ggplot2::aes(color = .data$model),
      linewidth = 0.55,
      alpha = 0.72
    ) +
    ggplot2::geom_line(
      data = meta_data,
      ggplot2::aes(color = .data$model),
      linewidth = 1.05,
      linetype = "longdash"
    ) +
    ggplot2::scale_color_manual(
      values = model_colors,
      breaks = names(model_colors),
      drop = FALSE
    ) +
    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.04, 0.08))
    ) +
    ggplot2::labs(
      x = NULL,
      y = "RMSE (percentage points)",
      color = NULL,
      title = "Out-of-sample forecast error over time",
      subtitle = paste0(
        "Monthly cross-sectional RMSE on common testing dates, ",
        format(common_start, "%b %Y"), "--", format(common_end, "%b %Y")
      )
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = journal_palette$background,
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = journal_palette$background,
        color = NA
      ),
      panel.grid.major = ggplot2::element_line(
        color = journal_palette$grid,
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = journal_palette$text),
      axis.title = ggplot2::element_text(color = journal_palette$text),
      plot.title = ggplot2::element_text(
        color = journal_palette$text,
        face = "bold",
        size = base_size + 2
      ),
      plot.subtitle = ggplot2::element_text(
        color = journal_palette$text,
        size = base_size
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(color = journal_palette$text)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(
          alpha = 1,
          linewidth = c(rep(0.8, 7L), 1.3),
          linetype = c(rep("solid", 7L), "longdash")
        )
      )
    )
}

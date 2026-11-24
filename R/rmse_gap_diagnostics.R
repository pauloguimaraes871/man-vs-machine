rmse_gap_month_id <- function(dates) {
  dates <- as.Date(dates)
  if (anyNA(dates)) {
    rlang::abort("Monthly diagnostic dates must be non-missing Dates.")
  }
  as.integer(format(dates, "%Y")) * 12L + as.integer(format(dates, "%m"))
}

validate_raw_gap_panel <- function(raw_panel) {
  required_columns <- c("period", "code", "r_ml", "r_fund")
  if (!inherits(raw_panel, "data.frame") ||
      !all(required_columns %in% names(raw_panel))) {
    rlang::abort(paste0(
      "raw_panel must be a data frame containing ",
      paste(required_columns, collapse = ", "), "."
    ))
  }

  panel <- tibble::as_tibble(raw_panel) |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    dplyr::mutate(
      period = as.Date(.data$period),
      code = as.character(.data$code),
      r_ml = as.numeric(.data$r_ml),
      r_fund = as.numeric(.data$r_fund)
    )

  if (nrow(panel) == 0L || anyNA(panel$period) ||
      anyNA(panel$code) || any(panel$code == "") ||
      any(!is.finite(panel$r_ml)) || any(!is.finite(panel$r_fund))) {
    rlang::abort("raw_panel must contain non-empty funds and finite dated returns.")
  }
  if (anyDuplicated(panel[c("code", "period")])) {
    rlang::abort("raw_panel must contain unique fund-period keys.")
  }
  if (any(panel$r_ml < -100) || any(panel$r_fund < -100)) {
    rlang::abort("Simple percentage returns cannot be below -100 percent.")
  }

  panel |>
    dplyr::arrange(.data$code, .data$period)
}

compound_one_fund_horizon <- function(fund_rows, horizon_months) {
  fund_rows <- dplyr::arrange(fund_rows, .data$period)
  n_rows <- nrow(fund_rows)
  if (n_rows < horizon_months) {
    return(tibble::tibble(
      period = as.Date(character()),
      machine_return = numeric(),
      fund_return = numeric(),
      gap = numeric()
    ))
  }

  month_id <- rmse_gap_month_id(fund_rows$period)
  end_rows <- seq.int(horizon_months, n_rows)
  valid_end <- vapply(end_rows, function(end_row) {
    window_rows <- seq.int(end_row - horizon_months + 1L, end_row)
    length(window_rows) == horizon_months &&
      all(diff(month_id[window_rows]) == 1L)
  }, logical(1))
  end_rows <- end_rows[valid_end]

  if (length(end_rows) == 0L) {
    return(tibble::tibble(
      period = as.Date(character()),
      machine_return = numeric(),
      fund_return = numeric(),
      gap = numeric()
    ))
  }

  compound_at <- function(values, end_row) {
    window_rows <- seq.int(end_row - horizon_months + 1L, end_row)
    (prod(1 + values[window_rows] / 100) - 1) * 100
  }
  machine_return <- vapply(
    end_rows,
    function(i) compound_at(fund_rows$r_ml, i),
    numeric(1)
  )
  fund_return <- vapply(
    end_rows,
    function(i) compound_at(fund_rows$r_fund, i),
    numeric(1)
  )

  tibble::tibble(
    period = fund_rows$period[end_rows],
    machine_return = machine_return,
    fund_return = fund_return,
    gap = machine_return - fund_return
  )
}

build_horizon_performance <- function(raw_panel, horizons = c(1L, 3L)) {
  panel <- validate_raw_gap_panel(raw_panel)
  horizons <- as.integer(horizons)
  if (length(horizons) == 0L || anyNA(horizons) ||
      any(horizons < 1L) || anyDuplicated(horizons)) {
    rlang::abort("horizons must contain unique positive integers.")
  }

  fund_level <- purrr::map_dfr(horizons, function(horizon_months) {
    panel |>
      dplyr::group_by(.data$code) |>
      dplyr::group_modify(
        ~ compound_one_fund_horizon(.x, horizon_months)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(horizon_months = horizon_months, .before = "period")
  }) |>
    dplyr::arrange(.data$horizon_months, .data$period, .data$code)

  if (nrow(fund_level) == 0L ||
      any(abs(fund_level$gap -
              (fund_level$machine_return - fund_level$fund_return)) > 1e-10)) {
    rlang::abort("No valid horizon returns were produced from raw_panel.")
  }

  time_series <- fund_level |>
    dplyr::group_by(.data$horizon_months, .data$period) |>
    dplyr::summarise(
      n_funds = dplyr::n_distinct(.data$code),
      machine_return = mean(.data$machine_return),
      fund_return = mean(.data$fund_return),
      gap = mean(.data$gap),
      gap_dispersion = stats::sd(.data$gap),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$horizon_months, .data$period)

  list(fund_level = fund_level, time_series = time_series)
}

augment_rmse_series <- function(model_rmse_data) {
  required_columns <- c("dates", "model", "model_type", "rmse")
  if (!inherits(model_rmse_data, "data.frame") ||
      !all(required_columns %in% names(model_rmse_data))) {
    rlang::abort(paste0(
      "model_rmse_data must contain ",
      paste(required_columns, collapse = ", "), "."
    ))
  }

  rmse <- tibble::as_tibble(model_rmse_data) |>
    dplyr::transmute(
      dates = as.Date(.data$dates),
      model = as.character(.data$model),
      model_type = as.character(.data$model_type),
      rmse = as.numeric(.data$rmse)
    )
  if (nrow(rmse) == 0L || anyNA(rmse$dates) || anyNA(rmse$model) ||
      any(rmse$model == "") || any(!is.finite(rmse$rmse)) ||
      any(rmse$rmse < 0) || anyDuplicated(rmse[c("model", "dates")])) {
    rlang::abort("RMSE data must have unique model-date keys and finite values.")
  }

  base_mean <- rmse |>
    dplyr::filter(.data$model_type == "Base learner") |>
    dplyr::group_by(.data$dates) |>
    dplyr::summarise(
      model = "Base-learner mean",
      model_type = "Base-learner aggregate",
      rmse = mean(.data$rmse),
      .groups = "drop"
    )
  dplyr::bind_rows(rmse, base_mean) |>
    dplyr::arrange(.data$model, .data$dates)
}

summarise_target_difficulty <- function(oos_output_data) {
  required_columns <- c("tickers", "dates", "target")
  if (!inherits(oos_output_data, "data.frame") ||
      !all(required_columns %in% names(oos_output_data))) {
    rlang::abort(paste0(
      "oos_output_data must contain ",
      paste(required_columns, collapse = ", "), "."
    ))
  }

  targets <- tibble::as_tibble(oos_output_data) |>
    dplyr::transmute(
      tickers = as.character(.data$tickers),
      dates = as.Date(.data$dates),
      target = as.numeric(.data$target)
    )
  if (nrow(targets) == 0L || anyNA(targets$dates) ||
      anyNA(targets$tickers) || any(targets$tickers == "") ||
      anyDuplicated(targets[c("tickers", "dates")])) {
    rlang::abort("Target data must have unique, non-missing ticker-date keys.")
  }

  difficulty <- targets |>
    dplyr::filter(is.finite(.data$target)) |>
    dplyr::group_by(.data$dates) |>
    dplyr::summarise(
      n_assets = dplyr::n_distinct(.data$tickers),
      target_sd = stats::sd(.data$target),
      target_abs_mean = mean(abs(.data$target)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$dates)
  if (nrow(difficulty) == 0L || any(difficulty$n_assets < 2L) ||
      any(!is.finite(difficulty$target_sd)) || any(difficulty$target_sd <= 0)) {
    rlang::abort("Target dispersion must be finite and positive on every date.")
  }
  difficulty
}

normalize_rmse_by_target_dispersion <- function(
    model_rmse_data,
    target_difficulty) {
  rmse <- tibble::as_tibble(model_rmse_data) |>
    dplyr::mutate(
      dates = as.Date(.data$dates),
      model = as.character(.data$model),
      model_type = as.character(.data$model_type),
      rmse = as.numeric(.data$rmse)
    )
  difficulty <- tibble::as_tibble(target_difficulty) |>
    dplyr::mutate(dates = as.Date(.data$dates))
  if (!all(c("dates", "target_sd") %in% names(difficulty)) ||
      anyDuplicated(difficulty$dates)) {
    rlang::abort("target_difficulty must have unique dates and target_sd values.")
  }

  normalized <- rmse |>
    dplyr::inner_join(
      dplyr::select(difficulty, "dates", "target_sd"),
      by = "dates",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(rmse = .data$rmse / .data$target_sd) |>
    dplyr::select("dates", "model", "model_type", "rmse")
  if (nrow(normalized) != nrow(rmse) || any(!is.finite(normalized$rmse))) {
    rlang::abort("Every RMSE observation must match a finite target dispersion.")
  }
  normalized
}

align_rmse_performance <- function(model_rmse_data, performance_time_series) {
  required_performance <- c(
    "period", "horizon_months", "n_funds",
    "machine_return", "fund_return", "gap"
  )
  if (!inherits(performance_time_series, "data.frame") ||
      !all(required_performance %in% names(performance_time_series))) {
    rlang::abort(paste0(
      "performance_time_series must contain ",
      paste(required_performance, collapse = ", "), "."
    ))
  }

  performance <- tibble::as_tibble(performance_time_series) |>
    dplyr::mutate(period = as.Date(.data$period))
  if (anyDuplicated(performance[c("horizon_months", "period")]) ||
      anyNA(performance$period)) {
    rlang::abort("Performance data must have unique horizon-period keys.")
  }

  aligned <- augment_rmse_series(model_rmse_data) |>
    dplyr::inner_join(
      performance,
      by = c("dates" = "period"),
      relationship = "many-to-many"
    ) |>
    dplyr::rename(period = "dates") |>
    dplyr::group_by(.data$model, .data$horizon_months) |>
    dplyr::mutate(
      rmse_z = if (stats::sd(.data$rmse) > 0) {
        as.numeric(scale(.data$rmse))
      } else {
        NA_real_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$model, .data$horizon_months, .data$period)

  if (nrow(aligned) == 0L || anyNA(aligned$rmse_z) ||
      anyDuplicated(aligned[c("model", "horizon_months", "period")])) {
    rlang::abort("RMSE and performance data do not form a valid aligned panel.")
  }
  aligned
}

compute_rmse_correlations <- function(
    aligned_data,
    outcomes = c("machine_return", "fund_return", "gap"),
    methods = c("pearson", "spearman")) {
  if (!all(outcomes %in% names(aligned_data))) {
    rlang::abort("Every requested correlation outcome must be present in aligned_data.")
  }

  groups <- aligned_data |>
    dplyr::distinct(.data$model, .data$horizon_months)
  purrr::pmap_dfr(groups, function(model, horizon_months) {
    rows <- dplyr::filter(
      aligned_data,
      .data$model == .env$model,
      .data$horizon_months == .env$horizon_months
    )
    purrr::map_dfr(outcomes, function(outcome) {
      purrr::map_dfr(methods, function(method) {
        values <- rows[[outcome]]
        valid <- is.finite(rows$rmse) & is.finite(values)
        correlation <- if (sum(valid) >= 3L &&
                           stats::sd(rows$rmse[valid]) > 0 &&
                           stats::sd(values[valid]) > 0) {
          stats::cor(rows$rmse[valid], values[valid], method = method)
        } else {
          NA_real_
        }
        tibble::tibble(
          model = model,
          horizon_months = horizon_months,
          outcome = outcome,
          method = method,
          n = sum(valid),
          correlation = correlation
        )
      })
    })
  })
}

fit_hac_rmse_models <- function(
    aligned_data,
    models = c("Meta-model", "Base-learner mean"),
    horizons = 3L,
    outcomes = c("machine_return", "fund_return", "gap"),
    minimum_lag = 2L) {
  combinations <- tidyr::expand_grid(
    model = models,
    horizon_months = as.integer(horizons),
    outcome = outcomes
  )
  purrr::pmap_dfr(combinations, function(model, horizon_months, outcome) {
    rows <- aligned_data |>
      dplyr::filter(
        .data$model == .env$model,
        .data$horizon_months == .env$horizon_months
      ) |>
      dplyr::arrange(.data$period) |>
      dplyr::transmute(y = .data[[outcome]], rmse_z = .data$rmse_z) |>
      dplyr::filter(is.finite(.data$y), is.finite(.data$rmse_z))
    if (nrow(rows) < 12L || stats::sd(rows$y) == 0) {
      rlang::abort(paste0("Insufficient variation for HAC model: ", model, "/", outcome, "."))
    }

    fit <- stats::lm(y ~ rmse_z, data = rows)
    hac_lag <- max(as.integer(minimum_lag), horizon_months - 1L)
    covariance <- newey_west_covariance(
      fit,
      lag = hac_lag,
      adjust = TRUE
    )
    estimate <- stats::coef(fit)[["rmse_z"]]
    std_error <- sqrt(covariance["rmse_z", "rmse_z"])
    statistic <- estimate / std_error
    degrees_freedom <- stats::df.residual(fit)
    critical <- stats::qt(0.975, df = degrees_freedom)

    tibble::tibble(
      model = model,
      horizon_months = horizon_months,
      outcome = outcome,
      n = nrow(rows),
      hac_lag = hac_lag,
      estimate_per_sd_rmse = estimate,
      std_error = std_error,
      conf_low = estimate - critical * std_error,
      conf_high = estimate + critical * std_error,
      statistic = statistic,
      p_value = 2 * stats::pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE),
      r_squared = summary(fit)$r.squared
    )
  })
}

compute_high_error_comparison <- function(
    aligned_data,
    models = c("Meta-model", "Base-learner mean"),
    horizon_months = 3L,
    outcomes = c("machine_return", "fund_return", "gap"),
    quantile_probability = 0.75,
    minimum_lag = 2L) {
  if (!is.numeric(quantile_probability) || length(quantile_probability) != 1L ||
      !is.finite(quantile_probability) || quantile_probability <= 0 ||
      quantile_probability >= 1) {
    rlang::abort("quantile_probability must be one finite value strictly between zero and one.")
  }

  purrr::map_dfr(models, function(model) {
    rows <- aligned_data |>
      dplyr::filter(
        .data$model == .env$model,
        .data$horizon_months == .env$horizon_months
      ) |>
      dplyr::arrange(.data$period)
    cutoff <- stats::quantile(
      rows$rmse,
      probs = quantile_probability,
      names = FALSE,
      type = 7
    )
    rows <- dplyr::mutate(rows, high_error = .data$rmse >= cutoff)

    purrr::map_dfr(outcomes, function(outcome) {
      fit_rows <- tibble::tibble(
        y = rows[[outcome]],
        high_error = as.numeric(rows$high_error)
      ) |>
        dplyr::filter(is.finite(.data$y))
      fit <- stats::lm(y ~ high_error, data = fit_rows)
      hac_lag <- max(as.integer(minimum_lag), as.integer(horizon_months) - 1L)
      covariance <- newey_west_covariance(
        fit,
        lag = hac_lag,
        adjust = TRUE
      )
      estimate <- stats::coef(fit)[["high_error"]]
      std_error <- sqrt(covariance["high_error", "high_error"])
      statistic <- estimate / std_error
      degrees_freedom <- stats::df.residual(fit)

      tibble::tibble(
        model = model,
        horizon_months = horizon_months,
        outcome = outcome,
        n = nrow(fit_rows),
        high_error_cutoff = cutoff,
        high_error_periods = sum(rows$high_error),
        other_periods = sum(!rows$high_error),
        mean_high_error = mean(rows[[outcome]][rows$high_error]),
        mean_other = mean(rows[[outcome]][!rows$high_error]),
        difference = estimate,
        std_error = std_error,
        statistic = statistic,
        p_value = 2 * stats::pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE)
      )
    })
  })
}

compute_lead_lag_correlations <- function(
    model_rmse_data,
    performance_time_series,
    models = c("Meta-model", "Base-learner mean"),
    horizon_months = 3L,
    leads = -6L:6L,
    outcomes = c("machine_return", "fund_return", "gap")) {
  rmse <- augment_rmse_series(model_rmse_data) |>
    dplyr::filter(.data$model %in% .env$models) |>
    dplyr::mutate(rmse_month_id = rmse_gap_month_id(.data$dates))
  performance <- tibble::as_tibble(performance_time_series) |>
    dplyr::filter(.data$horizon_months == .env$horizon_months) |>
    dplyr::mutate(outcome_month_id = rmse_gap_month_id(.data$period))

  combinations <- tidyr::expand_grid(
    model = models,
    outcome = outcomes,
    outcome_lead_months = as.integer(leads)
  )
  purrr::pmap_dfr(combinations, function(model, outcome, outcome_lead_months) {
    model_rmse <- rmse |>
      dplyr::filter(.data$model == .env$model) |>
      dplyr::transmute(
        join_month_id = .data$rmse_month_id + outcome_lead_months,
        rmse = .data$rmse
      )
    rows <- performance |>
      dplyr::transmute(
        join_month_id = .data$outcome_month_id,
        outcome_value = .data[[outcome]]
      ) |>
      dplyr::inner_join(model_rmse, by = "join_month_id") |>
      dplyr::filter(is.finite(.data$outcome_value), is.finite(.data$rmse))
    valid_correlation <- nrow(rows) >= 3L &&
                       stats::sd(rows$outcome_value) > 0 &&
                       stats::sd(rows$rmse) > 0
    valid_model <- valid_correlation && nrow(rows) >= 12L
    correlation <- if (valid_correlation) {
      stats::cor(rows$rmse, rows$outcome_value, method = "pearson")
    } else {
      NA_real_
    }
    if (valid_model) {
      fit_rows <- dplyr::mutate(
        rows,
        rmse_z = as.numeric(scale(.data$rmse))
      )
      fit <- stats::lm(outcome_value ~ rmse_z, data = fit_rows)
      covariance <- newey_west_covariance(
        fit,
        lag = max(2L, as.integer(horizon_months) - 1L),
        adjust = TRUE
      )
      estimate <- stats::coef(fit)[["rmse_z"]]
      std_error <- sqrt(covariance["rmse_z", "rmse_z"])
      statistic <- estimate / std_error
      p_value <- 2 * stats::pt(
        abs(statistic),
        df = stats::df.residual(fit),
        lower.tail = FALSE
      )
    } else {
      estimate <- std_error <- statistic <- p_value <- NA_real_
    }

    tibble::tibble(
      model = model,
      horizon_months = horizon_months,
      outcome = outcome,
      outcome_lead_months = outcome_lead_months,
      n = nrow(rows),
      correlation = correlation,
      estimate_per_sd_rmse = estimate,
      std_error = std_error,
      statistic = statistic,
      p_value = p_value
    )
  }) |>
    dplyr::group_by(.data$model, .data$outcome) |>
    dplyr::mutate(
      p_adjusted_bh = stats::p.adjust(.data$p_value, method = "BH")
    ) |>
    dplyr::ungroup()
}

plot_rmse_gap_diagnostics <- function(aligned_data, lead_lag, high_error) {
  meta <- aligned_data |>
    dplyr::filter(
      .data$model == "Meta-model",
      .data$horizon_months == 3L
    ) |>
    dplyr::arrange(.data$period) |>
    dplyr::mutate(gap_z = as.numeric(scale(.data$gap)))
  if (nrow(meta) < 12L) {
    rlang::abort("At least 12 aligned meta-model observations are required for plotting.")
  }

  time_data <- meta |>
    dplyr::select("period", `RMSE` = "rmse_z", `Raw gap` = "gap_z") |>
    tidyr::pivot_longer(
      cols = c("RMSE", "Raw gap"),
      names_to = "series",
      values_to = "value"
    )
  p_time <- ggplot2::ggplot(
    time_data,
    ggplot2::aes(.data$period, .data$value, colour = .data$series)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c(RMSE = "#000000", `Raw gap` = "#1B6CA8")) +
    ggplot2::labs(
      title = "Realization-date alignment",
      subtitle = "Three-month raw gap and meta-model RMSE, standardized",
      x = NULL, y = "Standard deviations", colour = NULL
    )

  p_scatter <- ggplot2::ggplot(
    meta,
    ggplot2::aes(.data$rmse, .data$gap)
  ) +
    ggplot2::geom_point(colour = "#1B6CA8", alpha = 0.65) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                         colour = "#C0392B", fill = "#C0392B", alpha = 0.16) +
    ggplot2::labs(
      title = "Forecast error and the raw gap",
      subtitle = "Each point is a three-month window ending on the RMSE date",
      x = "Meta-model RMSE (pp)", y = "Raw gap (pp over three months)"
    )

  lead_gap <- lead_lag |>
    dplyr::filter(.data$outcome == "gap")
  p_lead <- ggplot2::ggplot(
    lead_gap,
    ggplot2::aes(.data$outcome_lead_months, .data$correlation,
                 colour = .data$model)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::scale_colour_manual(
      values = c(`Meta-model` = "#000000", `Base-learner mean` = "#1B6CA8")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(-6, 6, by = 2)) +
    ggplot2::labs(
      title = "Lead-lag diagnostic",
      subtitle = "Positive leads are gaps ending after the RMSE realization date",
      x = "Outcome lead (months)", y = "Pearson correlation", colour = NULL
    )

  high_plot <- high_error |>
    dplyr::filter(.data$model == "Meta-model") |>
    dplyr::select("outcome", "mean_high_error", "mean_other") |>
    tidyr::pivot_longer(
      cols = c("mean_high_error", "mean_other"),
      names_to = "error_group",
      values_to = "mean_return"
    ) |>
    dplyr::mutate(
      outcome = factor(
        .data$outcome,
        levels = c("machine_return", "fund_return", "gap"),
        labels = c("Machine return", "Fund return", "Raw gap")
      ),
      error_group = factor(
        .data$error_group,
        levels = c("mean_other", "mean_high_error"),
        labels = c("Other months", "Top-quartile RMSE")
      )
    )
  p_high <- ggplot2::ggplot(
    high_plot,
    ggplot2::aes(.data$outcome, .data$mean_return,
                 colour = .data$error_group, group = .data$error_group)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.35), size = 2.4) +
    ggplot2::scale_colour_manual(values = c("#1B6CA8", "#C0392B")) +
    ggplot2::labs(
      title = "High-error versus other periods",
      subtitle = "Mean horizon-matched three-month outcomes",
      x = NULL, y = "Percentage points", colour = NULL
    )

  plots <- list(p_time, p_scatter, p_lead, p_high)
  plots <- lapply(plots, function(plot) {
    plot +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  cowplot::plot_grid(
    plotlist = plots,
    labels = c("(a)", "(b)", "(c)", "(d)"),
    ncol = 2,
    label_fontface = "bold",
    label_size = 11
  )
}

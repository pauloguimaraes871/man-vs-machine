newey_west_lag <- function(n_obs) {
  if (!is.numeric(n_obs) || length(n_obs) != 1L || !is.finite(n_obs) ||
      n_obs <= 0 || n_obs != as.integer(n_obs)) {
    rlang::abort("n_obs must be one positive integer.")
  }

  as.integer(floor(4 * (as.integer(n_obs) / 100)^(2 / 9)))
}

newey_west_covariance <- function(
    fit,
    lag = newey_west_lag(stats::nobs(fit)),
    adjust = TRUE) {
  if (!inherits(fit, "lm")) {
    rlang::abort("fit must be an lm object.")
  }
  lag <- as.integer(lag)
  if (length(lag) != 1L || is.na(lag) || lag < 0L) {
    rlang::abort("lag must be one non-negative integer.")
  }

  design <- stats::model.matrix(fit)
  residuals <- stats::residuals(fit)
  n <- nrow(design)
  k <- ncol(design)
  if (n <= k || lag >= n || any(!is.finite(design)) ||
      any(!is.finite(residuals)) || base::qr(design)$rank < k) {
    rlang::abort("The fitted model is not valid for Newey-West inference.")
  }

  score <- design * as.numeric(residuals)
  meat <- base::crossprod(score)
  if (lag > 0L) {
    for (lag_i in seq_len(lag)) {
      weight <- 1 - lag_i / (lag + 1)
      covariance_lag <- base::crossprod(
        score[seq.int(lag_i + 1L, n), , drop = FALSE],
        score[seq_len(n - lag_i), , drop = FALSE]
      )
      meat <- meat + weight * (covariance_lag + t(covariance_lag))
    }
  }

  bread <- base::solve(base::crossprod(design))
  covariance <- bread %*% meat %*% bread
  if (isTRUE(adjust)) {
    covariance <- covariance * n / (n - k)
  }
  dimnames(covariance) <- list(colnames(design), colnames(design))
  (covariance + t(covariance)) / 2
}

newey_west_coefficient_t_statistic <- function(
    fit,
    term = "(Intercept)",
    lag = newey_west_lag(stats::nobs(fit)),
    adjust = TRUE) {
  if (!inherits(fit, "lm")) {
    rlang::abort("fit must be an lm object.")
  }
  if (!is.character(term) || length(term) != 1L || is.na(term) || !nzchar(term)) {
    rlang::abort("term must be one non-empty coefficient name.")
  }

  estimates <- stats::coef(fit)
  if (!term %in% names(estimates) || !is.finite(estimates[[term]])) {
    rlang::abort(paste0("The fitted model has no finite coefficient named ", term, "."))
  }

  covariance <- newey_west_covariance(fit, lag = lag, adjust = adjust)
  variance <- covariance[term, term]
  if (!is.finite(variance) || variance <= 0) {
    rlang::abort(paste0("The Newey-West variance for ", term, " is not positive and finite."))
  }

  unname(estimates[[term]] / sqrt(variance))
}

replace_capm_alpha_t_statistics <- function(performance_summary) {
  if (!is.list(performance_summary) ||
      !all(c("signal_universe_m_d_ref", "frequentist_fit_results_list") %in%
           names(performance_summary))) {
    rlang::abort("performance_summary must be a no-pooled performance summary.")
  }

  performance <- performance_summary$signal_universe_m_d_ref
  fits <- performance_summary$frequentist_fit_results_list
  required_columns <- c("tickers", "alpha", "alpha_t_stat")
  if (!is.data.frame(performance) || !all(required_columns %in% names(performance)) ||
      !is.list(fits) || is.null(names(fits))) {
    rlang::abort("The performance summary must contain named CAPM fits and alpha columns.")
  }
  if (anyNA(performance$tickers) || anyDuplicated(performance$tickers) ||
      any(!nzchar(performance$tickers)) || anyNA(names(fits)) ||
      anyDuplicated(names(fits)) || any(!nzchar(names(fits))) ||
      !setequal(performance$tickers, names(fits))) {
    rlang::abort("CAPM fit names must uniquely match the performance tickers.")
  }

  newey_west_statistics <- tibble::tibble(
    tickers = names(fits),
    alpha_t_stat_newey_west = unname(
      vapply(
        fits,
        newey_west_coefficient_t_statistic,
        term = "(Intercept)",
        FUN.VALUE = numeric(1)
      )
    )
  )
  performance <- dplyr::left_join(
    performance,
    newey_west_statistics,
    by = "tickers",
    relationship = "one-to-one"
  )
  if (any(!is.finite(performance$alpha_t_stat_newey_west))) {
    rlang::abort("Newey-West CAPM alpha t-statistics could not be matched completely.")
  }
  performance$alpha_t_stat <- performance$alpha_t_stat_newey_west
  performance$alpha_t_stat_newey_west <- NULL
  performance_summary$signal_universe_m_d_ref <- performance
  performance_summary
}

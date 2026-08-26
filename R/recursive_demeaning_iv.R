# Recursive-demeaning IV estimator following Pastor, Stambaugh, and Taylor
# (2015), equations (5)--(10). The implementation is deliberately limited to
# one endogenous regressor, which is the paper's core fund-size specification.

rd_iv_abort <- function(message) {
  rlang::abort(message, class = "rd_iv_error")
}

rd_iv_cluster_meat <- function(scores, cluster) {
  cluster <- as.character(cluster)
  groups <- split(seq_len(nrow(scores)), cluster)
  n_groups <- length(groups)
  if (n_groups < 2L) {
    rd_iv_abort("Clustered inference requires at least two clusters in each dimension.")
  }
  sums <- vapply(
    groups,
    function(rows) colSums(scores[rows, , drop = FALSE]),
    numeric(ncol(scores))
  )
  if (is.null(dim(sums))) sums <- matrix(sums, nrow = ncol(scores))
  tcrossprod(sums) * n_groups / (n_groups - 1)
}

rd_iv_vcov <- function(design, residuals, clusters) {
  n <- nrow(design)
  k <- ncol(design)
  if (n <= k) rd_iv_abort("The transformed sample has no residual degrees of freedom.")

  scores <- design * as.numeric(residuals)
  bread_cross <- crossprod(design)
  if (qr(bread_cross)$rank < k) {
    rd_iv_abort("The covariance design is rank deficient.")
  }
  bread <- solve(bread_cross)

  meats <- lapply(clusters, function(g) rd_iv_cluster_meat(scores, g))
  meat <- meats[[1L]]
  if (length(meats) == 2L) {
    intersection <- interaction(clusters[[1L]], clusters[[2L]], drop = TRUE)
    meat <- meat + meats[[2L]] - rd_iv_cluster_meat(scores, intersection)
  }

  vcov <- bread %*% meat %*% t(bread) * (n - 1) / (n - k)
  vcov <- (vcov + t(vcov)) / 2
  eigen_vcov <- eigen(vcov, symmetric = TRUE)
  if (any(eigen_vcov$values < -sqrt(.Machine$double.eps))) {
    rlang::warn(
      "The clustered covariance matrix was not positive semidefinite; negative eigenvalues were truncated to zero.",
      class = "rd_iv_non_psd_vcov"
    )
    vcov <- eigen_vcov$vectors %*%
      diag(pmax(eigen_vcov$values, 0), nrow = length(eigen_vcov$values)) %*%
      t(eigen_vcov$vectors)
  }
  dimnames(vcov) <- list(colnames(design), colnames(design))
  vcov
}

rd_iv_forward_demean <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  reverse_sum <- apply(x[n:1L, , drop = FALSE], 2L, cumsum)
  if (is.null(dim(reverse_sum))) reverse_sum <- matrix(reverse_sum, ncol = ncol(x))
  future_sum <- reverse_sum[n:1L, , drop = FALSE]
  x - future_sum / rev(seq_len(n))
}

rd_iv_backward_demean <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  cumulative_sum <- apply(x, 2L, cumsum)
  if (is.null(dim(cumulative_sum))) cumulative_sum <- matrix(cumulative_sum, ncol = ncol(x))
  prior_sum <- cumulative_sum - x
  out <- matrix(NA_real_, nrow = n, ncol = ncol(x), dimnames = dimnames(x))
  if (n >= 2L) {
    out[2L:n, ] <- x[2L:n, , drop = FALSE] -
      prior_sum[2L:n, , drop = FALSE] / seq_len(n - 1L)
  }
  out
}

rd_iv_fund_first_stage <- function(transformed, id_name, x_fd_name, z_bd_name) {
  ids <- unique(transformed[[id_name]])
  rows <- lapply(ids, function(id_value) {
    d <- transformed[transformed[[id_name]] == id_value, , drop = FALSE]
    x <- d[[x_fd_name]]
    z <- d[[z_bd_name]]
    denominator <- sum(z^2)
    slope_origin <- if (denominator > sqrt(.Machine$double.eps)) sum(z * x) / denominator else NA_real_

    slope_intercept <- NA_real_
    intercept <- NA_real_
    if (length(x) >= 2L && stats::sd(z) > sqrt(.Machine$double.eps)) {
      fit <- stats::lm(x ~ z)
      intercept <- unname(stats::coef(fit)[[1L]])
      slope_intercept <- unname(stats::coef(fit)[[2L]])
    }

    data.frame(
      id = as.character(id_value),
      observations = length(x),
      slope_no_intercept = slope_origin,
      intercept_with_intercept = intercept,
      slope_with_intercept = slope_intercept,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Estimate a recursively demeaned instrumental-variables panel model
#'
#' The outcome and endogenous regressor are recursively forward demeaned within
#' panel unit. The recursively backward-demeaned endogenous regressor instruments
#' its forward-demeaned counterpart. Optional exogenous controls are forward
#' demeaned and instrument themselves. Both stages are fitted through the origin.
#'
#' @param data A data frame containing the panel.
#' @param id,time,outcome,endogenous Scalar column names. `time` must order
#'   observations economically within `id`; `endogenous` is restricted to one
#'   variable.
#' @param exogenous Optional character vector of exogenous control names. Factors
#'   are expanded using treatment contrasts before recursive demeaning.
#' @param cluster One or two column names used for clustered inference.
#' @param keep_transformed Whether to retain the transformed estimation data.
#'
#' @return A list containing coefficient estimates, clustered covariance,
#'   first-stage diagnostics, fund-level first-stage diagnostics, sample
#'   diagnostics, and optionally the transformed data.
recursive_demeaning_iv <- function(data, id, time, outcome, endogenous,
                                   exogenous = character(),
                                   cluster = c(id, time),
                                   keep_transformed = TRUE) {
  if (!is.data.frame(data)) rd_iv_abort("`data` must be a data frame.")
  scalar_names <- list(id = id, time = time, outcome = outcome, endogenous = endogenous)
  invalid_scalar <- vapply(
    scalar_names,
    function(x) !is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x),
    logical(1)
  )
  if (any(invalid_scalar)) {
    rd_iv_abort(paste0("These arguments must be single non-empty column names: ",
                       paste(names(invalid_scalar)[invalid_scalar], collapse = ", "), "."))
  }
  if (!is.character(exogenous) || anyNA(exogenous) || any(!nzchar(exogenous))) {
    rd_iv_abort("`exogenous` must be a character vector of column names.")
  }
  if (!is.character(cluster) || !length(cluster) || length(cluster) > 2L ||
      anyNA(cluster) || any(!nzchar(cluster))) {
    rd_iv_abort("`cluster` must contain one or two column names.")
  }

  required <- unique(c(id, time, outcome, endogenous, exogenous, cluster))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    rd_iv_abort(paste0("Missing required column(s): ",
                       paste(missing_columns, collapse = ", "), "."))
  }
  if (!is.numeric(data[[outcome]]) || !is.numeric(data[[endogenous]])) {
    rd_iv_abort("`outcome` and `endogenous` must identify numeric columns.")
  }
  if (anyNA(data[required])) {
    rd_iv_abort("Required model, key, and cluster columns must not contain missing values.")
  }
  numeric_required <- unique(c(outcome, endogenous,
                               exogenous[vapply(data[exogenous], is.numeric, logical(1))]))
  if (length(numeric_required) &&
      any(!vapply(data[numeric_required], function(x) all(is.finite(x)), logical(1)))) {
    rd_iv_abort("Numeric model columns must contain only finite values.")
  }

  key <- data[c(id, time)]
  if (anyDuplicated(key)) rd_iv_abort("The panel contains duplicated id-time keys.")
  order_index <- order(data[[id]], data[[time]])
  input_was_sorted <- identical(order_index, seq_len(nrow(data)))
  if (!input_was_sorted) {
    rlang::inform("Input rows were reordered explicitly by id and time before transformation.")
  }
  ordered <- data[order_index, , drop = FALSE]

  exogenous_matrix <- matrix(numeric(0), nrow = nrow(ordered), ncol = 0L)
  if (length(exogenous)) {
    exogenous_formula <- stats::reformulate(exogenous)
    exogenous_frame <- stats::model.frame(
      exogenous_formula, data = ordered, na.action = stats::na.fail
    )
    exogenous_matrix <- stats::model.matrix(exogenous_formula, exogenous_frame)
    exogenous_matrix <- exogenous_matrix[
      , colnames(exogenous_matrix) != "(Intercept)", drop = FALSE
    ]
  }

  id_rows <- split(seq_len(nrow(ordered)), ordered[[id]])
  short_ids <- names(id_rows)[lengths(id_rows) < 2L]
  if (length(short_ids)) {
    rlang::warn(
      sprintf("Dropping %d panel unit(s) with fewer than two observations.", length(short_ids)),
      class = "rd_iv_short_panel"
    )
  }
  usable_rows <- id_rows[lengths(id_rows) >= 2L]
  if (!length(usable_rows)) rd_iv_abort("No panel unit has at least two observations.")

  transformed_parts <- lapply(usable_rows, function(rows) {
    y <- matrix(ordered[[outcome]][rows], ncol = 1L,
                dimnames = list(NULL, paste0(outcome, "__fd")))
    x_endogenous <- matrix(ordered[[endogenous]][rows], ncol = 1L,
                           dimnames = list(NULL, endogenous))
    x_all <- cbind(x_endogenous, exogenous_matrix[rows, , drop = FALSE])
    x_forward <- rd_iv_forward_demean(x_all)
    x_backward <- rd_iv_backward_demean(x_endogenous)
    y_forward <- rd_iv_forward_demean(y)
    keep <- 2L:length(rows)

    out <- ordered[rows[keep], unique(c(id, time, cluster)), drop = FALSE]
    out[[paste0(outcome, "__fd")]] <- y_forward[keep, 1L]
    out[[paste0(endogenous, "__fd")]] <- x_forward[keep, 1L]
    out[[paste0(endogenous, "__bd")]] <- x_backward[keep, 1L]
    if (ncol(exogenous_matrix)) {
      exogenous_forward <- x_forward[keep, -1L, drop = FALSE]
      colnames(exogenous_forward) <- paste0(colnames(exogenous_matrix), "__fd")
      out <- cbind(out, as.data.frame(exogenous_forward, check.names = FALSE))
    }
    out
  })
  transformed <- do.call(rbind, transformed_parts)
  rownames(transformed) <- NULL

  y_name <- paste0(outcome, "__fd")
  x_name <- paste0(endogenous, "__fd")
  z_name <- paste0(endogenous, "__bd")
  exogenous_names <- grep("__fd$", names(transformed), value = TRUE)
  exogenous_names <- setdiff(exogenous_names, c(y_name, x_name))

  if (length(exogenous_names)) {
    nonzero <- vapply(
      transformed[exogenous_names],
      function(x) stats::sd(x) > sqrt(.Machine$double.eps),
      logical(1)
    )
    if (any(!nonzero)) {
      rlang::warn(
        paste0("Dropping exogenous column(s) eliminated by within-unit transformation: ",
               paste(sub("__fd$", "", exogenous_names[!nonzero]), collapse = ", "), "."),
        class = "rd_iv_dropped_exogenous"
      )
      exogenous_names <- exogenous_names[nonzero]
    }
  }
  dropped_collinear_exogenous <- character()

  y <- transformed[[y_name]]
  x <- cbind(transformed[[x_name]])
  colnames(x) <- endogenous
  z <- cbind(transformed[[z_name]])
  colnames(z) <- paste0(endogenous, "__backward_instrument")
  if (length(exogenous_names)) {
    x_exogenous <- as.matrix(transformed[exogenous_names])
    clean_names <- sub("__fd$", "", exogenous_names)
    colnames(x_exogenous) <- clean_names
    x <- cbind(x, x_exogenous)
    z <- cbind(z, x_exogenous)
  }

  # Forward-demeaned factor indicators can contain an exact dependency even
  # after treatment coding. For example, once t = 1 is omitted, the remaining
  # time indicators sum to a within-unit constant whose forward demean is zero.
  # Preserve the endogenous regressor and its instrument, then add exogenous
  # columns sequentially only when they increase the rank of both X and Z.
  if (ncol(x) > 1L) {
    keep_controls <- logical(ncol(x) - 1L)
    x_kept <- x[, 1L, drop = FALSE]
    z_kept <- z[, 1L, drop = FALSE]
    for (j in 2L:ncol(x)) {
      x_candidate <- cbind(x_kept, x[, j, drop = FALSE])
      z_candidate <- cbind(z_kept, z[, j, drop = FALSE])
      adds_x_rank <- qr(x_candidate)$rank > qr(x_kept)$rank
      adds_z_rank <- qr(z_candidate)$rank > qr(z_kept)$rank
      if (adds_x_rank && adds_z_rank) {
        keep_controls[j - 1L] <- TRUE
        x_kept <- x_candidate
        z_kept <- z_candidate
      }
    }
    if (any(!keep_controls)) {
      dropped_collinear_exogenous <- colnames(x)[-1L][!keep_controls]
      rlang::warn(
        paste0("Dropping transformed exogenous column(s) that are exactly collinear: ",
               paste(dropped_collinear_exogenous, collapse = ", "), "."),
        class = "rd_iv_collinear_exogenous"
      )
    }
    x <- x_kept
    z <- z_kept
  }

  n <- nrow(x)
  if (n <= ncol(x)) rd_iv_abort("The transformed model has no residual degrees of freedom.")
  if (qr(x)$rank < ncol(x)) rd_iv_abort("The transformed regressor matrix is rank deficient.")
  if (qr(z)$rank < ncol(z)) rd_iv_abort("The transformed instrument matrix is rank deficient.")

  z_cross <- crossprod(z)
  first_stage_coefficients <- solve(z_cross, crossprod(z, x[, 1L]))
  fitted_endogenous <- as.numeric(z %*% first_stage_coefficients)
  x_hat <- x
  x_hat[, 1L] <- fitted_endogenous

  second_stage_cross <- crossprod(x_hat, x)
  if (qr(second_stage_cross)$rank < ncol(x)) {
    rd_iv_abort("The 2SLS normal equations are rank deficient.")
  }
  coefficients <- solve(second_stage_cross, crossprod(x_hat, y))
  names(coefficients) <- colnames(x)
  residuals <- as.numeric(y - x %*% coefficients)

  cluster_values <- lapply(cluster, function(name) transformed[[name]])
  vcov <- rd_iv_vcov(x_hat, residuals, cluster_values)
  coefficient_se <- sqrt(diag(vcov))
  coefficient_table <- data.frame(
    term = names(coefficients),
    estimate = as.numeric(coefficients),
    std_error = as.numeric(coefficient_se[names(coefficients)]),
    stringsAsFactors = FALSE
  )
  coefficient_table$t <- coefficient_table$estimate / coefficient_table$std_error
  coefficient_table$p <- 2 * stats::pnorm(-abs(coefficient_table$t))

  first_stage_residuals <- x[, 1L] - fitted_endogenous
  first_stage_vcov <- rd_iv_vcov(z, first_stage_residuals, cluster_values)
  instrument_name <- colnames(z)[1L]
  instrument_estimate <- unname(first_stage_coefficients[1L])
  instrument_se <- sqrt(first_stage_vcov[instrument_name, instrument_name])
  instrument_t <- instrument_estimate / instrument_se

  unrestricted_ssr <- sum(first_stage_residuals^2)
  if (ncol(z) == 1L) {
    restricted_ssr <- sum(x[, 1L]^2)
  } else {
    restricted_z <- z[, -1L, drop = FALSE]
    restricted_fit <- restricted_z %*%
      solve(crossprod(restricted_z), crossprod(restricted_z, x[, 1L]))
    restricted_ssr <- sum((x[, 1L] - restricted_fit)^2)
  }
  residual_df <- n - ncol(z)
  classical_f <- ((restricted_ssr - unrestricted_ssr) / 1) /
    (unrestricted_ssr / residual_df)
  partial_r2 <- (restricted_ssr - unrestricted_ssr) / restricted_ssr
  first_stage <- data.frame(
    endogenous = endogenous,
    instrument = instrument_name,
    estimate = instrument_estimate,
    std_error = instrument_se,
    t = instrument_t,
    f_clustered_wald = instrument_t^2,
    p_clustered_wald = stats::pchisq(instrument_t^2, df = 1L, lower.tail = FALSE),
    f_classical = classical_f,
    partial_r2 = partial_r2,
    instrument_positive = instrument_estimate > 0,
    weak_by_f10 = instrument_t^2 < 10,
    observations = n,
    stringsAsFactors = FALSE
  )
  if (is.finite(first_stage$f_clustered_wald) && first_stage$f_clustered_wald < 10) {
    rlang::warn(
      sprintf("The cluster-robust first-stage Wald F-statistic is %.2f; treat the RD-IV estimate as weak-instrument sensitive.",
              first_stage$f_clustered_wald),
      class = "rd_iv_weak_first_stage"
    )
  }

  fund_first_stage <- rd_iv_fund_first_stage(transformed, id, x_name, z_name)
  valid_fund_slopes <- is.finite(fund_first_stage$slope_no_intercept)
  first_stage$fund_slopes_available <- sum(valid_fund_slopes)
  first_stage$fund_slopes_negative <- sum(
    fund_first_stage$slope_no_intercept[valid_fund_slopes] < 0
  )
  first_stage$share_fund_slopes_negative <- if (any(valid_fund_slopes)) {
    first_stage$fund_slopes_negative / first_stage$fund_slopes_available
  } else {
    NA_real_
  }
  id_lengths <- lengths(id_rows)
  diagnostics <- data.frame(
    input_rows = nrow(data),
    transformed_rows = n,
    input_ids = length(id_rows),
    estimation_ids = length(usable_rows),
    dropped_short_ids = length(short_ids),
    minimum_periods = min(id_lengths),
    maximum_periods = max(id_lengths),
    unbalanced_panel = length(unique(id_lengths)) > 1L,
    input_was_sorted = input_was_sorted,
    endogenous = endogenous,
    outcome = outcome,
    exogenous_columns = paste(sub("__fd$", "", exogenous_names), collapse = ";"),
    dropped_collinear_exogenous = paste(dropped_collinear_exogenous, collapse = ";"),
    cluster_columns = paste(cluster, collapse = ";"),
    stringsAsFactors = FALSE
  )

  result <- list(
    coefficients = coefficient_table,
    vcov = vcov,
    first_stage = first_stage,
    fund_first_stage = fund_first_stage,
    diagnostics = diagnostics,
    residuals = residuals,
    fitted_endogenous = fitted_endogenous,
    transformed = if (isTRUE(keep_transformed)) transformed else NULL,
    design = list(regressors = x, instruments = z, projected_regressors = x_hat)
  )
  class(result) <- "recursive_demeaning_iv"
  result
}

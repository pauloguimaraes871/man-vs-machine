#' Build a rolling past-performance fund benchmark
#'
#' At each evaluation date, rank funds by their arithmetic mean return over the
#' preceding lookback window, select the top share among funds satisfying the
#' minimum-history requirement, and equal-weight their current-period returns.
#' The current return is never used to form its own ranking.
#'
#' @param fund_panel Numeric matrix with dates in rows and uniquely named funds
#'   in columns. Returns must use one consistent unit and frequency.
#' @param lookback_months Positive integer number of preceding rows used to rank
#'   funds.
#' @param min_history_months Positive integer minimum number of finite returns
#'   required within the lookback window.
#' @param top_share Proportion in the interval (0, 1] of eligible funds retained.
#' @return Numeric vector with one benchmark return per input row. The first
#'   lookback_months entries are missing.
build_funds_mom <- function(
    fund_panel,
    lookback_months = 12L,
    min_history_months = 9L,
    top_share = 0.25) {
  if (!is.matrix(fund_panel) || !is.numeric(fund_panel) ||
      nrow(fund_panel) <= lookback_months || ncol(fund_panel) == 0L) {
    rlang::abort("The fund panel must be a numeric matrix longer than the lookback window.")
  }
  if (is.null(colnames(fund_panel)) || anyDuplicated(colnames(fund_panel))) {
    rlang::abort("The fund panel must have unique fund identifiers.")
  }
  if (length(lookback_months) != 1L || !is.finite(lookback_months) ||
      lookback_months < 1L || lookback_months != as.integer(lookback_months)) {
    rlang::abort("lookback_months must be one positive integer.")
  }
  if (length(min_history_months) != 1L || !is.finite(min_history_months) ||
      min_history_months < 1L || min_history_months > lookback_months ||
      min_history_months != as.integer(min_history_months)) {
    rlang::abort("min_history_months must be an integer between 1 and lookback_months.")
  }
  if (length(top_share) != 1L || !is.finite(top_share) ||
      top_share <= 0 || top_share > 1) {
    rlang::abort("top_share must lie in (0, 1].")
  }
  benchmark <- rep(NA_real_, nrow(fund_panel))
  evaluation_rows <- seq.int(lookback_months + 1L, nrow(fund_panel))
  for (t in evaluation_rows) {
    history_rows <- seq.int(t - lookback_months, t - 1L)
    history <- fund_panel[history_rows, , drop = FALSE]
    scores <- colMeans(history, na.rm = TRUE)
    valid <- colSums(is.finite(history)) >= min_history_months & is.finite(scores)
    if (!any(valid)) next

    ranked <- names(scores)[valid][order(-scores[valid], names(scores)[valid])]
    selected_count <- max(1L, ceiling(top_share * length(ranked)))
    selected <- ranked[seq_len(selected_count)]
    current_returns <- fund_panel[t, selected, drop = TRUE]
    if (any(is.finite(current_returns))) {
      benchmark[[t]] <- mean(current_returns, na.rm = TRUE)
    }
  }
  benchmark
}

#' Compound percentage returns from an explicit unit-wealth baseline
#'
#' @param pct_returns Numeric vector of finite simple returns in percentage
#'   points. Values below -100 are invalid.
#' @param truncate_at_first_missing If TRUE, the first missing return ends the
#'   wealth path and any later observations are ignored. This makes the
#'   exit policy explicit for individual-fund dispersion paths.
#' @return Numeric vector of length length(pct_returns) + 1L; its first value
#'   is one and subsequent values are compounded terminal wealth.
compound_wealth_from_baseline <- function(
    pct_returns,
    truncate_at_first_missing = FALSE) {
  if (!is.numeric(pct_returns) || length(pct_returns) == 0L ||
      any(pct_returns[is.finite(pct_returns)] < -100)) {
    rlang::abort(
      "pct_returns must be a non-empty numeric vector with no value below -100."
    )
  }

  missing_returns <- !is.finite(pct_returns)
  if (any(missing_returns)) {
    first_missing <- which(missing_returns)[[1L]]
    if (!isTRUE(truncate_at_first_missing)) {
      rlang::abort(
        "Missing returns require truncate_at_first_missing = TRUE."
      )
    }
    observed_returns <- pct_returns[seq_len(first_missing - 1L)]
  } else {
    observed_returns <- pct_returns
  }

  observed_wealth <- cumprod(1 + observed_returns / 100)
  c(
    1,
    observed_wealth,
    rep(NA_real_, length(pct_returns) - length(observed_returns))
  )
}

# =============================================================================
# Realization-date RMSE and human-machine performance diagnostic
#
# This is a standalone exploratory analysis. It does not write manuscript
# tables, alter the figure manifest, or enter the paper's regression pipeline.
# RMSE dated t is matched to returns realised over the three monthly windows
# ending at t, which were forecast three months earlier.
# =============================================================================

source(here::here("R", "model_rmse_dynamics.R"), local = TRUE)
source(here::here("R", "newey_west.R"), local = TRUE)
source(here::here("R", "rmse_gap_diagnostics.R"), local = TRUE)

machine_run_path <- here::here("data", "derived", "machine_run", "mb_grid.qs")
raw_gap_panel_path <- here::here(
  "data", "derived", "gap_raw_fund_month_full.qs"
)
output_dir <- here::here("output", "rmse_gap_diagnostics")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(machine_run_path)) {
  rlang::abort(paste0("Missing model backtest bundle: ", machine_run_path, "."))
}
if (!file.exists(raw_gap_panel_path)) {
  rlang::abort(paste0(
    "Missing full raw-gap fund panel: ", raw_gap_panel_path,
    ". Run analysis_gap.R once to create it."
  ))
}

mb_grid <- qs::qread(machine_run_path, nthreads = 4)
raw_gap_panel <- qs::qread(raw_gap_panel_path, nthreads = 4)

model_rmse_data <- build_model_rmse_data(mb_grid)
target_difficulty <- summarise_target_difficulty(
  mb_grid@meta_sb_backtest_results@oos_sb_outputs_m_df@data
)
normalized_rmse_data <- normalize_rmse_by_target_dispersion(
  model_rmse_data,
  target_difficulty
)
horizon_performance <- build_horizon_performance(
  raw_gap_panel,
  horizons = c(1L, 3L)
)
aligned <- align_rmse_performance(
  model_rmse_data,
  horizon_performance$time_series
)
normalized_aligned <- align_rmse_performance(
  normalized_rmse_data,
  horizon_performance$time_series
)

correlations <- compute_rmse_correlations(aligned)
hac_models <- fit_hac_rmse_models(aligned, horizons = c(1L, 3L))
high_error <- compute_high_error_comparison(aligned)
normalized_correlations <- compute_rmse_correlations(normalized_aligned)
normalized_hac_models <- fit_hac_rmse_models(
  normalized_aligned,
  horizons = c(1L, 3L)
)
normalized_high_error <- compute_high_error_comparison(normalized_aligned)
lead_lag <- compute_lead_lag_correlations(
  model_rmse_data,
  horizon_performance$time_series
)

diagnostic_plot <- plot_rmse_gap_diagnostics(
  aligned,
  lead_lag,
  high_error
)
ggplot2::ggsave(
  file.path(output_dir, "rmse_gap_diagnostic.png"),
  diagnostic_plot,
  width = 12,
  height = 8,
  dpi = 320,
  bg = "white"
)

write_diagnostic_csv <- function(data, path) {
  utils::write.csv(data, path, row.names = FALSE, na = "")
}

write_diagnostic_csv(
  horizon_performance$time_series,
  file.path(output_dir, "horizon_performance.csv")
)
write_diagnostic_csv(
  aligned,
  file.path(output_dir, "rmse_performance_aligned.csv")
)
write_diagnostic_csv(
  correlations,
  file.path(output_dir, "rmse_performance_correlations.csv")
)
write_diagnostic_csv(
  hac_models,
  file.path(output_dir, "rmse_performance_hac.csv")
)
write_diagnostic_csv(
  high_error,
  file.path(output_dir, "rmse_high_error_comparison.csv")
)
write_diagnostic_csv(
  target_difficulty,
  file.path(output_dir, "target_difficulty.csv")
)
write_diagnostic_csv(
  normalized_correlations,
  file.path(output_dir, "normalized_rmse_performance_correlations.csv")
)
write_diagnostic_csv(
  normalized_hac_models,
  file.path(output_dir, "normalized_rmse_performance_hac.csv")
)
write_diagnostic_csv(
  normalized_high_error,
  file.path(output_dir, "normalized_rmse_high_error_comparison.csv")
)
write_diagnostic_csv(
  lead_lag,
  file.path(output_dir, "rmse_performance_lead_lag.csv")
)
qs::qsave(
  list(
    model_rmse_data = model_rmse_data,
    target_difficulty = target_difficulty,
    normalized_rmse_data = normalized_rmse_data,
    horizon_performance = horizon_performance,
    aligned = aligned,
    normalized_aligned = normalized_aligned,
    correlations = correlations,
    hac_models = hac_models,
    high_error = high_error,
    normalized_correlations = normalized_correlations,
    normalized_hac_models = normalized_hac_models,
    normalized_high_error = normalized_high_error,
    lead_lag = lead_lag
  ),
  file.path(output_dir, "rmse_gap_diagnostic_results.qs"),
  preset = "high"
)

headline_correlations <- correlations |>
  dplyr::filter(
    .data$model %in% c("Meta-model", "Base-learner mean"),
    .data$horizon_months == 3L,
    .data$method == "pearson"
  ) |>
  dplyr::select(
    "model", "outcome", "n", "correlation"
  )

cat("\n=== RMSE and horizon-matched three-month outcomes: Pearson correlations ===\n")
print(as.data.frame(headline_correlations), digits = 3)
cat("\n=== HAC regressions: outcome change per one-SD increase in RMSE ===\n")
print(as.data.frame(hac_models), digits = 3)
cat("\n=== Target-dispersion-normalized RMSE: HAC regressions ===\n")
print(as.data.frame(normalized_hac_models), digits = 3)
cat("\n=== Top-quartile RMSE periods versus other periods ===\n")
print(as.data.frame(high_error), digits = 3)
cat("\nDiagnostic outputs written to: ", output_dir, "\n", sep = "")

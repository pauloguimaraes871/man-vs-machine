# Copied from Multifatorial 3.0 pipeline, R/helpers/backtest_meta_portfolios.R
# at commit 8739bb0bce966dad8f7c701f0c7413a6fa25f61f (see R/pipeline/README.md).

#' Compute portfolio-level signal metrics
#'
#' Prepares a meta_dataframe of portfolio metrics by selecting relevant signals,
#' filtering to target dates, handling missing values time-wise, and applying
#' KNN imputation and winsorization using a date-aware recipe.
#'
#' @param pregold_m_df A `meta_dataframe` containing pre-gold level signals.
#' @param gold_dates A `Date` vector defining the dates to retain.
#'
#' @return A `meta_dataframe` with processed portfolio metrics.
compute_meta_port_metrics <- function(pregold_m_df, gold_dates){
  
  tictoc::tic("\nPreprocessing port metrics...\n\n")
  
  ##Pass port_metrics
  port_metrics_m_df <- pregold_m_df %>% factoRverse::screen_by_feature(
    ##Momentum
    sharpe_6m, sharpe_3m, sharpe_12m, sharpe_6m_c1_median,
    ##Value
    ebitda_ev, div_me_med_36m, fcf_me_c1_adj,
    ni_me, fcf_me,
    ##Quality
    ocf_at_12m, gp_at_12m, roa_3m, roa_12m, roe_12m,
    gross_mrg_12m, fcf_be_12m, net_mrg_12m,
    ##Growth
    ni_me_sur_5y, roe_12m_gr3y, ocf_at_12m_gr3y,
    gross_mrg_12m_gr3y,
    ##Low Vol
    beta_ibov_60m, ret_vol_12m,
    ##Size
    log_mkt_cap,
    ##Interactions
    qv, vm, smcj, garp, garp, sm, qmj
  )
  
  ##Filter dates
  port_metrics_m_df@data <- port_metrics_m_df@data %>%
    dplyr::filter(dates %in% gold_dates)
  
  ##Fill time-wise NAs with 0
  ### Init output list
  split_dates <- split(port_metrics_m_df@data, port_metrics_m_df@data$dates)
  output_list <- vector("list", length(split_dates))
  
  ###Loop through each date
  for (i in seq_along(split_dates)) {
    port_metrics_m_d_ref <- split_dates[[i]]
    date_val <- unique(port_metrics_m_d_ref$dates)
    cat("\nProcessing date:", as.Date(date_val), "\n")
    
    output_list[[i]] <- replace_all_na_by_zero(port_metrics_m_d_ref)
  }
  
  ##Bind all back
  port_metrics_m_df@data <- dplyr::bind_rows(output_list)
  
  ##Create a recipe for mapping
  set.seed(123)
  recipe <- recipes::recipe(port_metrics_m_df@data) %>%
    ###Update roles
    recipes::update_role(dplyr::all_of(c("id", "tickers", "dates")),
                         new_role = "id_vars") %>% 
    recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>% 
    ###Impute KNN
    recipes::step_impute_knn(recipes::all_numeric_predictors(), neighbors = 5) %>% 
    ###Winsorize
    factoRverse::step_winsorize(recipes::all_numeric_predictors(), probs = c(0.05, 0.95))
  
  ##Apply it
  port_metrics_m_df <- factoRverse::map_recipe_timewise(
    port_metrics_m_df, 
    recipe = recipe,
    verbose = TRUE,
    parallel = FALSE,
    type = "signals"
  )
  
  tictoc::toc() 
  
  port_metrics_m_df
  
}

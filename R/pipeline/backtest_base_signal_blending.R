# Copied from Multifatorial 3.0 pipeline, R/helpers/backtest_base_signal_blending.R
# at commit 8739bb0bce966dad8f7c701f0c7413a6fa25f61f (see R/pipeline/README.md).

#' @title Run Signal Blending Backtests with Checkpoints
#' @description Executes signal blending model backtests across multiple algorithms
#' including heuristic, statistical, tree-based, and neural networks. Handles
#' checkpointing to avoid re-running completed models, integrates sector-based IR fixes,
#' and supports parallelization.
#'
#' @param gold_m_df A `meta_dataframe` containing engineered features for signals.
#' @param gold_aux_tables A named list of auxiliary objects, including:
#'   - `ret_indexes_m_xts`: benchmark returns
#'   - `signal_themes_m_df`: signal theme classifications
#'   - `fwd_3m_target_act_ibov_m_df`: target variable
#' @param cons_ss_results_bundles A named list that includes the `winner_ss_results_bundle`
#'        and its `winner_ss_backtest_results` object, plus `extracted_returns_m_xts`.
#' @param chosen_sb_configs Character vector specifying which SB configs to run.
#'        Must match allowed configs: e.g., "ols", "sw_ir", "xgb_bayes", etc.
#' @param parallel Logical. If TRUE, enables internal parallelization for models with
#'        hyperparameter tuning (e.g., RF, NN, XGB, GLMNET).
#'
#' @return A named list of class `sb_backtest_results`, one per requested configuration.
#'         Each element contains the backtest outputs for the corresponding signal blending algorithm.
#'
#' @details
#' The function builds all predefined SB model configurations, including:
#' - Heuristic: `ew`, `sw_ir`, `rp_cc`, `mvo_ir_cc_con`
#' - Statistical: `ols`, `glmnet_grid`, `glmnet_bayes`
#' - Tree-based: `rf_grid`, `rf_bayes`, `xgb_grid`, `xgb_bayes`
#' - Neural networks: `nn1_grid`, `nn1_bayes`, `nn2_grid`, `nn2_bayes`, `nn3_grid`, `nn3_bayes`
#'
#' For models with IR-based construction, a fallback IR calculation is computed using
#' sector portfolios if needed.
#'
#' The function supports checkpointing: each model is saved with a unique tag, and completed
#' configurations will be skipped unless the checkpoint is removed.
#'
#' @export
backtest_base_signal_blending <- function(gold_signals_m_df, gold_clusters_m_df,
                                          gold_aux_tables, 
                                          winner_ss_backtest_results_list,
                                          extracted_returns_m_xts_list,
                                          signal_port_backtest_cohorts,
                                          old_base_sb_backtest_results_list = NULL,
                                          batches_dates = NULL,
                                          winner_dataset = NULL,
                                          chosen_sb_configs,
                                          validation_sample_size,
                                          source,
                                          parallel){
  
  #Initial Setup----------------------------------------------------------------

  ##Check if chosen_sb_configs match expectations
  tuned_algos     <- c("glmnet", "rf", "xgb", "nn1", "nn2", "nn3")
  tuning_method   <- c("_grid", "_bayes")
  non_tuned_algos <- c("ew", "sw_val", "sw_mom", "sw_lt_perf", "sw_overall",
                       "rp_cc", "hrp_cc", "mvo_overall_cc_con", "mmaf_overall_cc_sw_hrp_top", 
                       "mmaf_overall_cc_hrp_sw_bottom", "mmaf_cc_hrp_hrp_top",
                       "ols")
  allowable_configs <- c(paste0(
    rep(tuned_algos, each = length(tuning_method)), tuning_method
  ), non_tuned_algos
  )
  
  if (any(!chosen_sb_configs %in% allowable_configs)){
    stop("chosen_sb_config not allowed")
  }
  
  #Prepare objects--------------------------------------------------------------  
  message("\nExtracting objects...\n\n")  
  
  ##Extract objs
  gold_dates <- gold_signals_m_df@data %>% dplyr::pull(dates) %>% unique() %>% sort()
  gold_aux_tables <- prepare_gold_aux_to_backtest(
    gold_aux_tables = gold_aux_tables, gold_dates = gold_dates
    )
  
    ###Themes
    signal_themes_m_df   <- gold_aux_tables$signal_themes_m_df
    cluster_themes_m_df  <- gold_aux_tables$adj_cluster_themes_m_df
    signal_clusters_m_df <- gold_aux_tables$signal_clusters_m_df
    
    ###Gold Clusters (Remove Ind Signals)
    gold_clusters_m_df@data <- gold_clusters_m_df@data %>%
      dplyr::select(
        id, tickers, dates, dplyr::contains("fac_"),
        signal_clusters_m_df@data %>%
          dplyr::filter(cluster %in% c("industries", "interactions")) %>%
          dplyr::pull(tickers) %>% 
          unique()
      )
    
    
    ###backtest_returns_m_xts and benchmark_returns_m_xts
    returns_ind_signals_m_xts <- extracted_returns_m_xts_list$returns_ind_signals_m_xts
    returns_clusters_m_xts    <- extracted_returns_m_xts_list$returns_clusters_m_xts
    ret_indexes_m_xts         <- extracted_returns_m_xts_list$ret_indexes_m_xts
    ret_indexes_m_xts@data    <- ret_indexes_m_xts@data[, "ibov", drop = FALSE]
    
    ###SS Backtest Results
    winner_ss_backtest_results_ind_signals <- winner_ss_backtest_results_list$ind_signals
    winner_ss_backtest_results_clusters    <- winner_ss_backtest_results_list$clusters
    
      ####training and val sample sizes
      training_sample_size        <- winner_ss_backtest_results_ind_signals@ss_backtest_workflow[[1]]$initial_sample_size
      validation_sample_size      <- validation_sample_size
      rebal_months_ml             <- winner_ss_backtest_results_ind_signals@ss_backtest_config@rebalancing_months
      rebal_months_heuristics     <- 
        signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals@port_backtest_results_list[[1]]@port_backtest_config@rebalancing_months
      
    ###Add an additional month at the beginning backtest to satisfy sb reqs
      ####Get info for first row
      first_date <- gold_signals_m_df@data %>% dplyr::pull(dates) %>% min()
      first_return <- as.numeric(
        restore_meta_xts_dates(gold_aux_tables$ret_indexes_m_xts)@data[first_date, "ibov"]
      )
  
      ####Create and add first row
        #####returns_ind_signals_m_xts
        first_row_returns_ind_signals_xts <- xts::xts(
          matrix(first_return, nrow = 1, ncol = ncol(returns_ind_signals_m_xts@data)),
          order.by = as.Date(first_date)
        )
        names(first_row_returns_ind_signals_xts) <- names(returns_ind_signals_m_xts@data)
        #####Add only if this date is not already present
        if (!zoo::index(first_row_returns_ind_signals_xts) %in% zoo::index(returns_ind_signals_m_xts@data)){
          returns_ind_signals_m_xts@data <- rbind(
            first_row_returns_ind_signals_xts, returns_ind_signals_m_xts@data
          )
        }
        #####returns_clusters_m_xts
        first_row_returns_clusters_xts <- xts::xts(
          matrix(first_return, nrow = 1, ncol = ncol(returns_clusters_m_xts@data)),
          order.by = as.Date(first_date)
        )
        names(first_row_returns_clusters_xts) <- names(returns_clusters_m_xts@data)
        #####Add only if this date is not already present
        if (!zoo::index(first_row_returns_clusters_xts) %in% zoo::index(returns_clusters_m_xts@data)){
          returns_clusters_m_xts@data <- rbind(
            first_row_returns_clusters_xts, returns_clusters_m_xts@data
          )
        }
        #####Benchmarks
        first_row_ret_indexes_xts <- xts::xts(
          matrix(first_return, nrow = 1, ncol = ncol(ret_indexes_m_xts@data)),
          order.by = as.Date(first_date)
        )
        names(first_row_ret_indexes_xts) <- names(ret_indexes_m_xts@data)
        #####Add only if this date is not already present
        if (!zoo::index(first_row_ret_indexes_xts) %in% zoo::index(ret_indexes_m_xts@data)){
          ret_indexes_m_xts@data <- rbind(
            first_row_ret_indexes_xts, ret_indexes_m_xts@data
          )
        }
          
        
        
  ##While we want to force inclusion of sectors in ss, their IR is not calculated
  ##Which is a problem for heuristic ports. For those, we recalculate IR
  sectors_port_ss_backtest_results <- run_sectors_port_ss_backtest_results(
    gold_signals_m_df                      = gold_signals_m_df,
    signal_themes_m_df                     = signal_themes_m_df,
    returns_ind_signals_m_xts              = returns_ind_signals_m_xts,
    ret_indexes_m_xts                      = ret_indexes_m_xts,
    winner_ss_backtest_results_ind_signals = winner_ss_backtest_results_ind_signals
  )

  ###Replace data
    ####Ind Signals
    winner_ss_backtest_results_ind_signals@signal_universe_m_df@data <- 
      winner_ss_backtest_results_ind_signals@signal_universe_m_df@data %>%
      dplyr::left_join(
        sectors_port_ss_backtest_results@signal_universe_m_df@data %>%
          dplyr::select(id, info_ratio) %>%
          dplyr::rename(info_ratio2 = info_ratio),
        by = "id"
      )  %>% 
      dplyr::mutate(
        info_ratio = dplyr::if_else(
          is.na(info_ratio) & theme == "forced",
          info_ratio2,
          info_ratio
        )
      ) %>% 
      dplyr::select(-info_ratio2)
    ####Clusters
    winner_ss_backtest_results_clusters@signal_universe_m_df@data <- 
      winner_ss_backtest_results_clusters@signal_universe_m_df@data %>%
      dplyr::left_join(
        sectors_port_ss_backtest_results@signal_universe_m_df@data %>%
          dplyr::select(id, info_ratio) %>%
          dplyr::rename(info_ratio2 = info_ratio),
        by = "id"
      )  %>% 
      dplyr::mutate(
        info_ratio = dplyr::if_else(
          is.na(info_ratio) & theme == "forced",
          info_ratio2,
          info_ratio
        )
      ) %>% 
      dplyr::select(-info_ratio2)
    
  
  
  gc()
  
  #Compute factor timing metrics------------------------------------------------
  
  signal_val_metrics <- c("div_me", "fcf_me", "ni_me", "ocf_me_c1_adj")
  lt_performance_metrics <- c("info_ratio", "act_ann_martin_ratio", "act_ann_calmar_ratio", 
                              "alpha", "alpha_t_stat", "appraisal_ratio") 
  
  computed_signal_metrics_list <- compute_signal_metrics(
    winner_ss_backtest_results_ind_signals = winner_ss_backtest_results_ind_signals,
    winner_ss_backtest_results_clusters    = winner_ss_backtest_results_clusters,
    signal_val_metrics                     = signal_val_metrics,
    lt_performance_metrics                 = lt_performance_metrics,
    signal_port_backtest_cohorts           = signal_port_backtest_cohorts,
    returns_ind_signals_m_xts              = returns_ind_signals_m_xts,
    returns_clusters_m_xts                 = returns_clusters_m_xts,
    training_sample_size                   = training_sample_size,
    validation_sample_size                 = validation_sample_size,
    rebal_months_heuristics                = rebal_months_heuristics
  )
        
   
  #Create SB Configs-----------------------------------------------
  base_sb_configs_list <- create_sb_configs(
    old_base_sb_backtest_results_list = old_base_sb_backtest_results_list,
    training_sample_size              = training_sample_size + 1,
    validation_sample_size            = validation_sample_size,
    rebal_months_ml                   = rebal_months_ml,
    rebal_months_heuristics           = rebal_months_heuristics
  ) 
  
  #Run models-------------------------------------------------------------------
  tictoc::tic("\nRunning SB backtests...\n\n")  
  
  ##Define groups
  groups <- list(
    ind_signals = list(
      gold_m_df              = gold_signals_m_df,
      ss_backtest_results    = winner_ss_backtest_results_ind_signals,
      backtest_returns_m_xts = returns_ind_signals_m_xts,
      signal_themes_m_df     = signal_themes_m_df,
      signal_metrics_m_df    = computed_signal_metrics_list$signal_metrics_ind_signals_m_df
    ),
    clusters    = list(
      gold_m_df              = gold_clusters_m_df,
      ss_backtest_results    = winner_ss_backtest_results_clusters,
      backtest_returns_m_xts = returns_clusters_m_xts,
      signal_themes_m_df     = cluster_themes_m_df,
      signal_metrics_m_df    = computed_signal_metrics_list$signal_metrics_clusters_m_df
    )
  )
  
  ##If in an update, subset winner dataset
  if (!is.null(winner_dataset)){
    groups <- groups[winner_dataset] ##Important to keep single [] to keep list structure
  }
  
  ##Run backtest
  tictoc::tic("\nRunning SB backtests...\n\n")
  base_sb_backtest_results_lists <- run_all_sb_backtests(
    old_base_sb_backtest_results_list = old_base_sb_backtest_results_list,
    batches_dates                     = batches_dates,
    groups                            = groups,
    gold_aux_tables                   = gold_aux_tables,
    ret_indexes_m_xts                 = ret_indexes_m_xts,
    base_sb_configs_list              = base_sb_configs_list,
    chosen_sb_configs                 = chosen_sb_configs,
    parallel                          = parallel,
    source                            = source
  ) 
  tictoc::toc()
  
  
  #Proof of continuity----------------------------------------------------------
  if (!is.null(old_base_sb_backtest_results_list)){
    
    ### Proof of continuity needs to be based on RP, SW GLMNET, since they are
    ### quick and deterministic and representative of their 'species'
    message("\nTaking proof of continuity for backtest results...")
    
    rp_cc_clusters_config  <- base_sb_configs_list$rp_cc_model_config  
    sw_overall_config      <- base_sb_configs_list$sw_overall_model_config
    
    ##Run both backtests
    #### Seeds
    set.seed(123)
    tensorflow::tf$random$set_seed(123)
      
      ###RF
      groups_rp <- adjust_signal_universe(groups$clusters, metric_col = NULL,
                                          sectors_treatment      = "reclassify")
    
      rp_cc_check <- run_one_sb_backtest(
        sb_config              = rp_cc_clusters_config,
        gold_m_df              = groups_rp$gold_m_df,
        gold_aux_tables        = gold_aux_tables,
        backtest_returns_m_xts = groups_rp$backtest_returns_m_xts,
        benchmark_returns_m_xts= ret_indexes_m_xts,
        ss_backtest_results    = groups_rp$ss_backtest_results,
        signal_themes_m_df     = groups_rp$signal_themes_m_df,
        inner_parallel         = FALSE,
        verbose                = TRUE
        )
      
      ###SW
      groups_sw <- adjust_signal_universe(groups$clusters, metric_col = "overall_agg_z_score",
                                          sectors_treatment      = "exclude")
      
      sw_overall_check <- run_one_sb_backtest(
        sb_config              = sw_overall_config,
        gold_m_df              = groups_sw$gold_m_df,
        gold_aux_tables        = gold_aux_tables,
        backtest_returns_m_xts = groups_sw$backtest_returns_m_xts,
        benchmark_returns_m_xts= ret_indexes_m_xts,
        ss_backtest_results    = groups_sw$ss_backtest_results,
        signal_themes_m_df     = groups_sw$signal_themes_m_df,
        inner_parallel         = FALSE,
        verbose                = TRUE
      )
      
    proof_of_continuity <- list(
      rp_cc_check        = rp_cc_check,
      sw_overall_check   = sw_overall_check
    )
    
    
  }
  
  #Return-----------------------------------------------------------------------
  return(list(
    base_sb_backtest_results_lists    = base_sb_backtest_results_lists,
    computed_signal_metrics_list      = computed_signal_metrics_list,
    base_sb_configs_list              = base_sb_configs_list,
    proof_of_continuity               = if (exists("proof_of_continuity")) proof_of_continuity else NULL
  ))
  
  
}

#' Run sector-level signal selection backtest
#'
#' Executes a signal-selection backtest restricted to predefined sector
#' portfolios using the configuration derived from the winning individual
#' signal backtest.
#'
#' @param gold_signals_m_df Meta-dataframe with signal inputs.
#' @param signal_themes_m_df Dataframe mapping signals to themes/sectors.
#' @param returns_ind_signals_m_xts Signal return time-series.
#' @param ret_indexes_m_xts Benchmark return series.
#' @param winner_ss_backtest_results_ind_signals Winning individual signal
#'   backtest results used to derive configuration parameters.
#'
#' @return `ss_backtest_results` object for sector portfolios.
run_sectors_port_ss_backtest_results <- function(gold_signals_m_df, signal_themes_m_df,
                                                 returns_ind_signals_m_xts, ret_indexes_m_xts,
                                                 winner_ss_backtest_results_ind_signals){
  ###Run new backtest
  message("\n\nRunning sector ports backtest...\n\n")
  sector_ports_config <- factoRverse::create_ss_backtest_config(
    initial_sample_size = winner_ss_backtest_results_ind_signals@ss_backtest_workflow[[1]]$initial_sample_size + 1, #106 bco of first_row added in m_xts
    rebalancing_months = winner_ss_backtest_results_ind_signals@ss_backtest_config@rebalancing_months,
    active_returns = TRUE,
    chosen_signals_and_positions = c(
      pro_cyclicals = "long", bond_proxies = "long", industrials = "long",
      defensives = "long", commodities = "long"
    ),
    split_method = "expanding", config_name = "sector_ports"
  ) %>% factoRverse::add_alpha_test_strategy(
    model_structure = "no_pooled", signal_significance_threshold = 0.99,
    p_correction_method = "none", market_factor_proxy = "ibov",
    enable_theme_representativeness = TRUE
  )
  
  sectors_port_ss_backtest_results <- factoRverse::run_ss_backtest(
    config = sector_ports_config, signals_m_df = gold_signals_m_df,
    backtest_returns_m_xts = returns_ind_signals_m_xts,
    benchmark_returns_m_xts = ret_indexes_m_xts,
    signal_themes_m_df = signal_themes_m_df
  )
}

compute_signal_metrics <- function(winner_ss_backtest_results_ind_signals,
                                   winner_ss_backtest_results_clusters,
                                   signal_port_backtest_cohorts,
                                   signal_val_metrics, lt_performance_metrics,  
                                   returns_ind_signals_m_xts, returns_clusters_m_xts,
                                   training_sample_size, validation_sample_size,
                                   rebal_months_heuristics){
  
  message("\nComputing signal metrics...\n\n")
  
  ### Setup---------------------------------------------------------------------
  
  #### Restore meta xts dates
  returns_ind_signals_m_xts <- restore_meta_xts_dates(returns_ind_signals_m_xts)  
  returns_clusters_m_xts    <- restore_meta_xts_dates(returns_clusters_m_xts)
  
  #### Define relevant dates
  first_date <- returns_ind_signals_m_xts@data %>%
    zoo::index() %>%
    as.Date() %>%
    min()
  first_training_date <- lubridate::add_with_rollback(
    first_date, months(training_sample_size)
  )
  
  ### Valuation Metrics---------------------------------------------------------
  message("\nComputing signal valuation metrics...\n\n")
  compute_val_metrics_m_df <- function(signal_val_metrics, signal_port_backtest_cohort,
                                       initial_window, first_training_date){
    
    #### Extract each valuation metric, compute zscores and spreads, and join in a single df
    signal_val_metrics_m_df <- purrr::map(
      signal_val_metrics, function(curr_signal_val_metric){
        
        ##Extract metric
        curr_signal_val_metric_m_xts <- 
          signal_port_backtest_cohort@port_metrics_m_xts_list[[paste0(curr_signal_val_metric, "_m_xts")]]
        
        ##Clean colnames
        short_signals <- stringr::str_detect(names(curr_signal_val_metric_m_xts@data), "_short_s__")
        names(curr_signal_val_metric_m_xts@data) <- names(curr_signal_val_metric_m_xts@data) %>%
          stringr::str_remove("c__cs_") %>%
          stringr::str_remove("c__sw_") %>%
          stringr::str_remove("_long_s__comdinheiro_pregold_f__comdinheiro_target") %>%
          stringr::str_remove("_short_s__comdinheiro_pregold_f__comdinheiro_target")
        names(curr_signal_val_metric_m_xts@data)[short_signals] <-
          paste0("low_", names(curr_signal_val_metric_m_xts@data)[short_signals])
        
        ##Compute spread
        curr_signal_val_spread_wide_df <- curr_signal_val_metric_m_xts@data %>%
          as.data.frame() %>%
          ### Compute the spread between current valuation level and benchmark
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns  = function(curr_col){
                curr_col - .data[[paste0("bench_", curr_signal_val_metric)]]
              },
              .names = "{.col}_spread"
            )
          ) %>%
          dplyr::select(
            dplyr::ends_with("_spread"), -dplyr::contains("bench")
          ) %>%
          tibble::rownames_to_column("dates") %>%
          dplyr::mutate(dates = as.Date(dates)) %>%
          dplyr::arrange(dates) 
        ### Get long version
        curr_signal_val_spread_long_df  <- curr_signal_val_spread_wide_df %>%
          tidyr::pivot_longer(
            cols = -dates, names_to = "tickers", values_to = paste0(curr_signal_val_metric, "_spread")
          ) %>%
          dplyr::mutate(tickers = stringr::str_remove(tickers, "_spread"),
                        id = paste0(tickers, "-", dates)) %>%
          dplyr::select(id, tickers, dates, dplyr::contains("_spread"))
        
        ##Compute zscores
        curr_signal_val_zscore_wide_df <- curr_signal_val_spread_wide_df %>%
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::contains("_spread"),
              .fns  = ~ {
                x <- .x ## To avoid NSE issues with .x in the function, we assign it to a local variable
                
                slider::slide_dbl(
                  .x = x,
                  .f = function(window) {
                    
                    ### Guard
                    if (length(window) < initial_window) return(NA_real_)
                    
                    ### Get data
                    curr <- tail(window, 1)
                    past <- head(window, -1)
                    
                    ### Gate
                    if (length(past) < initial_window) return(NA_real_)
                    
                    ### Compute zscore
                    sd_past <- stats::sd(past)
                    if (sd_past == 0 || is.na(sd_past)) return(NA_real_)
                    
                    mean_past <- mean(past)
                    (curr - mean_past) / sd_past
                    
                  },
                  .before = Inf,
                  .complete = FALSE
                )
                
              },
              .names = "{.col}_z"
            )
          ) %>%
          dplyr::arrange(dates) %>%
          dplyr::select(dates, dplyr::ends_with("_z"))
        ### Get long version
        curr_signal_val_zscore_long_df  <- curr_signal_val_zscore_wide_df %>%
          tidyr::pivot_longer(
            cols = -dates, names_to = "tickers", values_to = paste0(curr_signal_val_metric, "_z_score")
          ) %>%
          dplyr::mutate(tickers = stringr::str_remove(tickers, "_spread_z"),
                        id = paste0(tickers, "-", dates)) %>%
          dplyr::select(id, tickers, dates, dplyr::contains("_z_score"))
        
        #### Join and return
        curr_signal_val_spread_long_df %>%
          dplyr::left_join(
            curr_signal_val_zscore_long_df, by = c("id", "tickers", "dates")
          )
      }
    ) %>%
      purrr::reduce(dplyr::left_join, by = c("id", "tickers", "dates")) %>%
      as.data.frame() 
    
    ##### When does each spread column stops being 0?
    metrics_aniversaries <- signal_val_metrics_m_df %>%
      dplyr::select(dates, dplyr::contains("_spread")) %>%
      tidyr::pivot_longer(
        cols = -dates, names_to = "metric", values_to = "spread"
      ) %>% 
      dplyr::group_by(metric) %>%
      dplyr::summarise(
        first_non_zero_spread_date = min(dates[spread != 0], na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        metric_aniversary = lubridate::add_with_rollback(
          first_non_zero_spread_date, months(initial_window)
        )
      )
    
    ##### Replace all z_score columns before 10y anniversary with NA_real_
    signal_val_metrics_m_df <- signal_val_metrics_m_df %>% 
      dplyr::mutate(
        dates = as.Date(dates),
        dplyr::across(
          dplyr::contains("_z_score"),
          ~ {
            ### Get metric anniversary
            metric_name <- stringr::str_remove(dplyr::cur_column(), "_z_score")
            aniversary_date <- metrics_aniversaries %>%
              dplyr::filter(metric == paste0(metric_name, "_spread")) %>%
              dplyr::pull(metric_aniversary)
            
            ifelse(dates < aniversary_date, NA_real_, .x)
          }
        )
      ) %>%
      dplyr::arrange(id)
    
    ##### Guard test that we have only zeros for dates before aniversary
    check <- signal_val_metrics_m_df %>%
      dplyr::select(dates, dplyr::contains("_z_score")) %>%
      tidyr::pivot_longer(
        cols = -dates, names_to = "metrics", values_to = "z_score"
      ) %>% 
      dplyr::mutate(metrics = stringr::str_remove(metrics, "_z_score")) %>%
      dplyr::left_join(
        metrics_aniversaries %>% 
          dplyr::mutate(metric = stringr::str_remove(metric, "_spread")),
        by = c("metrics" = "metric")
      ) %>% 
      dplyr::mutate(
        check = ifelse(dates < metric_aniversary, is.na(z_score), TRUE)
      ) %>% 
      dplyr::summarise(all_checks_passed = all(check))
    
    if (!check$all_checks_passed){
      stop("Not all z_scores before metric anniversary are NA")
    }
    
    #### Compute an agg val metric
    signal_val_metrics_m_df <- signal_val_metrics_m_df %>% 
      dplyr::group_by(dates) %>%
      dplyr::mutate(
        val_agg_z_score = rowMeans(
          dplyr::across(
            dplyr::contains("_z_score"),
            .fns = ~ factoRverse::signal_transform(.x)
          ),
          na.rm = TRUE
        )
      ) %>%
      dplyr::ungroup() %>%
      as.data.frame()
    
    #### Filter for first oos date
    signal_val_metrics_m_df %>% 
      dplyr::filter(dates >= as.Date(first_training_date)) %>%
      dplyr::arrange(id) %>%
      factoRverse::create_meta_dataframe()
  }
  
  #### Compute for individual signals and clusters      
  signal_val_metrics_ind_signals_m_df <- compute_val_metrics_m_df(
    signal_val_metrics          = signal_val_metrics,
    signal_port_backtest_cohort = signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals,
    initial_window              = training_sample_size,
    first_training_date         = first_training_date
  )
  signal_val_metrics_clusters_m_df <- compute_val_metrics_m_df(
    signal_val_metrics          = signal_val_metrics,
    signal_port_backtest_cohort = signal_port_backtest_cohorts$signal_port_backtest_cohort_clusters,
    initial_window              = training_sample_size,
    first_training_date         = first_training_date
  )
  
  ### Long Term Performance------------------------------------------------------
  message("\nComputing long term performance metrics...\n\n")  
  
  #### Define helper
  compute_lt_perf_metrics_m_df <- function(lt_performance_metrics, signal_port_backtest_cohort,
                                           first_training_date){
    
    #### Extract each lt performance metric, clean colnames, and join in a single df 
    signal_lt_perf_metrics <- purrr::map(
      lt_performance_metrics, function(curr_lt_performance_metric){
        
        ##Extract stat
        curr_signal_stats_m_xts <- 
          signal_port_backtest_cohort@port_stats_m_xts_nested_list$net_return[[paste0(curr_lt_performance_metric, "_m_xts")]]
        
        ##Clean colnames
        short_signals <- stringr::str_detect(names(curr_signal_stats_m_xts@data), "_short_s__")
        names(curr_signal_stats_m_xts@data) <- names(curr_signal_stats_m_xts@data) %>%
          stringr::str_remove("c__cs_") %>%
          stringr::str_remove("c__sw_") %>%
          stringr::str_remove("_long_s__comdinheiro_pregold_f__comdinheiro_target") %>%
          stringr::str_remove("_short_s__comdinheiro_pregold_f__comdinheiro_target")
        names(curr_signal_stats_m_xts@data)[short_signals] <-
          paste0("low_", names(curr_signal_stats_m_xts@data)[short_signals])
        
        curr_signal_stats_m_df <- curr_signal_stats_m_xts@data %>%
          as.data.frame() %>%
          tibble::rownames_to_column("dates") %>%
          dplyr::mutate(dates = as.Date(dates)) %>%
          tidyr::pivot_longer(
            cols = -dates, names_to = "tickers", values_to = curr_lt_performance_metric
          ) %>%
          dplyr::mutate(
            id = paste0(tickers, "-", dates) 
          ) %>%
          dplyr::select(id, tickers, dates, dplyr::all_of(curr_lt_performance_metric))
        
        curr_signal_stats_m_df
      }
    ) %>% 
      purrr::reduce(dplyr::left_join, by = c("id", "dates", "tickers")) %>%
      as.data.frame()
    
    ##### Compute an agg lt perf metric after filtering for first oos date
    signal_lt_perf_metrics %>% 
      dplyr::filter(dates >= as.Date(first_training_date)) %>%
      dplyr::group_by(dates) %>%
      dplyr::mutate(
        lt_perf_agg_z_score = rowMeans(
          dplyr::across(
            dplyr::all_of(lt_performance_metrics),
            .fns = ~ factoRverse::signal_transform(.x)
          ),
          na.rm = TRUE
        )
      ) %>%
      dplyr::ungroup() %>%
      as.data.frame() %>%
      dplyr::arrange(id) %>%
      factoRverse::create_meta_dataframe()
    
  }
  #### Ind Signals    
  signal_lt_perf_metrics_ind_signals_m_df <- compute_lt_perf_metrics_m_df(
    lt_performance_metrics      = lt_performance_metrics,
    signal_port_backtest_cohort = signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals,
    first_training_date         = first_training_date
  )
  
  #### Clusters    
  signal_lt_perf_metrics_clusters_m_df <- compute_lt_perf_metrics_m_df(
    lt_performance_metrics      = lt_performance_metrics,
    signal_port_backtest_cohort = signal_port_backtest_cohorts$signal_port_backtest_cohort_clusters,
    first_training_date         = first_training_date
  )
  
  #### Do a first consolidation before passing to factor momentum
  signal_metrics_ind_signals_m_df <- signal_val_metrics_ind_signals_m_df@data %>% 
    dplyr::left_join(
      signal_lt_perf_metrics_ind_signals_m_df@data %>% 
        dplyr::select(id, dplyr::contains("lt_perf_agg_z_score")),
      by = "id"
    ) %>% factoRverse::create_meta_dataframe()
  signal_metrics_clusters_m_df <- signal_val_metrics_clusters_m_df@data %>%
    dplyr::left_join(
      signal_lt_perf_metrics_clusters_m_df@data %>% 
        dplyr::select(id, dplyr::contains("lt_perf_agg_z_score")),
      by = "id"
    ) %>% factoRverse::create_meta_dataframe()
  
  ### Factor Momentum----------------------------------------------------------
  message("\nComputing factor momentum metrics...\n\n")  
  #### Correction for extracted_returns_m_xts bug
  returns_ind_signals_m_xts@source <-  returns_ind_signals_m_xts@source[
    -length(names(returns_ind_signals_m_xts@data))
  ]
  returns_clusters_m_xts@source <-  returns_clusters_m_xts@source[
    -length(names(returns_clusters_m_xts@data))
  ]
  
  #### Ret 3m
  ##### Individual Signals
  signal_metrics_ind_signals_m_df <- compute_features_windowed(
    data_xts       = returns_ind_signals_m_xts, col_names = colnames(returns_ind_signals_m_xts@data),
    window = "rolling", only_unique = FALSE, 
    mkt_pregold_m_df = signal_metrics_ind_signals_m_df,
    period         = 2, min_non_na = 3, FUN = "geom_mean_ret", feature_name_prefix = "fac_ret_3m",
    na.rm          = FALSE, parallel = FALSE
  )
  ##### Clusters
  signal_metrics_clusters_m_df <- compute_features_windowed(
    data_xts       = returns_clusters_m_xts, col_names = colnames(returns_clusters_m_xts@data),
    window = "rolling", only_unique = FALSE,
    mkt_pregold_m_df = signal_metrics_clusters_m_df,
    period         = 2, min_non_na = 3, FUN = "geom_mean_ret", feature_name_prefix = "fac_ret_3m",
    na.rm          = FALSE, parallel = FALSE
  )
  
  #### Ret 1m
  ##### Individual Signals
  signal_metrics_ind_signals_m_df <- compute_features_windowed(
    data_xts       = returns_ind_signals_m_xts, col_names = colnames(returns_ind_signals_m_xts@data),
    window = "rolling", only_unique = FALSE,
    mkt_pregold_m_df = signal_metrics_ind_signals_m_df,
    period         = 0, min_non_na = 1, FUN = "median", feature_name_prefix = "fac_ret_1m",
    na.rm          = FALSE, parallel = FALSE
  )
  ##### Clusters
  signal_metrics_clusters_m_df <- compute_features_windowed(
    data_xts       = returns_clusters_m_xts, col_names = colnames(returns_clusters_m_xts@data),
    window = "rolling", only_unique = FALSE,
    mkt_pregold_m_df = signal_metrics_clusters_m_df,
    period         = 0, min_non_na = 1, FUN = "median", feature_name_prefix = "fac_ret_1m",
    na.rm          = FALSE, parallel = FALSE
  )
  
  #### Vol 36m
  ##### Individual Signals
  signal_metrics_ind_signals_m_df <- compute_features_windowed(
    data_xts       = returns_ind_signals_m_xts, col_names = colnames(returns_ind_signals_m_xts@data),
    window = "rolling", only_unique = FALSE,
    mkt_pregold_m_df = signal_metrics_ind_signals_m_df,
    period         = 35, min_non_na = 30, FUN = "sd", feature_name_prefix = "fac_sd_36m",
    na.rm          = FALSE, parallel = FALSE
  )
  ##### Clusters
  signal_metrics_clusters_m_df <- compute_features_windowed(
    data_xts       = returns_clusters_m_xts, col_names = colnames(returns_clusters_m_xts@data),
    window = "rolling", only_unique = FALSE,
    mkt_pregold_m_df = signal_metrics_clusters_m_df,
    period         = 35, min_non_na = 30, FUN = "sd", feature_name_prefix = "fac_sd_36m",
    na.rm          = FALSE, parallel = FALSE
  )
  
  #### Standardize returns as in Factor Momentum Everywhere
  ##### Individual Signals
  signal_metrics_ind_signals_m_df <- signal_metrics_ind_signals_m_df %>% 
    factoRverse::compute_formula(
      formula = fac_ret_1m_sd ~ fac_ret_1m / fac_sd_36m
    ) %>%
    factoRverse::compute_formula(
      formula = fac_ret_3m_sd ~ fac_ret_3m / fac_sd_36m
    )
  
  ##### Clusters
  signal_metrics_clusters_m_df <- signal_metrics_clusters_m_df %>% 
    factoRverse::compute_formula(
      formula = fac_ret_1m_sd ~ fac_ret_1m / fac_sd_36m
    ) %>%
    factoRverse::compute_formula(
      formula = fac_ret_3m_sd ~ fac_ret_3m / fac_sd_36m
    )
  
  
  #### Agg Momentum
  ##### Individual Signals
  signal_metrics_ind_signals_m_df@data <- signal_metrics_ind_signals_m_df@data %>% 
    dplyr::group_by(dates) %>%
    dplyr::mutate(
      mom_agg_z_score = rowMeans(
        dplyr::across(
          dplyr::ends_with("m_sd"),
          .fns = ~ factoRverse::signal_transform(.x)
        ),
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  ##### Clusters
  signal_metrics_clusters_m_df@data <- signal_metrics_clusters_m_df@data %>% 
    dplyr::group_by(dates) %>%
    dplyr::mutate(
      mom_agg_z_score = rowMeans(
        dplyr::across(
          dplyr::ends_with("m_sd"),
          .fns = ~ factoRverse::signal_transform(.x)
        ),
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  ### Compute overall metric----------------------------------------------------
  message("\nComputing overall metric...\n\n")  
  agg_metrics <- c(
    "val_agg_z_score", "mom_agg_z_score", "lt_perf_agg_z_score"
  )  
  
  
  ####Individual Signals
  signal_metrics_ind_signals_m_df <- signal_metrics_ind_signals_m_df@data %>% 
    dplyr::filter(lubridate::month(dates) %in% rebal_months_heuristics) %>%
    dplyr::group_by(dates) %>%
    dplyr::mutate(
      overall_agg_z_score = rowMeans(
        dplyr::across(
          dplyr::all_of(agg_metrics),
          .fns = ~ factoRverse::signal_transform(.x)
        ),
        na.rm = FALSE
      )
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    dplyr::arrange(id) %>%
    factoRverse::create_meta_dataframe()
  
  ####Clusters
  signal_metrics_clusters_m_df <- signal_metrics_clusters_m_df@data %>% 
    dplyr::filter(lubridate::month(dates) %in% rebal_months_heuristics) %>%
    dplyr::group_by(dates) %>%
    dplyr::mutate(
      overall_agg_z_score = rowMeans(
        dplyr::across(
          dplyr::all_of(agg_metrics),
          .fns = ~ factoRverse::signal_transform(.x)
        ),
        na.rm = FALSE
      )
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame()  %>%
    dplyr::arrange(id) %>%
    factoRverse::create_meta_dataframe()
  
  ###Return---------------------------------------------------------------------
  
  #### Guard: no NAs in agg metrics
  if (any(is.na(signal_metrics_ind_signals_m_df@data$val_agg_z_score))){
    stop("NA values found in val_agg_z_score for individual signals")
  }
  if (any(is.na(signal_metrics_clusters_m_df@data$val_agg_z_score))){
    stop("NA values found in val_agg_z_score for clusters")
  }
  if (any(is.na(signal_metrics_ind_signals_m_df@data$mom_agg_z_score))){
    stop("NA values found in mom_agg_z_score for individual signals")
  }
  if (any(is.na(signal_metrics_clusters_m_df@data$mom_agg_z_score))){
    stop("NA values found in mom_agg_z_score for clusters")
  }
  if (any(is.na(signal_metrics_ind_signals_m_df@data$lt_perf_agg_z_score))){
    stop("NA values found in lt_perf_agg_z_score for individual signals")
  }
  if (any(is.na(signal_metrics_clusters_m_df@data$lt_perf_agg_z_score))){
    stop("NA values found in lt_perf_agg_z_score for clusters")
  }
  
  return(
    list(
      signal_metrics_ind_signals_m_df = signal_metrics_ind_signals_m_df,
      signal_metrics_clusters_m_df    = signal_metrics_clusters_m_df
    )
  )      
  
}

create_sb_configs <- function(training_sample_size, validation_sample_size,
                              rebal_months_ml, rebal_months_heuristics,
                              old_base_sb_backtest_results_list = NULL){
  
  message("\nSetting/Retrieving SB Model Configs...\n\n") 
  if (!is.null(old_base_sb_backtest_results_list)){
    ###Recreate
    base_sb_configs_list <- purrr::map(old_base_sb_backtest_results_list, function(old_backtest){
      old_sb_config                      <- old_backtest@sb_backtest_config 
      old_sb_config@training_sample_size <- old_backtest@sb_backtest_workflow[[1]]$training_sample_size
      
      old_sb_config
    })
    
    ###Rename
    names(base_sb_configs_list) <- purrr::map_chr(old_base_sb_backtest_results_list, function(old_backtest){
      paste0(old_backtest@sb_backtest_config@config_name, "_model_config")
    }) %>% unname()
    
    return(base_sb_configs_list)
    
  }
  
  ##EW Configuration------------------------------------------------------------
  ew_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "ew",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "ew"
  )
  
  ##SW Configurations----------------------------------------------------------- 
  ###Historical Performance
  sw_lt_perf_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "sw",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "sw_lt_perf", custom_objective = "max_lt_perf_agg_z_score"
  )
  ###Mom
  sw_mom_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "sw",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "sw_mom", custom_objective = "max_mom_agg_z_score"
  )
  ###Val
  sw_val_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "sw",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "sw_val", custom_objective = "max_val_agg_z_score"
  )
  ###Overall
  sw_overall_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "sw",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "sw_overall", custom_objective = "max_overall_agg_z_score"
  )
  
  ##RP Configuration-------------------------------------------------------------
  rp_cc_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "rp",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size), 
    rebalancing_months = rebal_months_heuristics,
    config_name = "rp_cc"
  ) %>% 
    factoRverse::add_cov_est_method(
      cov_estimation_method = "shrink_cc", cov_matrix_sample_size = training_sample_size, 
      active_returns = TRUE, cov_matrix_benchmark = "ibov"
    ) %>% 
    factoRverse::add_rp_parameters(
      rp_method = "cyclical-spinu"
    )
  
  ##HRP Configuration-----------------------------------------------------------
  hrp_cc_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "hrp",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "hrp_cc"
  ) %>% 
    factoRverse::add_cov_est_method(
      cov_estimation_method = "shrink_cc", cov_matrix_sample_size = training_sample_size, 
      active_returns = TRUE, cov_matrix_benchmark = "ibov"
    ) 
  
  ##MVO Configuration-----------------------------------------------------------
  mvo_overall_cc_con_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "mvo",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "mvo_overall_cc_con", custom_objective = "max_overall_agg_z_score"
  ) %>% 
    factoRverse::add_cov_est_method(
      cov_estimation_method = "shrink_cc", cov_matrix_sample_size = training_sample_size, 
      active_returns = TRUE, cov_matrix_benchmark = "ibov"
    ) %>% 
    factoRverse::add_concentration_constraint_policy(
      benchmark = "theme_sb", max_abs_active_group_weight = c(theme = 0.50)
    ) %>%
    factoRverse::add_mvo_parameters(
      n_random_ports = 1000, opt_objective = "sharpe", n_resamples = 50,
      exp_ret_score_jitter = 0.50, cov_eigval_jitter = 0.25
    )
  
  ##MMAF Configuration-----------------------------------------------------------
  ###Top-Down SW with overall metric + HRP
  mmaf_overall_cc_sw_hrp_top_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "mmaf",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "mmaf_overall_cc_sw_hrp_top",
    custom_objective = "max_overall_agg_z_score"
  ) %>% 
    factoRverse::add_cov_est_method(
      cov_estimation_method = "shrink_cc", cov_matrix_sample_size = training_sample_size, 
      active_returns = TRUE, cov_matrix_benchmark = "ibov"
    ) %>%
    factoRverse::add_mmaf_parameters(
      mmaf_group_col = "theme", mmaf_method = "top_down", 
      macro_port_construction_method = "hrp", 
      micro_port_construction_method = "sw",
      top_down_proxy_port_method =  "sw"
    ) %>%
    factoRverse::add_hrp_parameters(
      linkage = "single", exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL,
      level = "macro"
    )
  
  ###Bottom-up HRP + SW with overall metric
  mmaf_overall_cc_hrp_sw_bottom_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "mmaf",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "mmaf_overall_cc_hrp_sw_bottom",
    custom_objective = "max_overall_agg_z_score"
  ) %>% 
    factoRverse::add_cov_est_method(
      cov_estimation_method = "shrink_cc", cov_matrix_sample_size = training_sample_size, 
      active_returns = TRUE, cov_matrix_benchmark = "ibov"
    ) %>%
    factoRverse::add_mmaf_parameters(
      mmaf_group_col = "theme", mmaf_method = "bottom_up", 
      macro_port_construction_method = "sw",
      micro_port_construction_method = "hrp"
    ) %>%
    factoRverse::add_hrp_parameters(
      linkage = "single", exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL,
      level = "micro"
    )
  
  ###Top-down HRP + HRP 
  mmaf_cc_hrp_hrp_top_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "mmaf",  target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_heuristics,
    config_name = "mmaf_cc_hrp_hrp_top"
  ) %>% 
    factoRverse::add_cov_est_method(
      cov_estimation_method = "shrink_cc", cov_matrix_sample_size = training_sample_size, 
      active_returns = TRUE, cov_matrix_benchmark = "ibov"
    ) %>%
    factoRverse::add_mmaf_parameters(
      mmaf_group_col = "theme", mmaf_method = "top_down", 
      macro_port_construction_method = "hrp",
      micro_port_construction_method = "hrp",
      top_down_proxy_port_method =  "hrp"
    ) %>%
    factoRverse::add_hrp_parameters(
      linkage = "single", exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL,
      level = "micro"
    ) %>%
    factoRverse::add_hrp_parameters(
      linkage = "single", exp_ret_score_tilt = "none", exp_ret_score_tilt_eta = NULL,
      level = "macro"
    )
  
  
  ##OLS Configuration-----------------------------------------------------------
  ols_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "ols", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = (training_sample_size + validation_sample_size),
    rebalancing_months = rebal_months_ml,
    config_name = "ols")
  
  ##GLMNET Configuration--------------------------------------------------------
  ###Grid
  glmnet_grid_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "glmnet", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, 
    rebalancing_months = rebal_months_ml,
    config_name = "glmnet_grid") %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "grid_search", 
      validation_sample_size = validation_sample_size,
      chosen_eval_metric = "rmse"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "alpha",
      grid = c(0.25, 0.5, 1)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "lambda.min.ratio",
      grid = seq(0.1, 0.7, by = 0.1)
    ) 
  
  ###Bayes Opt
  glmnet_bayes_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "glmnet", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, 
    rebalancing_months = rebal_months_ml,
    config_name = "glmnet_bayes") %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "bayesian_opt",
      validation_sample_size = validation_sample_size,
      chosen_eval_metric = "rmse", n_iter = 25, init_points = 8,
      k_iter = 8, acq = "ucb"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = c("alpha", "lambda.min.ratio"),
      bounds = list(c(0,1), c(0.0001, 0.5))
    ) 
  
  ##RF Configuration------------------------------------------------------------
  ###Grid
  rf_grid_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "rf", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size,
    rebalancing_months = rebal_months_ml,
    config_name = "rf_grid") %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "grid_search", 
      validation_sample_size = validation_sample_size,
      chosen_eval_metric = "rmse"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "mtry",
      grid = seq(0.25, 1, by = 0.25)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "num.trees",
      grid = c(500)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "max.depth",
      grid = c(2, 4, 8, 10)
    )  %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "min.bucket",
      grid = c(1, 5, 10)
    ) 
  
  ###Bayes Opt
  rf_bayes_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "rf", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, 
    rebalancing_months = rebal_months_ml,
    config_name = "rf_bayes") %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "bayesian_opt", validation_sample_size = validation_sample_size,
      chosen_eval_metric = "rmse", n_iter = 50, init_points = 12,
      k_iter = 4, acq = "ucb"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
      bounds = list(c(0.1,1), c(200L, 800L), c(2L, 10L), c(1,10))
    ) 
  
  ##XGB Configuration-----------------------------------------------------------
  ###Grid
  xgb_grid_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "xgb", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size,
    rebalancing_months = rebal_months_ml,
    config_name = "xgb_grid") %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "grid_search",
      validation_sample_size = validation_sample_size,
      chosen_eval_metric = "rmse", early_stop = 25
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "min_child_weight",
      grid = c(1, 5)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "max_depth",
      grid = c(3, 5, 8)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "subsample",
      grid = c(0.8)
    )  %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "colsample_bytree",
      grid = c(0.50, 0.75)
    )  %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "eta",
      grid = c(0.001, 0.01, 0.05)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "alpha",
      grid = c(0, 5)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "gamma",
      grid = c(0)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "nrounds",
      grid = c(500)
    )
  
  ###Bayes Opt
  xgb_bayes_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "xgb", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, 
    rebalancing_months = rebal_months_ml,
    config_name = "xgb_bayes") %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "bayesian_opt",
      validation_sample_size = validation_sample_size,
      chosen_eval_metric = "rmse", early_stop = 25,
      n_iter = 60, k_iter = 3, init_points = 15, acq = "ucb"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = c("min_child_weight", "max_depth", "subsample",
                         "colsample_bytree", "eta", "alpha", "gamma",
                         "nrounds"),
      bounds = list(c(1L,6L), c(2L, 8L), c(0.25, 1),
                    c(0.25, 1), c(0.005, 0.10), c(0,5), c(0, 5),
                    c(200L, 1000L))
    )
  
  ##NN1 Configuration-----------------------------------------------------------
  ###Grid
  nn1_grid_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "nn", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, rebalancing_months = rebal_months_ml,
    config_name = "nn1_grid") %>% 
    factoRverse::add_keras_architecture(
      nn_optimizer = "Adam", units = 32,
      activation = "relu", batch_norm_option = TRUE
    ) %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "grid_search", chosen_eval_metric = "rmse",
      validation_sample_size = validation_sample_size, early_stop = 15
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "regularizer_l1",
      grid = 10^-c(2, 3, 5)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "regularizer_l2",
      grid = c(0, 1e-4)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "droprate",
      grid = c(0.25, 0.50)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "lr",
      grid = 10^-seq(2, 4, length = 3)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "size_of_batch",
      grid = 256
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "number_of_epochs",
      grid = 100
    )
  
  ###Bayes
  nn1_bayes_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "nn", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, 
    rebalancing_months = rebal_months_ml,
    config_name = "nn1_bayes") %>% 
    factoRverse::add_keras_architecture(
      nn_optimizer = "Adam", units = 32,
      activation = "relu", batch_norm_option = TRUE
    ) %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "bayesian_opt", chosen_eval_metric = "rmse",
      validation_sample_size = validation_sample_size, early_stop = 15, 
      n_iter = 65, k_iter = 1, init_points = 12, acq = "ucb"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = c("regularizer_l1", "regularizer_l2", "droprate",
                         "lr", "size_of_batch", "number_of_epochs"),
      bounds = list(c(1e-05, 1e-02), c(1e-05, 1e-03), c(0.25, 0.75),
                    c(1e-04, 1e-02), c(128L, 512L), c(60L, 110L))
    ) 
  
  
  ##NN2-------------------------------------------------------------------------
  ###Grid
  nn2_grid_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "nn", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size,
    rebalancing_months = rebal_months_ml,
    config_name = "nn2_grid") %>% 
    factoRverse::add_keras_architecture(
      nn_optimizer = "Adam", units = c(32, 16),
      activation = rep("relu", 2), batch_norm_option = rep(TRUE, 2)
    ) %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "grid_search", chosen_eval_metric = "rmse",
      validation_sample_size = validation_sample_size, early_stop = 15
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "regularizer_l1",
      grid = 10^-c(2, 3, 5)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "regularizer_l2",
      grid = c(0, 1e-4)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "droprate",
      grid = c(0.25, 0.50)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "lr",
      grid = 10^-seq(2, 4, length = 3)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "size_of_batch",
      grid = 256
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "number_of_epochs",
      grid = 100
    )
  
  ###Bayes
  nn2_bayes_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "nn", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size,
    rebalancing_months = rebal_months_ml,
    config_name = "nn2_bayes") %>% 
    factoRverse::add_keras_architecture(
      nn_optimizer = "Adam", units = c(32, 16),
      activation = rep("relu", 2), batch_norm_option = rep(TRUE, 2)
    ) %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "bayesian_opt", chosen_eval_metric = "rmse",
      validation_sample_size = validation_sample_size, early_stop = 15, 
      n_iter = 70, k_iter = 1, init_points = 12, acq = "ucb"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = c("regularizer_l1", "regularizer_l2", "droprate",
                         "lr", "size_of_batch", "number_of_epochs"),
      bounds = list(c(1e-05, 1e-02), c(1e-05, 1e-03), c(0.25, 0.75),
                    c(1e-04, 1e-02), c(128L, 512L), c(60L, 110L))
    ) 
  
  ##NN3-------------------------------------------------------------------------
  ###Grid
  nn3_grid_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "nn", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size, 
    rebalancing_months = rebal_months_ml,
    config_name = "nn3_grid") %>% 
    factoRverse::add_keras_architecture(
      nn_optimizer = "Adam", units = c(32, 16, 8),
      activation = rep("relu", 3), batch_norm_option = rep(TRUE, 3)
    ) %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "grid_search", chosen_eval_metric = "rmse",
      validation_sample_size = validation_sample_size, early_stop = 15
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "regularizer_l1",
      grid = 10^-c(2, 3, 5)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "regularizer_l2",
      grid = c(0, 1e-4)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "droprate",
      grid = c(0.25, 0.50)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "lr",
      grid = 10^-seq(2, 4, length = 3)
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "size_of_batch",
      grid = 256
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = "number_of_epochs",
      grid = 100
    )
  
  ###Bayes
  nn3_bayes_model_config <- factoRverse::create_sb_backtest_config(
    sb_algorithm = "nn", target_fwd_name = "fwd_act_geom_mean_ret_3m",
    training_sample_size = training_sample_size,
    rebalancing_months = rebal_months_ml,
    config_name = "nn3_bayes") %>% 
    factoRverse::add_keras_architecture(
      nn_optimizer = "Adam", units = c(32, 16, 8),
      activation = rep("relu", 3), batch_norm_option = rep(TRUE, 3)
    ) %>% 
    factoRverse::add_tuning_strategy(
      tuning_method = "bayesian_opt", chosen_eval_metric = "rmse",
      validation_sample_size = validation_sample_size, early_stop = 15, 
      n_iter = 75, k_iter = 1, init_points = 12, acq = "ucb"
    ) %>% 
    factoRverse::add_hyperparameter(
      hyperparameter = c("regularizer_l1", "regularizer_l2", "droprate",
                         "lr", "size_of_batch", "number_of_epochs"),
      bounds = list(c(1e-05, 1e-02), c(1e-05, 1e-03), c(0.25, 0.75),
                    c(1e-04, 1e-02), c(128L, 512L), c(60L, 110L))
    )
  
  #Return----------------------------------------------------------------------    
  return(
    list(
      ew_model_config = ew_model_config,
      sw_lt_perf_model_config = sw_lt_perf_model_config,
      sw_mom_model_config = sw_mom_model_config,
      sw_val_model_config = sw_val_model_config,
      sw_overall_model_config = sw_overall_model_config,
      rp_cc_model_config = rp_cc_model_config,
      hrp_cc_model_config = hrp_cc_model_config,
      mvo_overall_cc_con_model_config = mvo_overall_cc_con_model_config,
      mmaf_overall_cc_sw_hrp_top_model_config = mmaf_overall_cc_sw_hrp_top_model_config,
      mmaf_overall_cc_hrp_sw_bottom_model_config = mmaf_overall_cc_hrp_sw_bottom_model_config,
      mmaf_cc_hrp_hrp_top_model_config = mmaf_cc_hrp_hrp_top_model_config,
      ols_model_config = ols_model_config,
      glmnet_grid_model_config = glmnet_grid_model_config,
      glmnet_bayes_model_config = glmnet_bayes_model_config,
      rf_grid_model_config = rf_grid_model_config,
      rf_bayes_model_config = rf_bayes_model_config,
      xgb_grid_model_config = xgb_grid_model_config,
      xgb_bayes_model_config = xgb_bayes_model_config,
      nn1_grid_model_config = nn1_grid_model_config,
      nn1_bayes_model_config = nn1_bayes_model_config,
      nn2_grid_model_config = nn2_grid_model_config,
      nn2_bayes_model_config = nn2_bayes_model_config,
      nn3_grid_model_config = nn3_grid_model_config,
      nn3_bayes_model_config = nn3_bayes_model_config
    )
  )
}

#' Run signal blending backtests with optional parallelization
#'
#' Executes all selected signal blending backtests using predefined model
#' configurations. Supports heuristic and ML-based models, optional outer or
#' inner parallel execution, and checkpointing to allow safe resumption after
#' interruptions.
#'
#' @param groups Named list containing grouped backtest inputs (signals,
#'   auxiliary tables, returns, themes, and prior SS backtest results).
#' @param gold_aux_tables Auxiliary data tables required for backtesting.
#' @param ret_indexes_m_xts Benchmark return indexes used in the backtests.
#' @param base_sb_configs_list List containing all SB model
#'   configuration objects.
#' @param chosen_sb_configs Character vector indicating which SB configurations
#'   should be executed.
#'
#' @return Named list of signal blending backtest result objects.
run_all_sb_backtests <- function(groups, gold_aux_tables, ret_indexes_m_xts,
                                 base_sb_configs_list, chosen_sb_configs,
                                 old_base_sb_backtest_results_list = NULL,
                                 batches_dates = NULL, source,
                                 parallel){
  
  ## Initial setup---------------------------------------------------------------
  message("\nSetup for SB backtests...\n\n")
  ### Checkpoint Setup
  checkpoint <- save_checkpoint(
    path = "backtest_base_signal_blending_checkpoint", 
    current_date = groups[[1]]$gold_m_df@current_date
  )
  last_steps_done <- checkpoint$last_steps_done
  checkpoint_file <- checkpoint$checkpoint_file
  #This chunk is not needed, because loading and reloading occurs in SB blocks
  #if (!is.null(checkpoint$data)){
  #  message("Resuming from checkpoint. Last steps done: ", 
  #          paste(last_steps_done, collapse = ", "))
  #  base_sb_backtest_results_lists <- checkpoint$data #Adjust!!
  #} else {
  #  base_sb_backtest_results_lists <- list()
  #}
  
  
  
  ## Define model registry ----------------------------------------------------
  message("\nDefining SB model registry...\n\n")  
  sb_registry <- list(
    
    ## Heuristic models
    list(name="ew", config="ew_model_config",
         heuristic=TRUE, metric=NULL,
         sectors_treatment="exclude",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="sw_val", config="sw_val_model_config",
         heuristic=TRUE, metric="val_agg_z_score",
         sectors_treatment="exclude",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="sw_mom", config="sw_mom_model_config",
         heuristic=TRUE, metric="mom_agg_z_score",
         sectors_treatment="exclude",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="sw_lt_perf", config="sw_lt_perf_model_config",
         heuristic=TRUE, metric="lt_perf_agg_z_score",
         sectors_treatment="exclude",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="sw_overall", config="sw_overall_model_config",
         heuristic=TRUE, metric="overall_agg_z_score",
         sectors_treatment="exclude",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="rp_cc", config="rp_cc_model_config",
         heuristic=TRUE, metric=NULL, 
         sectors_treatment="reclassify",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="hrp_cc", config="hrp_cc_model_config",
         heuristic=TRUE, metric=NULL, 
         sectors_treatment="reclassify",
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="mmaf_overall_cc_sw_hrp_top",
         config="mmaf_overall_cc_sw_hrp_top_model_config",
         sectors_treatment="reclassify",
         heuristic=TRUE, metric="overall_agg_z_score",
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="mmaf_overall_cc_hrp_sw_bottom",
         config="mmaf_overall_cc_hrp_sw_bottom_model_config",
         sectors_treatment="reclassify",
         heuristic=TRUE, metric="overall_agg_z_score",
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="mmaf_cc_hrp_hrp_top",
         config="mmaf_cc_hrp_hrp_top_model_config",
         sectors_treatment="reclassify",
         heuristic=TRUE, metric=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="mvo_overall_cc_con",
         config="mvo_overall_cc_con_model_config",
         sectors_treatment="reclassify",
         heuristic=TRUE, metric="overall_agg_z_score",
         outer_parallel=parallel, inner_parallel=FALSE),     ### MVO special case
    
    ## ML models
    list(name="ols", config="ols_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="glmnet_bayes", config="glmnet_bayes_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="glmnet_grid", config="glmnet_grid_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="rf_bayes", config="rf_bayes_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="rf_grid", config="rf_grid_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="xgb_bayes", config="xgb_bayes_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    list(name="xgb_grid", config="xgb_grid_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=parallel),
    
    ### NNs - special case: sequential 
    list(name="nn1_bayes", config="nn1_bayes_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="nn1_grid", config="nn1_grid_model_config",
         heuristic=FALSE,
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="nn2_bayes", config="nn2_bayes_model_config",
         heuristic=FALSE, 
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="nn2_grid", config="nn2_grid_model_config",
         heuristic=FALSE,
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="nn3_bayes", config="nn3_bayes_model_config",
         heuristic=FALSE,
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=FALSE),
    
    list(name="nn3_grid", config="nn3_grid_model_config",
         heuristic=FALSE,
         sectors_treatment=NULL,
         outer_parallel=FALSE, inner_parallel=FALSE)
  )
  ### Validate registry
  for (spec in sb_registry) {
    
    outer_parallel <- isTRUE(spec$outer_parallel)
    inner_parallel <- isTRUE(spec$inner_parallel)
    
    if (outer_parallel && inner_parallel) {
      stop(
        paste0(
          "Invalid SB registry configuration for model '",
          spec$name,
          "': both outer_parallel and inner_parallel are TRUE. ",
          "Nested parallelism is not allowed."
        )
      )
    }
  }
  
  
  ## Run-------------------------------------------------------------------------
  for (spec in sb_registry){
    
    model_name <- spec$name
    
    tictoc::tic(paste0("\nRunning ", model_name, " backtest\n\n"))
    
    ## Guard: skip block if already done (useful when re-running the function after fixing issues or when running only a subset of models)  
    if (!skip_block(model_name, last_steps_done = last_steps_done) &&
        model_name %in% chosen_sb_configs){
      
      ### Extract configuration object
      config_obj <- base_sb_configs_list[[spec$config]]
      if (is.null(config_obj)){
        stop("Missing SB config: ", spec$config)
      }
      ### Extract old_sb_backtest_results for updates
      if (!is.null(old_base_sb_backtest_results_list)){
        
        #### Check that names contemplate only ind_signals or clusters, never both
         if (any(grepl("ind_signals", names(old_base_sb_backtest_results_list))) &
             any(grepl("clusters", names(old_base_sb_backtest_results_list)))){
           stop("Old backtest results list contains both 'ind_signals' and 'clusters' in names.")
         }
        
        #### Get position
        old_results_pos <- which(
          purrr::map_chr(old_base_sb_backtest_results_list, function(backtest){
            backtest@sb_backtest_config@config_name
          }) == config_obj@config_name
        )
        old_results <- old_base_sb_backtest_results_list[[old_results_pos]]
        if (is.null(old_results)){
          stop("No old backtest results found for config: ", config_obj@config_name)
        }
        
        ###Unserialize old results
        model_obj <- old_results@final_sb_model@model
        sb_algorithm <- old_results@sb_backtest_config@sb_algorithm
        if (sb_algorithm == "xgb" && is.raw(model_obj)) {
          message("Unserializing old XGB model for config: ", config_obj@config_name)
          old_results@final_sb_model@model <- xgboost::xgb.load.raw(model_obj)
          
        } else if (sb_algorithm == "nn" && methods::is(model_obj, "keras_ensemble")) {
          ###Mirror of the keras_ensemble branch in the serialization block below.
          ###Members are unserialized individually; the guard tolerates a member
          ###that is already a live model so an in-session object is a no-op.
          message("Unserializing old Keras ensemble (", length(model_obj@members),
                  " members) for config: ", config_obj@config_name)
          model_obj@members <- lapply(model_obj@members, function(member) {
            if (is.raw(member)) {
              keras::unserialize_model(model = member, compile = TRUE)
            } else {
              member
            }
          })
          old_results@final_sb_model@model <- model_obj

        } else if (sb_algorithm == "nn" && is.raw(model_obj)) {
          message("Unserializing old Keras model for config: ", config_obj@config_name)
          old_results@final_sb_model@model <- keras::unserialize_model(
            model   =  model_obj,
            compile = TRUE
          )

        }
      }
      
      
      ### Determine parallelism strategy
      outer_parallel <- isTRUE(spec$outer_parallel)
      #### In a batch update, we train only one dataset, thus outer_parallel must be always false
      if (!is.null(old_base_sb_backtest_results_list)){
        outer_parallel <- FALSE
      }
      inner_parallel <- isTRUE(spec$inner_parallel)
      old_max  <- getOption("future.globals.maxSize")
      old_plan <- future::plan()
      
      #### Set backend logic
      if (outer_parallel || inner_parallel){
        
        #### Workers
        workers <- if (outer_parallel){
          max(1, min(2, future::availableCores() - 1L))
        } else {
          max(1, min(6, future::availableCores() - 1L))
        }
        
        #### Globals size
        maxsize_gb <- as.numeric(Sys.getenv("FUTURE_GLOBALS_MAXSIZE_GB", 12))
        options(future.globals.maxSize = maxsize_gb * 1024^3)
        
        ####Plan
        future::plan(future::multisession, workers=workers)
        
        #### Needed for foreach-based tuning inside ML models
        if (inner_parallel){
          doFuture::registerDoFuture()
        }
      } else {
        future::plan(future::sequential)
      }
      
      ### Main logic
      if (outer_parallel){
        #### Run using furrr
        results_list <- furrr::future_imap(
          groups, 
          function(g, g_name){
            
            ###If update, error
            if (!is.null(old_base_sb_backtest_results_list)){
              stop("Outer parallelism is not allowed in batch updates.")
            }
            
            message(paste0(
              "\n===== Group '", g_name,
              "' started at ", Sys.time(),
              " | PID: ", Sys.getpid(), " =====\n\n"
            ))
            
            #### Heuristic eligibility adjustment
            if (isTRUE(spec$heuristic)){
              g <- adjust_signal_universe(g, metric_col     = spec$metric,
                                          sectors_treatment = spec$sectors_treatment)
            }
            
            #### Run backtest
            res <- run_logged(
              fun = function(){
                
                #### Seeds
                set.seed(123)
                tensorflow::tf$random$set_seed(123)
                
                run_one_sb_backtest(
                  sb_config              = config_obj,
                  gold_m_df              = g$gold_m_df,
                  gold_aux_tables        = gold_aux_tables,
                  backtest_returns_m_xts = g$backtest_returns_m_xts,
                  benchmark_returns_m_xts= ret_indexes_m_xts,
                  ss_backtest_results    = g$ss_backtest_results,
                  signal_themes_m_df     = g$signal_themes_m_df,
                  inner_parallel         = FALSE,
                  verbose                = TRUE
                )
              },
              log_file = paste0(config_obj@config_name, "_",Sys.getpid(), "_", 
                                format(Sys.time(), "%Y%m%d_%H%M%S")
              ),
              log_path=file.path(here::here(), "logs", "preprocessing")
            )
            
            cat(paste0(
              "\n===== Group '", g_name,
              "' completed at ", Sys.time(),
              " | PID: ", Sys.getpid(), " =====\n"
            ))
            
            res
          },
          .options = furrr::furrr_options(
            seed = 1234,
            stdout = TRUE,
            conditions = "condition"
          ),
          .progress = FALSE
        )
        
      } else {
        results_list <- purrr::imap(
          groups,
          function(g, g_name){
            
            message(paste0(
              "\n===== Group '", g_name,
              "' started at ", Sys.time(),
              " | PID: ", Sys.getpid(), " =====\n\n"
            ))
            
            #### Heuristic eligibility adjustment
            if (isTRUE(spec$heuristic)){
              g <- adjust_signal_universe(g, metric_col     = spec$metric,
                                          sectors_treatment = spec$sectors_treatment)
            }
            
            #### Run backtest
            if (is.null(old_base_sb_backtest_results_list)){
              ##### For first backtests
              res <- run_logged(
                fun = function(){
                  #### Seeds
                  set.seed(123)
                  tensorflow::tf$random$set_seed(123)
                  
                  run_one_sb_backtest(
                    sb_config              = config_obj,
                    gold_m_df              = g$gold_m_df,
                    gold_aux_tables        = gold_aux_tables,
                    backtest_returns_m_xts = g$backtest_returns_m_xts,
                    benchmark_returns_m_xts= ret_indexes_m_xts,
                    ss_backtest_results    = g$ss_backtest_results,
                    signal_themes_m_df     = g$signal_themes_m_df,
                    inner_parallel         = inner_parallel,
                    verbose                = TRUE
                  )
                },
                log_file = paste0(
                  config_obj@config_name, "_",
                  Sys.getpid(), "_",
                  format(Sys.time(), "%Y%m%d_%H%M%S")
                ),
                log_path = file.path(here::here(), "logs", "preprocessing")
              )
            } else {
              ##### For updates
              res <- run_logged(
                fun = function(){
                  #### Seeds
                  set.seed(123)
                  tensorflow::tf$random$set_seed(123)
                  
                  update_one_sb_backtest(
                    old_results            = old_results,
                    batches_dates          = batches_dates,
                    gold_m_df              = g$gold_m_df,
                    gold_aux_tables        = gold_aux_tables,
                    backtest_returns_m_xts = g$backtest_returns_m_xts,
                    benchmark_returns_m_xts= ret_indexes_m_xts,
                    ss_backtest_results    = g$ss_backtest_results,
                    signal_themes_m_df     = g$signal_themes_m_df,
                    inner_parallel         = inner_parallel,
                    verbose                = TRUE
                  )
                },
                log_file = paste0(
                  config_obj@config_name, "_",
                  Sys.getpid(), "_",
                  format(Sys.time(), "%Y%m%d_%H%M%S")
                ),
                log_path = file.path(here::here(), "logs", "preprocessing")
              )
              
            }
            
            
            cat(paste0(
              "\n===== Group '", g_name,
              "' completed at ", Sys.time(),
              " | PID: ", Sys.getpid(), " =====\n"
            ))
            
            res
          }
        )
      }
      
      ### Store versions of the tables used in the training and persist models
      for (dataset in seq_len(length(results_list))){
        #### Get date version
        current_date            <- groups[[1]]$gold_m_df@current_date
        
        #### Update workflow with versions
        sb_backtest_workflow <- results_list[[dataset]]@sb_backtest_workflow
        sb_backtest_workflow <- update_workflow_with_versions(
          workflow    = sb_backtest_workflow,
          object_name = "mb_backtest_results_bundles",
          current_date= current_date,
          source      = source
        )

        ### Get it back
        results_list[[dataset]]@sb_backtest_workflow <- sb_backtest_workflow
        
        ###If sb_algorithm is 'nn' or 'xgb', serialize
        if (results_list[[dataset]]@sb_backtest_config@sb_algorithm == "xgb"){
          message("Serializing XGB model for dataset ", dataset)
          results_list[[dataset]]@final_sb_model@model <- xgboost::xgb.save.raw(
            results_list[[dataset]]@final_sb_model@model
          )
        } else if (results_list[[dataset]]@sb_backtest_config@sb_algorithm == "nn"){
          nn_model <- results_list[[dataset]]@final_sb_model@model
          ###With n_ensembles > 1 the fitted model is a keras_ensemble holding one
          ###Keras model per member (factoRverse >= 0.7.0), not a bare model, and
          ###serialize_model() rejects it. Each member is serialized in place and
          ###the keras_ensemble wrapper is kept: that wrapper is what tells the
          ###reload path above to unserialize members rather than a single model,
          ###and it keeps the object's class stable across the round trip so its
          ###predict() method still averages the members afterwards.
          if (methods::is(nn_model, "keras_ensemble")) {
            message("Serializing Keras ensemble (", length(nn_model@members),
                    " members) for dataset ", dataset)
            nn_model@members <- lapply(nn_model@members, function(member) {
              keras::serialize_model(model = member, include_optimizer = TRUE)
            })
            results_list[[dataset]]@final_sb_model@model <- nn_model
          } else {
            message("Serializing Keras model for dataset ", dataset)
            results_list[[dataset]]@final_sb_model@model <- keras::serialize_model(
              model             = nn_model,
              include_optimizer = TRUE
            )
          }
        }
          
      } 
           
      #### Checkpoint
      if (length(last_steps_done) > 0){
        
        message("🔁 Reloading ", checkpoint_file)
        
        checkpoint <- qs::qread(checkpoint_file)
        base_sb_backtest_results_lists <- checkpoint$data
        rm(checkpoint)
        
      } else {
        base_sb_backtest_results_lists <- list()
      }
      
      base_sb_backtest_results_lists <-
        c(base_sb_backtest_results_lists,
          setNames(list(results_list),
                   paste0(model_name,"_backtest_results_list")))
      
      last_steps_done <- dplyr::union(last_steps_done, model_name)
      
      checkpoint <- list(
        steps = last_steps_done,
        data  = base_sb_backtest_results_lists
      )
      
      #### Save checkpoint atomically
      qs::qsave(checkpoint, paste0(checkpoint_file, ".tmp"))
      fs::file_move(paste0(checkpoint_file, ".tmp"), checkpoint_file)
        
      rm(base_sb_backtest_results_lists)
      gc()
      
      #### Restore backend
      future::plan(old_plan)
      options(future.globals.maxSize = old_max)
    }
    
    tictoc::toc()
  }
  
  ## Return---------------------------------------------------------------------  
  if (length(last_steps_done) > 0){
    ###Reload sb_backtest_results_list
    message("🔁  Reloading  ", checkpoint_file)
    checkpoint                     <- qs::qread(checkpoint_file)
    base_sb_backtest_results_lists <- checkpoint$data
    rm(checkpoint)
  }
  
  return(
    base_sb_backtest_results_lists
  )
  
}

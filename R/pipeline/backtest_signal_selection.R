# Copied from Multifatorial 3.0 pipeline, R/helpers/backtest_signal_selection.R
# at commit 8739bb0bce966dad8f7c701f0c7413a6fa25f61f (see R/pipeline/README.md).

#' Run signal selection (SS) backtests for frequentist and Bayesian configurations with checkpointing
#'
#' Executes a set of *signal selection* strategies (frequentist and/or Bayesian)
#' using precomputed portfolio backtest results as the return source. The
#' function builds `ss_backtest_config` objects, runs
#' `factoRverse::run_ss_backtest()` for each configuration (optionally in
#' parallel for frequentist models), and returns a consolidated list of
#' backtest results. A checkpointing mechanism enables resuming interrupted
#' executions without recomputation.
#'
#' @param gold_m_df A `meta_dataframe` of finalized (gold) signals used as
#'   predictors in the SS backtests.
#' @param gold_aux_tables A named list of auxiliary objects. Must include:
#'   - `ret_indexes_m_xts`: Benchmark returns (`meta_xts`).
#'   - `signal_themes_m_df`: Mapping of signals to themes (`meta_dataframe`).
#'   - `emerging_jkp_priors_m_df`, `world_jkp_priors_m_df`: Prior datasets for
#'     Bayesian specifications (both `meta_dataframe`).
#' @param signal_port_backtest_cohort A `port_backtest_cohort` object produced
#'   by `backtest_signal_portfolios()` providing per-signal portfolio returns.
#' @param chosen_signals_and_positions Named character vector mapping signal
#'   identifiers to positions (e.g., `"long"`, `"short"`). Passed into each
#'   `ss_backtest_config`.
#' @param chosen_ss_configs Character vector of configuration names to run. Names
#'   must match those internally constructed (see *Configurations* section).
#'   Both frequentist and Bayesian names may be mixed.
#' @param parallel Logical. If `TRUE`, frequentist configurations are evaluated
#'   in parallel via `future::multisession`; Bayesian configurations always run
#'   sequentially (for stability / resource control).
#'
#' @section Configurations:
#'   The function internally defines:
#'
#'   **Frequentist (prefix `freq_`):**
#'   - `freq_np_none`, `freq_np_bonf`, `freq_np_hommel`, `freq_np_BH`:
#'     *no pooled* models with different p-value adjustments.
#'   - `freq_pp_theme_int_fix_slope_none`, `freq_pp_theme_int_fix_slope_BH`:
#'     partial-pooled mixed-effects with theme-specific intercepts and fixed slope.
#'   - `freq_pp_theme_int_theme_slope_none`, `freq_pp_theme_int_theme_slope_BH`:
#'     partial-pooled mixed-effects with theme-specific intercepts and slopes.
#'
#'   **Bayesian (prefix `bayesian_`):**
#'   - `bayesian_pp_theme_int_fix_slope_shrk`: Shrinkage priors (user-specified).
#'   - `bayesian_pp_theme_int_fix_slope_EM`: Informative priors from emerging
#'     markets dataset.
#'   - `bayesian_pp_theme_int_fix_slope_WORLD`: Informative priors from world
#'     dataset.
#'
#'   Bayesian models use `cmdstanr` (via `options(brms.backend = "cmdstanr")`)
#'   and supplied `brms` control parameters. Frequentist partial-pooled models
#'   leverage `lme4` controls (e.g., optimizer, Satterthwaite df method).
#'
#' @section Priors Handling:
#'   A `priors_list` is constructed with elements `NULL` (frequentist),
#'   `emerging_jkp_priors_m_df`, and `world_jkp_priors_m_df`. Theme labels have
#'   underscores removed and corrected (e.g., `"invesment"` → `"investment"`).
#'   Shrinkage priors are added explicitly through
#'   `factoRverse::add_brms_prior()`.
#'
#' @section Workflow:
#'   1. **Checkpointing:** Loads prior progress from
#'      `checkpoints/backtest_signal_selection_checkpoint.rds` if available.
#'   2. **Extraction:** Calls `factoRverse:::extract_returns_m_xts()` to obtain
#'      signal portfolio returns aligned with benchmark returns.
#'   3. **Configuration Assembly:** Builds the complete set of frequentist and
#'      Bayesian `ss_backtest_config` objects, then filters them using
#'      `chosen_ss_configs`.
#'   4. **Frequentist Backtests:** Executed sequentially or in parallel. Each
#'      result is cleaned (removal of final call object) and annotated with the
#'      originating cohort name.
#'   5. **Bayesian Backtests:** Always sequential. Sets `cmdstanr` backend,
#'      runs each configuration with its matching priors, logs memory usage,
#'      and restores defaults afterward.
#'   6. **Aggregation:** Results are combined into a single list
#'      (`ss_backtest_results`) and checkpointed.
#'
#' @section Parallel Execution:
#'   When `parallel = TRUE`, frequentist runs use
#'   `future::plan(future::multisession, workers = min(length(freq_ss_configs),
#'   future::availableCores() - 1L))` and reproducible RNG
#'   (`furrr::furrr_options(seed = TRUE)`). Bayesian runs are *not* parallelized.
#'
#' @section Side Effects:
#'   Creates/updates a checkpoint file under `here::here("checkpoints")`. Logs
#'   progress, memory usage, and timing information to the console.
#'
#' @return A named list with elements:
#'   - `ss_backtest_results`: All executed SS backtests (frequentist and/or
#'     Bayesian).
#'   - `extracted_returns_m_xts`: The object returned by
#'     `factoRverse:::extract_returns_m_xts()`.
#'   - `chosen_freq_ss_configs`, `chosen_bayesian_ss_configs`: Lists of the
#'     selected configuration objects actually run.
#'
#' @seealso
#'   `factoRverse::run_ss_backtest()`,
#'   `factoRverse::create_ss_backtest_config()`,
#'   `backtest_signal_portfolios()`.
backtest_signal_selection <- function(gold_signals_m_df, gold_clusters_m_df,
                                      gold_aux_tables,
                                      signal_port_backtest_cohorts,
                                      old_winner_freq_ss_backtest_results_list = NULL,
                                      old_winner_bayesian_ss_backtest_results_list = NULL,
                                      batches_dates = NULL,
                                      initial_sample_size,
                                      chosen_signals_and_positions,
                                      chosen_ss_configs,
                                      source, parallel){

  #Initial Setup----------------------------------------------------------------
  current_date <- gold_signals_m_df@current_date
  ##Checkpoint Setup
  checkpoint <- save_checkpoint(
    path = "backtest_signal_selection_checkpoint", 
    current_date = current_date
  )
  last_steps_done <- checkpoint$last_steps_done
  checkpoint_file <- checkpoint$checkpoint_file
  if (!is.null(checkpoint$data)){
    message("Resuming from checkpoint. Last steps done: ", 
            paste(last_steps_done, collapse = ", "))
    ss_backtest_results_lists <- checkpoint$data #Adjust!!
  } else {
    ss_backtest_results_lists <- list()
  }
  
  #Extract objects--------------------------------------------------------------
  message("\nExtracting objects...\n\n")  
  
  ##Extract objs
    gold_dates      <- gold_signals_m_df@data$dates %>% unique() %>% sort() 
    gold_aux_tables <- prepare_gold_aux_to_backtest(gold_aux_tables, gold_dates)
  
    ###Gold Aux
    ret_indexes_m_xts      <- gold_aux_tables$ret_indexes_m_xts
    ret_indexes_m_xts      <- restore_meta_xts_dates(ret_indexes_m_xts)
    ret_indexes_m_xts@data <- ret_indexes_m_xts@data[, "ibov"] #Can't have NA 
    
    signal_themes_m_df       <- gold_aux_tables$signal_themes_m_df
    cluster_themes_m_df      <- gold_aux_tables$adj_cluster_themes_m_df
    emerging_jkp_priors_m_df <- gold_aux_tables$emerging_jkp_priors_m_df
    world_jkp_priors_m_df    <- gold_aux_tables$world_jkp_priors_m_df
    
    ###Extract backtest_returns_m_xts and benchmark_returns_m_xts
      ####Restore
      signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals@port_returns_m_xts_list$net_returns_m_xts <-
        restore_meta_xts_dates(signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals@port_returns_m_xts_list$net_returns_m_xts)
      signal_port_backtest_cohorts$signal_port_backtest_cohort_clusters@port_returns_m_xts_list$net_returns_m_xts <-
        restore_meta_xts_dates(signal_port_backtest_cohorts$signal_port_backtest_cohort_clusters@port_returns_m_xts_list$net_returns_m_xts)
  
      ####Extract returns
      extracted_returns_ind_signals_m_xts <- factoRverse:::extract_returns_m_xts(
        port_backtest_cohort = signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals,
        signals_m_df = gold_signals_m_df, benchmark_returns_m_xts = ret_indexes_m_xts,
        verbose = TRUE
      )
      extracted_returns_clusters_m_xts <- factoRverse:::extract_returns_m_xts(
        port_backtest_cohort = signal_port_backtest_cohorts$signal_port_backtest_cohort_clusters,
        signals_m_df = gold_clusters_m_df, benchmark_returns_m_xts = ret_indexes_m_xts,
        verbose = TRUE
      )
    ###Assign extracted values
    returns_ind_signals_m_xts     <- extracted_returns_ind_signals_m_xts$backtest_returns_m_xts
    port_cohort_ind_signals_name  <- signal_port_backtest_cohorts$signal_port_backtest_cohort_ind_signals@cohort_name
    returns_clusters_m_xts        <- extracted_returns_clusters_m_xts$backtest_returns_m_xts
    port_cohort_clusters_name     <- signal_port_backtest_cohorts$signal_port_backtest_cohort_clusters@cohort_name
    ret_indexes_m_xts             <- extracted_returns_ind_signals_m_xts$benchmark_returns_m_xts
    

  ##Create priors list
  priors_list <- list(NULL, emerging_jkp_priors_m_df, world_jkp_priors_m_df)
  names(priors_list) <- c("bayesian_pp_theme_int_theme_slope_shrk",
                          "bayesian_pp_theme_int_theme_slope_EM",
                          "bayesian_pp_theme_int_theme_slope_WORLD")
  
  ##Remove signal_port_backtest_cohort
  rm(signal_port_backtest_cohorts)
  gc()
  
  
  #Create Signal Selection Configs----------------------------------------------
  message("\nCreating/Retrieving SS backtest configurations...\n\n")
  
    ##Ind Signals
    ss_configs_ind_signals <- create_ss_backtest_configs(
        ###Creation route
        chosen_signals_and_positions            = chosen_signals_and_positions,
        chosen_ss_configs                       = chosen_ss_configs,
        initial_sample_size                     = initial_sample_size,
        ###Retrieval route
        old_winner_freq_ss_backtest_results     = old_winner_freq_ss_backtest_results_list$ind_signals,
        old_winner_bayesian_ss_backtest_results = old_winner_bayesian_ss_backtest_results_list$ind_signals,
        winner_ss_config                        = chosen_ss_configs
      )
    
    ##Clusters
      ###Get clusters
      chosen_clusters <- c(
        paste0(
          "fac_", 
          gold_aux_tables$signal_clusters_m_df@data %>% 
            dplyr::filter(!cluster %in% c("interactions", "industries")) %>% 
            dplyr::pull(cluster) %>% 
            unique()
          ),
        gold_aux_tables$signal_clusters_m_df@data %>% 
          dplyr::filter(cluster %in% c("interactions", "industries")) %>% 
          dplyr::pull(tickers) %>% 
          unique()
      )
      chosen_clusters_and_positions <- rep("long", length(chosen_clusters)) %>% 
        setNames(chosen_clusters)
      ###Make industries == 'force'
      industries <- gold_aux_tables$signal_clusters_m_df@data %>% 
        dplyr::filter(cluster == "industries") %>% 
        dplyr::pull(tickers) %>% 
        unique()
      chosen_clusters_and_positions[industries] <- "force"
      ###Define
      ss_configs_clusters <- create_ss_backtest_configs(
        ###Creation route
        chosen_signals_and_positions            = chosen_clusters_and_positions,
        chosen_ss_configs                       = chosen_ss_configs,
        initial_sample_size                     = initial_sample_size,
        ###Retrieval route
        old_winner_freq_ss_backtest_results     = old_winner_freq_ss_backtest_results_list$clusters,
        old_winner_bayesian_ss_backtest_results = old_winner_bayesian_ss_backtest_results_list$clusters,
        winner_ss_config                        = chosen_ss_configs
      )
      
 
  #Run Freq Backtests-----------------------------------------------------------
  tictoc::tic("\nRunning Frequentist SS backtests...\n\n")
      
    ##Extract
    chosen_freq_ss_configs_ind_signals <- ss_configs_ind_signals$chosen_freq_ss_configs
    chosen_freq_ss_configs_clusters    <- ss_configs_clusters$chosen_freq_ss_configs
  
    ##Check if length match
    if(length(chosen_freq_ss_configs_ind_signals) != length(chosen_freq_ss_configs_clusters)){
      stop("Length of chosen_freq_ss_configs_ind_signals and chosen_freq_ss_configs_clusters do not match!")
    }
    ##Check if names match
    if(!identical(names(chosen_freq_ss_configs_ind_signals), names(chosen_freq_ss_configs_clusters))){
      stop("Names of chosen_freq_ss_configs_ind_signals and chosen_freq_ss_configs_clusters do not match!")
    }
    
   
  if (!skip_block("run_freq_ss_backtest", last_steps_done = last_steps_done) && 
      length(chosen_freq_ss_configs_ind_signals) > 0){ 
    
    ##Tasks
    groups <- list(
      ind_signals = list(
        ss_configs                   = chosen_freq_ss_configs_ind_signals,
        gold_m_df                    = gold_signals_m_df,
        returns_m_xts                = returns_ind_signals_m_xts,
        port_cohort_name             = port_cohort_ind_signals_name,
        themes_m_df                  = signal_themes_m_df,
        old_ss_backtest_results      = old_winner_freq_ss_backtest_results_list$ind_signals
      ),
      clusters = list(
        ss_configs                   = chosen_freq_ss_configs_clusters,
        gold_m_df                    = gold_clusters_m_df,
        returns_m_xts                = returns_clusters_m_xts,
        port_cohort_name             = port_cohort_clusters_name,
        themes_m_df                  = cluster_themes_m_df,
        old_ss_backtest_results      = old_winner_freq_ss_backtest_results_list$clusters
      )
    )
    
    ##Run ALL port_backtests
    if (parallel){
      
      ###Temporarily override the plan
      old_plan <- future::plan()                            # store current
      on.exit(future::plan(old_plan), add = TRUE)           # restore on exit
      workers <- min(2, future::availableCores() - 1L)
      maxsize_gb <- as.numeric(Sys.getenv("FUTURE_GLOBALS_MAXSIZE_GB", 8))
      options(future.globals.maxSize = maxsize_gb * 1024^3)
      future::plan(future::multisession, workers = workers)
      
    } else {
      future::plan(future::sequential)
    }
      
      ###Run ALL port_backtests in parallel
      freq_ss_backtest_results_list_by_group <- furrr::future_map(
        groups,
        function(g) {
          
          message(paste0(
            "\n===== Group '", g$port_cohort_name, "' started at ", Sys.time(),
            " | PID: ", Sys.getpid(), " =====\n\n"
          ))
          
          ####Run
          # Map over configs within the group
          if (is.null(g$old_ss_backtest_results)){
            ###For first backtests
            res <- purrr::map(
              g$ss_configs,
              function(cfg) {
                # Per-config log file
                run_logged(
                  fun = function() {
                    run_one_freq_backtest(
                      config               = cfg,
                      gold_m_df            = g$gold_m_df,
                      returns_cohort_m_xts = g$returns_m_xts,
                      ret_indexes_m_xts    = ret_indexes_m_xts,
                      port_cohort_name     = g$port_cohort_name,
                      themes_m_df          = g$themes_m_df
                    )
                  },
                  log_file = paste0(cfg@config_name, "_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d_%H%M%S")),
                  log_path = file.path(here::here(), "logs", "preprocessing")
                )
              }
            )
          } else {
            ###For updates
            res <- purrr::map(
              list(g$old_ss_backtest_results),
              function(old_ss_backtest_results) {
                # Per-config log file
                run_logged(
                  fun = function() {
                    update_one_freq_backtest(
                      old_results          = old_ss_backtest_results,
                      batches_dates        = batches_dates,
                      gold_m_df            = g$gold_m_df,
                      returns_cohort_m_xts = g$returns_m_xts,
                      ret_indexes_m_xts    = ret_indexes_m_xts,
                      themes_m_df          = g$themes_m_df
                    )
                  },
                  log_file = paste0(old_ss_backtest_results@backtest_identifier, "_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d_%H%M%S")),
                  log_path = file.path(here::here(), "logs", "preprocessing")
                )
              }
            )
          }
      
          names(res) <- vapply(g$ss_configs, function(x) x@config_name, character(1))
          
          cat(paste0(
            "\n===== Group '", g$port_cohort_name, "' completed at ", Sys.time(),
            " | PID: ", Sys.getpid(), " =====\n"
          ))
          
          res
        },
        .options = furrr::furrr_options(
          seed = TRUE,
          stdout = structure(TRUE, drop = TRUE),
          conditions = structure("condition", drop = TRUE)
        ),
        .progress = FALSE
      )
      
    ##Combine
    freq_ss_backtest_results_lists <- list(
      ind_signals = freq_ss_backtest_results_list_by_group$ind_signals,
      clusters    = freq_ss_backtest_results_list_by_group$clusters
    )
    
    ##Loop through groups and adjust workflows
    for (ss_backtest_idx in seq_len(length(freq_ss_backtest_results_lists$ind_signals))){
      ss_backtest_workflow <- freq_ss_backtest_results_lists$ind_signals[[ss_backtest_idx]]@ss_backtest_workflow
      ss_backtest_workflow <- update_workflow_with_versions(
        workflow    = ss_backtest_workflow,
        object_name = "ss_backtest_results_bundles",
        current_date= current_date,
        source      = source
      )
      freq_ss_backtest_results_lists$ind_signals[[ss_backtest_idx]]@ss_backtest_workflow <- ss_backtest_workflow
    }
    for (ss_backtest_idx in seq_len(length(freq_ss_backtest_results_lists$clusters))){
      ss_backtest_workflow <- freq_ss_backtest_results_lists$clusters[[ss_backtest_idx]]@ss_backtest_workflow
      ss_backtest_workflow <- update_workflow_with_versions(
        workflow    = ss_backtest_workflow,
        object_name = "ss_backtest_results_bundles",
        current_date= current_date,
        source      = source
      )
      freq_ss_backtest_results_lists$clusters[[ss_backtest_idx]]@ss_backtest_workflow <- ss_backtest_workflow
    }
    
    ##Define bundle
    ss_backtest_results_lists$freq_ss_backtest_results_lists <- freq_ss_backtest_results_lists
    
    #Save checkpoint just before leaving the block
    last_steps_done <- dplyr::union(last_steps_done, "run_freq_ss_backtest")  
    checkpoint <- list(steps = last_steps_done, data = ss_backtest_results_lists)
    qs::qsave(checkpoint, checkpoint_file)
    
    #Delete object to save RAM
    rm(freq_ss_backtest_results_lists)
    gc()
    
  }  
  
  tictoc::toc() 
  
  #Run Bayesian Backtests-------------------------------------------------------
  tictoc::tic("\nRunning Bayesian SS backtests...\n\n")
  
    ##Extract
    chosen_bayesian_ss_configs_ind_signals <- ss_configs_ind_signals$chosen_bayesian_ss_configs
    chosen_bayesian_ss_configs_clusters    <- ss_configs_clusters$chosen_bayesian_ss_configs
    
    ##Check if length match
    if(length(chosen_bayesian_ss_configs_ind_signals) != length(chosen_bayesian_ss_configs_clusters)){
      stop("Length of chosen_bayesian_ss_configs_ind_signals and chosen_bayesian_ss_configs_clusters do not match!")
    }
    ##Check if names match
    if(!identical(names(chosen_bayesian_ss_configs_ind_signals), names(chosen_bayesian_ss_configs_clusters))){
      stop("Names of chosen_bayesian_ss_configs_ind_signals and chosen_bayesian_ss_configs_clusters do not match!")
    }
    
    ## Re-inforce priors alignment
    if (length(chosen_bayesian_ss_configs_ind_signals) > 0){
      chosen_names <- names(chosen_bayesian_ss_configs_ind_signals)
      priors_for_chosen <- priors_list[chosen_names]
      if (length(priors_for_chosen) != length(chosen_bayesian_ss_configs_ind_signals) ||
          any(is.na(names(priors_for_chosen)))) {
        stop("Could not align priors_list to chosen_bayesian_ss_configs by name. Check names().")
      }
      if (!identical(names(priors_for_chosen), names(chosen_bayesian_ss_configs_ind_signals))) {
        stop("Names of priors_for_chosen and chosen_bayesian_ss_configs_ind_signals do not match after alignment.")
      }
      if (!identical(names(priors_for_chosen), names(chosen_bayesian_ss_configs_clusters))) {
        stop("Names of priors_for_chosen and chosen_bayesian_ss_configs_clusters do not match after alignment.")
      }
    }

  if (!skip_block("run_bayesian_ss_backtest", last_steps_done = last_steps_done) &&
      length(chosen_bayesian_ss_configs_ind_signals) > 0){
    
      ##Run ALL port_backtests sequential
      cat(paste0("===== Run started at ", Sys.time(), " =====\n\n"))
        ###Setup Bayesian
        options(brms.backend = "cmdstanr")  
        cmdstanr::set_cmdstan_path("C:/Users/paulo.fonteles/.cmdstan/cmdstan-2.38.0")
        Sys.setenv(CMDSTAN = cmdstanr::cmdstan_path())
        
        on.exit({
          options(brms.backend = "rstan")
          Sys.unsetenv("CMDSTAN")
          options(mc.cores = NULL)
        }, add = TRUE)
      
      
      if (parallel){
        ## Parallelism can be achieved under many different ways.
        ## The first option is parallelize brms inside each backtest. 
        ## This can be achieved via parallel argument to brms or using use options(mc.cores = chains), allowing MCMC chain parallelism
        ##        One backtest took app 2h under options(mc.cores = chains) with 4 chains
        ## The second option is to parallelize each backtest run using future::plan(multisession)
        old_plan <- future::plan()
        on.exit(future::plan(old_plan), add = TRUE)
        
        ## Option 1: Parallel brms inside each backtest via brms argument
          chains <- chosen_bayesian_ss_configs_ind_signals[[1]]@alpha_test_strategy@bayesian_model_parameters@brms_control$chains
          future::plan(future::multisession, workers = chains) #Should match chains If parallel to brms is TRUE, uncomment
        
        ## Option 2: Parallel MCMC chains with mc.cores
          #options(mc.cores = chains)  # CmdStanR uses this for parallel_chains by default
        
        ## Option 3: Parallel across backtests
        #future::plan(future::multisession, workers = length(chosen_bayesian_ss_configs_ind_signals)) 
      }
      
 
        ### Build two groups
        groups <- list(
          ind_signals = list(
            ss_configs            = chosen_bayesian_ss_configs_ind_signals,
            gold_m_df             = gold_signals_m_df,
            returns_m_xts         = returns_ind_signals_m_xts,
            port_cohort_name      = port_cohort_ind_signals_name,
            themes_m_df           = signal_themes_m_df
          ),
          clusters = list(
            ss_configs            = chosen_bayesian_ss_configs_clusters,
            gold_m_df             = gold_clusters_m_df,
            returns_m_xts         = returns_clusters_m_xts,
            port_cohort_name      = port_cohort_clusters_name,
            themes_m_df           = cluster_themes_m_df
          )
        )
        
        ### Run: groups sequentially; configs sequentially; run_ss_backtest parallel internally
        cat(paste0("===== Bayesian run started at ", Sys.time(), " =====\n\n"))
      
        bayesian_ss_backtest_results_list_by_group <- purrr::imap(
          groups,
          function(g, group_name) {
            
            cat(paste0(
              "\n===== Group '", group_name, "' (", g$port_cohort_name, ") started at ",
              Sys.time(), " =====\n\n"
            ))
            
            priors_g <- priors_for_chosen[names(g$ss_configs)]
            
            ### Run sequentially
            res <- purrr::map2(
              g$ss_configs,
              priors_g,
              function(cfg, prior) {
                
                # Run with logging
                run_logged(
                  fun = function() {
                    run_one_bayesian_backtest(
                      config               = cfg,
                      prior_m_df           = prior,
                      gold_m_df            = g$gold_m_df,
                      returns_cohort_m_xts = g$returns_m_xts,
                      ret_indexes_m_xts    = ret_indexes_m_xts,
                      themes_m_df          = g$themes_m_df,
                      port_cohort_name     = g$port_cohort_name,
                      parallel             = parallel
                    )
                  },
                  log_file = paste0(cfg@config_name, Sys.getpid(), "_", format(Sys.time(), "%Y%m%d_%H%M%S")),
                  log_path = file.path(here::here(), "logs", "preprocessing")
                )
              },
              .options = furrr::furrr_options(
                seed       = TRUE,
                stdout     = TRUE,                   # relay cat()/print()
                conditions = c("message", "warning") # relay message()/warning()
              )
            )
          
            names(res) <- vapply(g$ss_configs, function(x) x@config_name, character(1))
            
            cat(paste0(
              "\n===== Group '", group_name, "' completed at ", Sys.time(), " =====\n"
            ))
            
            res
          }
        )
        
        cat(paste0("\n===== Bayesian run completed at ", Sys.time(), " =====\n"))
        
        ##Combine
        bayesian_ss_backtest_results_lists <- list(
          ind_signals = bayesian_ss_backtest_results_list_by_group$ind_signals,
          clusters    = bayesian_ss_backtest_results_list_by_group$clusters
        )
        
        ##Loop through groups and adjust workflows
        for (ss_backtest_idx in seq_len(length(bayesian_ss_backtest_results_lists$ind_signals))){
          ss_backtest_workflow <- bayesian_ss_backtest_results_lists$ind_signals[[ss_backtest_idx]]@ss_backtest_workflow
          ss_backtest_workflow <- update_workflow_with_versions(
            workflow    = ss_backtest_workflow,
            object_name = "ss_backtest_results_bundles",
            current_date= current_date,
            source      = source
          )
          bayesian_ss_backtest_results_lists$ind_signals[[ss_backtest_idx]]@ss_backtest_workflow <- ss_backtest_workflow
        }
        for (ss_backtest_idx in seq_len(length(bayesian_ss_backtest_results_lists$clusters))){
          ss_backtest_workflow <- bayesian_ss_backtest_results_lists$clusters[[ss_backtest_idx]]@ss_backtest_workflow
          ss_backtest_workflow <- update_workflow_with_versions(
            workflow    = ss_backtest_workflow,
            object_name = "ss_backtest_results_bundles",
            current_date= current_date,
            source      = source
          )
          bayesian_ss_backtest_results_lists$clusters[[ss_backtest_idx]]@ss_backtest_workflow <- ss_backtest_workflow
        }
        
        ##Define bundle
        ss_backtest_results_lists$bayesian_ss_backtest_results_lists <- bayesian_ss_backtest_results_lists


    #Save checkpoint just before leaving the block
    last_steps_done <- dplyr::union(last_steps_done, "run_bayesian_ss_backtest")  
    checkpoint <- list(steps = last_steps_done, data = ss_backtest_results_lists)
    qs::qsave(checkpoint, checkpoint_file)
    
    #Delete object to save RAM
    rm(bayesian_ss_backtest_results_lists)
    gc()
    
  }
  
  tictoc::toc() 
  
  #Proof of continuity----------------------------------------------------------
  if (!is.null(old_winner_freq_ss_backtest_results_list)){
    
    ### Proof of continuity needs to be based on frequentist call, since a bayesian call is way too expensive
    ### This is another reason to always keep a freq and a bayesian winner and an overall winner
    message("\nTaking proof of continuity for backtest results...")
    
    ##Take backtest for clusters
    if (length(chosen_freq_ss_configs_clusters) != 1){
      stop("Expected length of chosen_freq_ss_configs_clusters to be 1 for proof of continuity, but got length ",
           length(chosen_freq_ss_configs_clusters))
    }
    continuity_config_clusters                     <- chosen_freq_ss_configs_clusters[[1]]
    continuity_config_clusters@initial_sample_size <- old_winner_freq_ss_backtest_results_list$clusters@ss_backtest_workflow[[1]]$initial_sample_size
    
    ##Run one freq backtest
    winner_freq_ss_backtest_results_clusters_check <- run_one_freq_backtest(
      config               = continuity_config_clusters,
      gold_m_df            = groups$clusters$gold_m_df,
      returns_cohort_m_xts = groups$clusters$returns_m_xts,
      ret_indexes_m_xts    = ret_indexes_m_xts,
      port_cohort_name     = groups$clusters$port_cohort_name,
      themes_m_df          = groups$clusters$themes_m_df
    )
    
    proof_of_continuity <- list(
      winner_freq_ss_backtest_results_clusters_check = winner_freq_ss_backtest_results_clusters_check
    )
  }
  
  #Return-----------------------------------------------------------------------
  
  ##For testing purposes, return both ss_backtest_results_lists and extracted_returns_m_xts
  return(list(
    ss_backtest_results_lists              = ss_backtest_results_lists,
    returns_ind_signals_m_xts              = returns_ind_signals_m_xts,
    returns_clusters_m_xts                 = returns_clusters_m_xts,
    ret_indexes_m_xts                      = ret_indexes_m_xts,
    chosen_freq_ss_configs_ind_signals     = chosen_freq_ss_configs_ind_signals,
    chosen_freq_ss_configs_clusters        = chosen_freq_ss_configs_clusters,
    chosen_bayesian_ss_configs_ind_signals = chosen_bayesian_ss_configs_ind_signals,
    chosen_bayesian_ss_configs_clusters    = chosen_bayesian_ss_configs_clusters,
    proof_of_continuity                    = if (exists("proof_of_continuity")) proof_of_continuity else NULL
  ))
  
  
  
}

#' Create SS Backtest Configurations
#'
#' Builds predefined frequentist and Bayesian signal-selection backtest
#' configurations, filters them according to user selection, and returns
#' the chosen configuration objects ready for backtesting workflows.
#'
#' @param chosen_signals_and_positions Character vector (or compatible object)
#'   defining which signals and portfolio positions should be included in the
#'   signal-selection backtests.
#' @param chosen_ss_configs Character vector with the names of the desired
#'   configurations to retain from the predefined frequentist and Bayesian
#'   sets.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{chosen_freq_ss_configs}{List of selected frequentist SS backtest
#'   configuration objects.}
#'   \item{chosen_bayesian_ss_configs}{List of selected Bayesian SS backtest
#'   configuration objects.}
#' }
#'
#' @details The function defines several standard signal-selection backtest
#' configurations combining multiple-testing corrections, pooling structures,
#' and (optionally) Bayesian hierarchical priors. Only configurations whose
#' names match `chosen_ss_configs` are returned. Validation checks ensure that
#' at least one configuration is selected and that all requested names exist.
create_ss_backtest_configs <- function(chosen_signals_and_positions, chosen_ss_configs,
                                       initial_sample_size,
                                       old_winner_freq_ss_backtest_results, 
                                       old_winner_bayesian_ss_backtest_results,
                                       winner_ss_config){
  
  ##Retrieval route
  if (!is.null(old_winner_freq_ss_backtest_results) | !is.null(old_winner_bayesian_ss_backtest_results)){
    ###Retrieve route
    chosen_freq_ss_configs            <- list()
    names(chosen_freq_ss_configs)     <- character(0)
    chosen_bayesian_ss_configs        <- list()
    names(chosen_bayesian_ss_configs) <- character(0)
    ####Freq
    if (!is.null(old_winner_freq_ss_backtest_results)){
      ####Check if length is 1
      if (length(old_winner_freq_ss_backtest_results) != 1){
        stop("Expected old_winner_freq_ss_backtest_results to have length 1, but got length ",
             length(old_winner_freq_backtest_results))
      }
      chosen_freq_ss_configs[[1]]                          <- old_winner_freq_ss_backtest_results@ss_backtest_config
      chosen_freq_ss_configs[[1]]@initial_sample_size      <- old_winner_freq_ss_backtest_results@ss_backtest_workflow[[1]]$initial_sample_size
      names(chosen_freq_ss_configs)                        <- winner_ss_config
      
      message("\nChosen frequentist SS configs:\n")
      print(names(chosen_freq_ss_configs))
    }
    ####Bayesian
    if (!is.null(old_winner_bayesian_ss_backtest_results)){
      ####Check if length is 1
      if (length(old_winner_bayesian_ss_backtest_results) != 1){
        stop("Expected old_winner_bayesian_ss_backtest_results to have length 1, but got length ",
             length(old_winner_bayesian_ss_backtest_results))
      }
      chosen_bayesian_ss_configs[[1]]                     <- old_winner_bayesian_ss_backtest_results@ss_backtest_config
      chosen_bayesian_ss_configs[[1]]@initial_sample_size <- old_winner_bayesian_ss_backtest_results@ss_backtest_workflow[[1]]$initial_sample_size
      names(chosen_bayesian_ss_configs)                   <- winner_ss_config["bayesian"]
      
      message("\nChosen Bayesian SS configs:\n")
      print(names(chosen_bayesian_ss_configs))
    }
    return(list(
      chosen_freq_ss_configs     = chosen_freq_ss_configs,
      chosen_bayesian_ss_configs = chosen_bayesian_ss_configs
    ))
  } 
  
  ##Frequentist Configurations--------------------------------------------------
    ###No Pooled, No P-correction
    freq_ss_config_1 <- factoRverse::create_ss_backtest_config(
      initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
      chosen_signals_and_positions = chosen_signals_and_positions,
      split_method = "expanding", config_name = "freq_np_none"
    ) %>% factoRverse::add_alpha_test_strategy(
      model_structure = "no_pooled", signal_significance_threshold = 0.05,
      p_correction_method = "none", market_factor_proxy = "ibov",
      enable_theme_representativeness = TRUE
    )
    
    ###No Pooled, Hommel
    freq_ss_config_2 <- factoRverse::create_ss_backtest_config(
      initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
      chosen_signals_and_positions = chosen_signals_and_positions,
      split_method = "expanding", config_name = "freq_np_hommel"
    ) %>% factoRverse::add_alpha_test_strategy(
      model_structure = "no_pooled", signal_significance_threshold = 0.05,
      p_correction_method = "hommel", market_factor_proxy = "ibov",
      enable_theme_representativeness = TRUE
    )
    
    ###No Pooled, BH
    freq_ss_config_3 <- factoRverse::create_ss_backtest_config(
      initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
      chosen_signals_and_positions = chosen_signals_and_positions,
      split_method = "expanding", config_name = "freq_np_BH"
    ) %>% factoRverse::add_alpha_test_strategy(
      model_structure = "no_pooled", signal_significance_threshold = 0.05,
      p_correction_method = "BH", market_factor_proxy = "ibov",
      enable_theme_representativeness = TRUE
    )
    
    ###No Pooled, BY
    freq_ss_config_4 <- factoRverse::create_ss_backtest_config(
      initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
      chosen_signals_and_positions = chosen_signals_and_positions,
      split_method = "expanding", config_name = "freq_np_BY"
    ) %>% factoRverse::add_alpha_test_strategy(
      model_structure = "no_pooled", signal_significance_threshold = 0.05,
      p_correction_method = "BH", market_factor_proxy = "ibov",
      enable_theme_representativeness = TRUE
    )
    
    ###Pooled, Theme Intercept + Theme Slope, None
    freq_ss_config_5 <- factoRverse::create_ss_backtest_config(
      initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
      chosen_signals_and_positions = chosen_signals_and_positions,
      split_method = "expanding", config_name = "freq_pp_theme_int_theme_slope"
    ) %>% factoRverse::add_alpha_test_strategy(
      model_structure = "partial_pooled", signal_significance_threshold = 0.05,
      p_correction_method = "none", market_factor_proxy = "ibov",
      enable_theme_representativeness = TRUE,
      theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific",
      lmer_control = list(lmer_optimizer = "Nelder_Mead")
    )
    
    ###Compile in list
    freq_ss_configs <- rlang::set_names(
      list(freq_ss_config_1, freq_ss_config_2, freq_ss_config_3, freq_ss_config_4, 
           freq_ss_config_5),
      nm = c(
        freq_ss_config_1@config_name,
        freq_ss_config_2@config_name,
        freq_ss_config_3@config_name,
        freq_ss_config_4@config_name,
        freq_ss_config_5@config_name
      )
    )
  
  ####Choose some
  chosen_freq_ss_configs <- 
    freq_ss_configs[which(names(freq_ss_configs) %in% chosen_ss_configs)]
  message("\nChosen frequentist SS configs:\n")
  print(names(chosen_freq_ss_configs))
  
  ##Bayesian Configurations-----------------------------------------------------
    ###Pooled, Theme Intercept + Theme Slope, Shrinkage Priors
    bayesian_ss_config_1 <- factoRverse::create_ss_backtest_config(
      initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
      chosen_signals_and_positions = chosen_signals_and_positions,
      split_method = "expanding", config_name = "bayesian_pp_theme_int_theme_slope_shrk"
    ) %>% factoRverse::add_alpha_test_strategy(
      model_structure = "partial_pooled", signal_significance_threshold = 0.05,
      p_correction_method = "bayesian", market_factor_proxy = "ibov",
      enable_theme_representativeness = TRUE,
      theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific",
      lmer_control = list(lmer_optimizer = "Nelder_Mead")
      ) %>% factoRverse::add_bayesian_model_parameters(
        brms_control = list(chains = 4, iter = 2000, warmup = 1000, thin = 1,
                            seed = 123, adapt_delta = 0.90)
        ) %>%
      factoRverse::add_brms_prior(effect = "fixed", type = "intercept", 
                     theme = c("tradingfrictions", "value", "quality", "lowfinrisk", "growth", "profitability", 
                               "seasonality", "investment", "lowmktrisk", "accruals", "lotterydemand", "momentum",
                               "ltreversal", "others"),
                     distribution_choice = rep("normal", 14),
                     pars = replicate(14, c(mean = 0, sd = 0.075), simplify = FALSE)) %>%
      factoRverse::add_brms_prior(effect = "fixed", type = "slope",
                     theme = c("tradingfrictions", "value", "quality", "lowfinrisk", "growth", "profitability", 
                               "seasonality", "investment", "lowmktrisk", "accruals", "lotterydemand", "momentum",
                               "ltreversal", "others"),
                     distribution_choice = rep("normal", 14),
                     pars = replicate(14, c(mean = 0, sd = 0.05), simplify = FALSE)) %>%
      factoRverse::add_brms_prior(effect = "random", type = "intercept", 
                     distribution_choice = c("student_t"),
                     pars = list(c(df = 5, mean = 0, sd = 0.1))) %>%
      factoRverse::add_brms_prior(effect = "random", type = "slope", 
                     distribution_choice = c("student_t"),
                     pars = list(c(df = 5, mean = 0, sd = 0.05))) %>%
      factoRverse::add_brms_prior(effect = "random", type = "sigma", 
                     distribution_choice = c("student_t"),
                     pars = list(c(df = 5, mean = 0, sd = 3.0))) %>%
      factoRverse::add_brms_prior(effect = "random", type = "cor",
                     distribution_choice = c("lkj"),
                     pars = c(eta = 2))
  
      
  ###Pooled, Theme Intercept + Theme Slope, EM Priors
  bayesian_ss_config_2 <- factoRverse::create_ss_backtest_config(
    initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
    chosen_signals_and_positions = chosen_signals_and_positions,
    split_method = "expanding", config_name = "bayesian_pp_theme_int_theme_slope_EM"
  ) %>% factoRverse::add_alpha_test_strategy(
    model_structure = "partial_pooled", signal_significance_threshold = 0.05,
    p_correction_method = "bayesian", market_factor_proxy = "ibov",
    enable_theme_representativeness = TRUE,
    theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific",
    lmer_control = list(lmer_optimizer = "Nelder_Mead")
  ) %>% factoRverse::add_bayesian_model_parameters(
    brms_control = list(chains = 4, iter = 2000, warmup = 1000, thin = 1,
                        seed = 123, adapt_delta = 0.90)
  )
  
  ###Pooled, Theme Intercept + Theme Slope, World Priors
  bayesian_ss_config_3 <- factoRverse::create_ss_backtest_config(
    initial_sample_size = initial_sample_size, rebalancing_months = 6, active_returns = TRUE,
    chosen_signals_and_positions = chosen_signals_and_positions,
    split_method = "expanding", config_name = "bayesian_pp_theme_int_theme_slope_WORLD"
  ) %>% factoRverse::add_alpha_test_strategy(
    model_structure = "partial_pooled", signal_significance_threshold = 0.05,
    p_correction_method = "bayesian", market_factor_proxy = "ibov",
    enable_theme_representativeness = TRUE,
    theme_level_intercept = "theme_specific", theme_level_slope = "theme_specific",
    lmer_control = list(lmer_optimizer = "Nelder_Mead")
  ) %>% factoRverse::add_bayesian_model_parameters(
    brms_control = list(chains = 4, iter = 2000, warmup = 1000, thin = 1,
                        seed = 123, adapt_delta = 0.90)
  )
  
  ###Compile in list
  bayesian_ss_configs <- rlang::set_names(
    list(bayesian_ss_config_1, bayesian_ss_config_2, bayesian_ss_config_3),
    nm = c(
      bayesian_ss_config_1@config_name,
      bayesian_ss_config_2@config_name,
      bayesian_ss_config_3@config_name
    )
  )
  
  ####Choose some
  chosen_bayesian_ss_configs <- 
    bayesian_ss_configs[which(names(bayesian_ss_configs) %in% chosen_ss_configs)]
  message("\nChosen bayesian SS configs:\n")
  print(names(chosen_bayesian_ss_configs))
  
  ##Validations-----------------------------------------------------------------
  #Check if at least chosen_freq_ss_configs or chosen_bayesian_ss_configs
  if (length(chosen_freq_ss_configs) == 0 && length(chosen_bayesian_ss_configs) == 0){
    stop("Please select at least one valid configuration")
  }
  
  #Check if chosen_ss_configs length is equal to chosen_bayesian + chosen_freq
  if (length(chosen_ss_configs) != 
      (length(chosen_freq_ss_configs) + length(chosen_bayesian_ss_configs))){
    stop("Some chosen_ss_configs were not found in either frequentist or bayesian configurations")
  }
  
  return(list(
    chosen_freq_ss_configs     = chosen_freq_ss_configs,
    chosen_bayesian_ss_configs = chosen_bayesian_ss_configs
  ))
  
}

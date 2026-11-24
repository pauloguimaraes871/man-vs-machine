# Copied from Multifatorial 3.0 pipeline, R/helpers/tools.R
# at commit 8739bb0bce966dad8f7c701f0c7413a6fa25f61f (see R/pipeline/README.md).

#' Run a Function with Logging and Error Capture
#'
#' Executes a function while redirecting all output and messages to a log file.
#' If an error occurs, output is restored to the console and the error is propagated.
#' This is useful for keeping clean logs of diagnostic steps (e.g., tests, filters) in a pipeline.
#'
#' @param fun A function (anonymous or named) to execute. Must return a value.
#' @param log_file Character. Desired name of the log file (without extension).
#' @param log_path Character. Directory where the log file will be saved.
#'
#' @return The result of the executed function. If an error occurs, it is printed to the console and re-thrown.
#'
#' @details
#' Output and messages generated during execution are captured and redirected to a log file in `log_path`,
#' named using the format `log_file_YYYY-MM-DD_HH-MM-SS.log`. This allows inspection of verbose output
#' or diagnostics without polluting the console during `targets` runs or parallel processing.
#'
#' This function is designed to be compatible with `targets`, including parallel workflows,
#' and avoids assigning to the global environment.
#'
#'
#' @export
run_logged <- function(fun, log_file, log_path) {
  # Ensure log path exists
  if (!dir.exists(log_path)) {
    dir.create(log_path, recursive = TRUE)
  }
  
  # Construct timestamped log filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  full_log_path <- file.path(log_path, paste0(log_file, "_", timestamp, ".log"))
  
  # Open file connection
  con <- file(full_log_path, open = "wt")
  
  # Ensure everything is cleaned up at the end
  on.exit({
    suppressWarnings(sink(NULL, type = "message"))
    suppressWarnings(sink(NULL, type = "output"))
    suppressWarnings(close(con))
  }, add = TRUE)
  
  # Redirect output and messages to file
  sink(con, type = "output")
  sink(con, type = "message")
  
  # Run function with error handling
  result <- tryCatch(
    fun(),
    error = function(e) {
      # Restore console output
      suppressWarnings(sink(NULL, type = "message"))
      suppressWarnings(sink(NULL, type = "output"))
      # Show error message to user
      message("❌ Error during execution: ", e$message)
      message("See log for details: ", full_log_path)
      stop(e)  # Propagate the error
    }
  )
  
  return(result)
}

#' Get the Name of the Most Recently Modified Folder
#'
#' This function scans a parent directory and returns the name of the folder
#' that was most recently modified. It assumes that all "objects" are represented
#' as folders and the date is inferred from the modification timestamp, not the folder name.
#'
#' @param parent_path Character. The path to the parent directory containing the folders.
#'
#' @return A character string with the name of the most recently modified folder.
#' @export
#'
get_latest_version <- function(parent_path) {
  # List all items in the folder
  all_items <- base::list.files(parent_path, full.names = TRUE)
  
  # Filter only directories
  folders <- all_items[base::file.info(all_items)$isdir]
  
  # Check if any folders exist
  if (length(folders) == 0) {
    stop("No folders found in the specified path.")
  }
  
  # Folder-level modification times
  folder_info  <- base::file.info(folders)
  folder_mtime <- folder_info$mtime
  
  # First pass: most recently modified folder(s)
  candidate_folders <- folders[folder_mtime == max(folder_mtime, na.rm = TRUE)]
  
  # If only one candidate, return it
  if (length(candidate_folders) == 1) {
    return(base::basename(candidate_folders))
  }
  
  # Tie-breaker: inspect contents inside each candidate folder
  get_inner_mtime <- function(folder_path) {
    inner_items <- base::list.files(
      folder_path,
      full.names = TRUE,
      recursive = TRUE,
      include.dirs = TRUE,
      no.. = TRUE
    )
    
    # If folder is empty, fall back to folder's own mtime
    if (length(inner_items) == 0) {
      return(base::file.info(folder_path)$mtime)
    }
    
    inner_mtime <- base::file.info(inner_items)$mtime
    max(inner_mtime, na.rm = TRUE)
  }
  
  candidate_inner_mtime <- vapply(
    candidate_folders,
    FUN = get_inner_mtime,
    FUN.VALUE = as.POSIXct(NA)
  )
  
  latest_folder <- candidate_folders[
    candidate_inner_mtime == max(candidate_inner_mtime, na.rm = TRUE)
  ]
  
  # Still tied after inspecting contents
  if (length(latest_folder) > 1) {
    stop(
      "Multiple folders remain tied even after inspecting modification times of their contents."
    )
  }
  
  base::basename(latest_folder)
}

#' Update workflow with latest versioned objects
#'
#' Updates the most recent workflow entry by attaching the latest available versions
#' of production data objects (e.g., gold tables, auxiliary tables, and backtest results),
#' based on folder modification times. The function validates input consistency and
#' conditionally adds version references depending on the specified object.
#'
#' @param workflow A list of workflow states, where each element is itself a list.
#' @param object_name Character. Name of the target object to be updated. Must be one of:
#' "signal_port_backtest_cohorts", "ss_backtest_results_bundles",
#' "mb_backtest_results_bundles", or "meta_port_backtest_results_bundles".
#' @param current_date Date or character coercible to Date. Must match the `current_date`
#' stored in the latest workflow entry (if present).
#' @param source Character. Data source identifier used to construct folder paths.
#'
#' @return A list with the same structure as `workflow`, where the last element is updated
#' with version identifiers for the relevant data objects.
update_workflow_with_versions <- function(workflow, object_name, current_date, source){
  
  #Extract most recent workflow
  if (object_name == "signal_port_backtest_cohorts"){
    n_workflows       <- 1
    current_workflow  <- workflow
  } else {
    n_workflows       <- length(workflow)
    current_workflow  <- workflow[[n_workflows]]
  }
  
  #Extract folder_version
  folder_date_version <- 
    paste0(lubridate::year(current_date), "_", 
           ifelse(lubridate::month(current_date) < 10,
                  paste0("0", lubridate::month(current_date)), 
                  lubridate::month(current_date))
    )
  
  #Initial checks
    ## Check that workflow list is a list
    if (!is.list(workflow)){
      stop("workflow must be a list.")
    }
  
    ## Check that object name is one of allowed options
    allowed_objects <- c("signal_port_backtest_cohorts", "ss_backtest_results_bundles", "mb_backtest_results_bundles", "meta_port_backtest_results_bundles")
    if (!object_name %in% allowed_objects){
      stop(paste0("object_name must be one of: ", paste(allowed_objects, collapse = ", ")))
    }
  
    ## Check that if workflow list has many elements, they are all lists
    if (!object_name %in% "signal_port_backtest_cohorts"){
      if (length(workflow) > 0 && !all(sapply(workflow, is.list))){
        stop("All elements of workflow must be lists.")
      }
    }
  
    ## Check that current_date element in current_workflow matches current_date arg
    if (!is.null(current_workflow$current_date) && 
        as.Date(current_workflow$current_date) != as.Date(current_date)){
      stop("current_date in current_workflow does not match current_date argument.")
    }
  
  #Based on object name, get latest version of the tables
    ### First let's add gold and gold aux which should belong to all
    #### Gold
    current_workflow$gold_results_version <- get_latest_version(here::here(
        "data", "prod",
        paste0("gold_", folder_date_version, "_", source, "_features")
      ))
    #### Gold Aux
    current_workflow$gold_aux_tables_version <- get_latest_version(here::here(
      "data", "prod",
      paste0("gold_", folder_date_version, "_", source, "_aux_tables")
    ))
    #### If object name is not signal_port_backtest_cohorts, add signal_port_backtest_cohorts version
    if (object_name != "signal_port_backtest_cohorts"){
      current_workflow$signal_port_backtest_cohorts_version <- get_latest_version(here::here(
        "data", "prod",
        paste0("gold_", folder_date_version, "_", source, "_signal_port_backtest_cohorts")
      ))
      
      #### If object name is not ss_backtest_results_bundles, add ss_backtest_results_bundles version
      if (object_name != "ss_backtest_results_bundles"){
        current_workflow$ss_backtest_results_bundles_version <- get_latest_version(here::here(
          "data", "prod",
          paste0("gold_", folder_date_version, "_", source, "_ss_backtest_results_bundles")
        ))
        
        #### If object name is not mb_backtest_results_bundles, add mb_backtest_results_bundles version
        if (object_name != "mb_backtest_results_bundles"){
          current_workflow$mb_backtest_results_bundles_version <- get_latest_version(here::here(
            "data", "prod",
            paste0("gold_", folder_date_version, "_", source, "_mb_backtest_results_bundles")
          ))
        }
      }
    }
    
  if (object_name != "signal_port_backtest_cohorts"){
    # Replace workflow last entry with updated current workflow
    workflow[[n_workflows]] <- current_workflow
    
    #Check that workflow length did not change
    if (length(workflow) != n_workflows){
      stop("Workflow length changed after update.")
    }
  } else {
    workflow                <- current_workflow
  }  
  
  #Message success
  message(paste0("Workflow updated with latest versions for object: ", object_name))
  
  return(workflow)
  
}

#' Recreate signals vector with corrected position
#'
#' @param chosen_signals_and_positions A named character vector `chosen_signals_and_positions`,
#'        whose *values* are "long", "short", or "force" and whose *names* are
#'        the (possibly `low_`-stripped) tickers.
#' @return An unnamed character vector of tickers, where every element that was
#'         marked `"short"` is prefixed with `"low_"`; all others are left
#'         unchanged.
correct_chosen_signals_and_positions <- function(chosen_signals_and_positions) {
  base::stopifnot(is.character(chosen_signals_and_positions), 
                  !is.null(base::names(chosen_signals_and_positions)))
  
  base::unname(
    base::ifelse(
      chosen_signals_and_positions == "short",
      paste0("low_", base::names(chosen_signals_and_positions)),
      base::names(chosen_signals_and_positions)
    )
  )
}

#' Load Historical Version of a meta_dataframe from Manifest
#'
#' Retrieves the latest saved version of a meta_dataframe (or similar object)
#' based on the information stored in a pipeline manifest. The function
#' searches the manifest's version table for the object name, identifies the
#' latest version path, and loads the corresponding \code{.qs} file.
#'
#' @param manifest A manifest list containing a \code{versions_table} data frame
#' with columns \code{object} and \code{latest_version} indicating where each
#' object version is stored.
#' @param object_name Character scalar indicating the name of the object to be
#' loaded. The function searches for entries in the manifest whose
#' \code{object} column starts with this value.
#'
#' @return The object loaded from disk using \code{qs::qread()}, typically a
#' \code{meta_dataframe} or related pipeline object.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#' \item Searches \code{manifest$versions_table} for rows where the
#' \code{object} column starts with \code{object_name}.
#' \item Extracts the \code{latest_version} path associated with the object.
#' \item Stops execution if multiple versions are detected.
#' \item Loads the object from the corresponding \code{.qs} file stored inside
#' the version folder.
#' }
#'
#' Informative messages are printed indicating the source path used for loading.
#'
#' @seealso \code{\link[qs]{qread}}, \code{\link[stringr]{str_detect}}
load_old_object <- function(manifest, object_name){
  
  ###Path
  old_m_df_path<- manifest$versions_table %>%
    dplyr::filter(stringr::str_detect(object, paste0("^", object_name))) %>%
    dplyr::pull(latest_version)
  
  #### If more than one path, stop
  if (length(old_m_df_path) > 1) {
    stop("Multiple versions found for ", object_name)
  }
  message("Loading historical data for ", object_name, " from: ", old_m_df_path)
  
  ###Object
  old_m_df     <- qs::qread(
    file.path(old_m_df_path,
              paste0(stringr::str_extract(old_m_df_path,
                                          "(?<=data/prod/)[^/]+"), ".qs")
    )
  )
  
  return(old_m_df)
  
}

#' Compute window-based features from meta_xts objects
#'
#' Applies rolling or seasonal window transformations to selected columns of a
#' \code{meta_xts} object using \code{compute_window()}, merges the results into a
#' market-level \code{meta_dataframe}, and records the operation in the workflow.
#'
#' @param data_xts A \code{meta_xts} object containing the source time series.
#' @param bench_xts Optional benchmark \code{meta_xts} used for relative metrics.
#' @param selected_bench Optional benchmark identifier.
#' @param mkt_pregold_m_df A \code{meta_dataframe} to which computed features are appended.
#' @param col_names Character vector of columns to transform.
#' @param specific_dates Optional vector of dates at which features are evaluated.
#' @param period Window length passed to \code{compute_window()}.
#' @param window Window type (e.g. rolling, seasonal).
#' @param only_unique Logical indicating whether only unique values are used.
#' @param top_n Integer controlling top-N selection logic.
#' @param min_non_na Minimum number of non-missing observations.
#' @param FUN Summary statistic applied over the window.
#' @param feature_name_prefix Name of the generated feature.
#' @param mult_last_n Integer controlling multiplicative adjustments.
#' @param mult_by Numeric multiplier.
#' @param na.rm Logical indicating NA removal in window computation.
#' @param parallel Logical indicating whether parallel execution is used.
#'
#' @return An updated \code{meta_dataframe} with the computed feature and workflow entry.
compute_features_windowed <- function(
    data_xts, bench_xts = NULL, selected_bench = NULL,
    mkt_pregold_m_df, col_names = colnames(data_xts@data), specific_dates = NULL,
    period = 11, window = "rolling", only_unique = FALSE, top_n = 1,
    min_non_na = 225, FUN = "skew", feature_name_prefix = "feature",
    mult_last_n = 0, mult_by = 1,
    na.rm = TRUE, parallel = TRUE) {
  
  ####Ensure specific dates are Date class
  if (!is.null(specific_dates)) {
    specific_dates <- as.Date(specific_dates)
  } else {
    specific_dates <- NULL
  }
  
  #####Define per-column function
  compute_one_column <- function(col_name) {
    feature_col_name <- paste0(feature_name_prefix, "_", col_name)
    
    result_xts <- withCallingHandlers(
      factoRverse::compute_window(
        data = data_xts,
        period = period,
        window = window,
        only_unique = only_unique,
        min_non_na = min_non_na,
        col_name = col_name,
        top_n = top_n,
        specific_dates = specific_dates,
        feature_name = feature_col_name,
        FUN = FUN,
        na.rm = na.rm,
        mult_last_n = mult_last_n,
        mult_by = mult_by,
        benchmark_returns_m_xts = bench_xts,
        selected_bench = selected_bench
      ),
      message = function(m) {
        if (
          grepl("Asset_type not identified for 'returns_meta_xts'", m$message) ||
          grepl("Detected frequency is: daily", m$message)
        ) {
          invokeRestart("muffleMessage")
        }
      }
    )
    
    result_xts@data[, feature_col_name, drop = FALSE]
  }
  
  ####Compute
  if (parallel) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = max(1L, future::availableCores() - 1L))
    result_list <- furrr::future_map(col_names, compute_one_column, .progress = TRUE)
  } else {
    result_list <- purrr::map(col_names, compute_one_column)
  }
  
  
  ####Tidy  
  #####Combine all columns into one xts
  result_xts <- do.call(cbind, result_list)
  #####Remove feature_name_prefix from colnames
  colnames(result_xts) <- stringr::str_remove_all(colnames(result_xts), paste0(feature_name_prefix, "_"))
  
  #####Turn into meta_dataframe
  results_m_df <- data.frame(
    dates = as.Date(zoo::index(result_xts)), zoo::coredata(result_xts),
    row.names = NULL
  ) %>% 
    tidyr::pivot_longer( #Transform to long
      cols = -dates,
      names_to = "tickers",
      values_to = feature_name_prefix
    ) %>% 
    as.data.frame() %>% 
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>% 
    dplyr::select(id, dates, tickers, !!rlang::sym(feature_name_prefix))
  
  ###Left-join
  mkt_pregold_m_df@data <- dplyr::left_join(
    mkt_pregold_m_df@data,  ###Adjust ids for so they match analysis_date <-> dates
    results_m_df %>% dplyr::mutate(
      dates = dplyr::if_else(dates %in% specific_dates, 
                             update(dates, day = 15),
                             dates),
      id    = paste0(tickers, "-", dates)
    ) %>%
      dplyr::select(-tickers, -dates),
    by = c("id")
  )
  
  ###Confirm it worked
  print(paste0("Feature '", feature_name_prefix, "' added. Summary:"))
  print(summary(mkt_pregold_m_df@data %>% dplyr::pull(!!rlang::sym(feature_name_prefix))))
  
  ###Workflow entry
  wf_entry <- list(
    list(
      period = period,
      window = window,
      feature_name = feature_name_prefix,
      FUN = FUN,
      na.rm = na.rm,
      min_non_na = min_non_na,
      only_unique = only_unique,
      top_n = top_n,
      mult_last_n = mult_last_n, 
      mult_by = mult_by, 
      computation = "meta_xts"
    )
  )
  mkt_pregold_m_df@workflow <- c(mkt_pregold_m_df@workflow, wf_entry)
  names(mkt_pregold_m_df@workflow)[length(mkt_pregold_m_df@workflow)] <- 
    paste0("compute_", feature_name_prefix , "_", mkt_pregold_m_df@current_date)
  
  
  return(mkt_pregold_m_df)
}

#' Replace all-NA numeric columns with zeros (single-date slice)
#'
#' Scans a single-date snapshot of a `gold_m_df` object and sets to 0 every
#' numeric, atomic column whose values are *all* `NA`.  This keeps the column
#' structure intact—handy when later modelling steps expect a fixed feature
#' set—while flagging the operation via console messages for easy debugging.
#'
#' @param gold_m_d_ref `data.frame`.  One-day cut of **gold\_m\_df** (i.e.,
#'   all rows share the same `dates` value).  Must contain  
#'   * a `dates` column (`Date`), and  
#'   * any number of numeric feature columns.
#'
#' @details
#' The helper executes three steps:
#' \enumerate{
#'   \item Prints the processing date (`cat()`), aiding pipeline logs.
#'   \item Locates candidate columns with  
#'         `is.numeric(x) && is.atomic(x) && all(is.na(x))`.
#'   \item Replaces those columns with 0 using  
#'         `dplyr::mutate(dplyr::across(..., ~ 0))` and prints the names
#'         of the columns affected.
#' }
#'
#' Only columns that are entirely `NA` on the given date are modified; mixed
#' `NA`/non-`NA` columns are left untouched.  Non-numeric columns are ignored.
#'
#' @return A `data.frame` identical to `gold_m_d_ref` but with the identified
#'   columns replaced by zeros.
#'
#' @seealso `dplyr::mutate()`, `dplyr::across()`
#' @keywords internal
replace_all_na_by_zero <- function(gold_m_d_ref) {
  ###Print for debugging
  cat(paste0("\nProcessing date: ", as.Date(unique(gold_m_d_ref$dates), "\n\n")))
  
  ###Identify cols to be replaced
  replaced_cols <- names(gold_m_d_ref)[
    sapply(gold_m_d_ref, function(x) {
      is.numeric(x) && is.atomic(x) && all(is.na(x))
    })
  ]
  
  ###Print replaced columns and replace
  if (length(replaced_cols) > 0) {
    cat("\nReplaced: ", paste(replaced_cols, collapse = ", "), "\n")
    gold_m_d_ref <- dplyr::mutate(
      gold_m_d_ref,
      dplyr::across(dplyr::all_of(replaced_cols), ~ 0)
    )
  }
  
  return(gold_m_d_ref)
}

#' Restore Date Index in a meta_xts Object
#'
#' This helper function ensures that the index of the `xts` object stored in the
#' `@data` slot of a `meta_xts` object is properly coerced to `Date` class. This is
#' particularly useful after deserialization (e.g., when loading a `meta_xts` object
#' from a `.rds` file or running in non-interactive environments like `targets::tar_make()`),
#' where the index may be converted to numeric.
#'
#' @param obj A `meta_xts` S4 object whose `@data` slot is an `xts` object.
#'
#' @return A `meta_xts` object with its `@data` index coerced to `Date`.
#'
#' @details Internally rewraps the `@data` slot using `xts::xts()` and `as.Date(zoo::index())`
#' to ensure the integrity of the index class.
#'
#' @examples
#' \dontrun{
#'   meta_obj <- restore_meta_xts_dates(meta_obj)
#' }
#'
#' @seealso [xts::xts()], [zoo::index()]
restore_meta_xts_dates <- function(obj) {
  obj@data <- xts::xts(obj@data, order.by = as.Date(zoo::index(obj@data)))
  return(obj)
}

#' Prepare auxiliary gold objects for backtesting by aligning dates
#'
#' This helper standardizes all auxiliary objects used alongside the main
#' `gold` dataset so that their time coverage strictly matches the vector
#' of `gold_dates`. It operates on a named list (`gold_aux_tables`) that
#' typically contains `meta_dataframe` (`*_m_df`) and `meta_xts`
#' (`*_m_xts`) objects produced during the feature engineering and
#' preprocessing pipeline.
#'
#' The function applies different filtering strategies depending on the
#' object suffix:
#'
#' • Objects ending with `_m_df` are assumed to contain a `@data`
#' `data.frame` with a `dates` column and are filtered using
#' `dplyr::filter(dates %in% gold_dates)`.
#'
#' • Objects ending with `_m_xts` are assumed to contain an xts-like
#' object in the `@data` slot. Their time index is first restored with
#' `factoRverse::restore_meta_xts_dates()` and then filtered using
#' `zoo::index()` to match `gold_dates`.
#'
#' This ensures complete temporal alignment across all auxiliary inputs
#' before running backtests, avoiding mismatched samples, unintended
#' look-ahead bias, or inconsistent benchmark/feature windows.
#'
#' @param gold_aux_tables Named list containing auxiliary gold objects.
#'   Each element is expected to be either a `meta_dataframe` object
#'   (suffix `_m_df`) or a `meta_xts` object (suffix `_m_xts`). Other
#'   elements are returned unchanged.
#'
#' @param gold_dates A `Date` vector defining the valid time window for
#'   backtesting. All auxiliary objects will be filtered to match exactly
#'   these dates.
#'
#' @return A list with the same structure as `gold_aux_tables`, but with
#'   all eligible objects filtered so their date coverage matches
#'   `gold_dates`.
#'
prepare_gold_aux_to_backtest <- function(gold_aux_tables, gold_dates) {
  
  stopifnot(is.list(gold_aux_tables))
  stopifnot(inherits(gold_dates, "Date"))
  
  ##Adjust dates----------------------------------------------------------------
  min_gold_date <- min(gold_dates)
  for (nm in names(gold_aux_tables)) {
    
    obj <- gold_aux_tables[[nm]]
    
    ## meta_dataframe objects
    if (grepl("_m_df$", nm)) {
      obj@data <- obj@data %>%
        dplyr::filter(dates >= min_gold_date)
    }
    
    ## meta_xts objects
    if (grepl("_m_xts$", nm)) {
      obj <- restore_meta_xts_dates(obj)
      obj@data <- obj@data[
        zoo::index(obj@data) >= min_gold_date,
        ,
        drop = FALSE
      ]
    }
    
    gold_aux_tables[[nm]] <- obj
  }
  
  ##Adjust themes structure-----------------------------------------------------
  gold_aux_tables$signal_themes_m_df@data <- gold_aux_tables$signal_themes_m_df@data %>% 
    dplyr::mutate(
      theme = stringr::str_replace_all(theme, "_", "")
    )
  gold_aux_tables$emerging_jkp_priors_m_df@data <- gold_aux_tables$emerging_jkp_priors_m_df@data %>% 
    dplyr::mutate(
      theme = stringr::str_replace_all(theme, "_", "")
    )
  gold_aux_tables$world_jkp_priors_m_df@data <- gold_aux_tables$world_jkp_priors_m_df@data %>% 
    dplyr::mutate(
      theme = stringr::str_replace_all(theme, "_", "")
    )
  gold_aux_tables$cluster_themes_m_df@data <- gold_aux_tables$cluster_themes_m_df@data %>% 
    dplyr::mutate(
      theme = stringr::str_replace_all(theme, "_", ""),
    ) 
  
  ##Adjust cluster_themes
  interaction_signals <- gold_aux_tables$signal_clusters_m_df@data %>%
    dplyr::filter(cluster == "interactions") %>%
    dplyr::pull(tickers) %>% 
    unique()
  industries_signals <- gold_aux_tables$signal_clusters_m_df@data %>%
    dplyr::filter(cluster == "industries") %>%
    dplyr::pull(tickers) %>% 
    unique()
  
  adj_cluster_themes_m_df <- factoRverse::create_meta_dataframe(
    gold_aux_tables$cluster_themes_m_df@data %>% 
      dplyr::filter(
        !tickers %in% c("interactions", "industries")
      ) %>%
      dplyr::mutate(
        tickers = paste0("fac_", tickers),
        id      = paste0(tickers, "-", dates)
      ) %>%
      dplyr::bind_rows(
        gold_aux_tables$signal_themes_m_df@data %>% 
          dplyr::filter(theme == "others") %>%
          dplyr::filter(tickers %in% c(industries_signals, interaction_signals))
      ) %>%
      dplyr::arrange(id),
    meta_dataframe_name = gold_aux_tables$cluster_themes_m_df@meta_dataframe_name
  )
  
  gold_aux_tables$adj_cluster_themes_m_df <- adj_cluster_themes_m_df
  
  return(gold_aux_tables)
  
}

#' Run a single frequentist SS backtest
#'
#' Internal helper that runs one frequentist signal-selection backtest,
#' logs execution metadata, and cleans workflow call information.
#'
#' @param config An `ss_backtest_config` object.
#' @param gold_m_df A `meta_dataframe` containing signal values.
#' @param returns_cohort_m_xts An `xts` object with backtest returns.
#' @param ret_indexes_m_xts An `xts` object with benchmark returns.
#' @param port_cohort_name Character scalar identifying the portfolio cohort.
#' @param themes_m_df A `meta_dataframe` mapping signals to themes.
#'
#' @return An `ss_backtest_results` object.
run_one_freq_backtest <- function(config,
                                  gold_m_df,
                                  returns_cohort_m_xts,
                                  ret_indexes_m_xts,
                                  port_cohort_name,
                                  themes_m_df) {
  
  ###Run backtest
  message("Running Freq backtest: ", config@config_name)
  message("▶️ Starting config: ", config@config_name, 
          " | PID: ", Sys.getpid(), 
          " | Time: ", Sys.time())
  
  ss_freq_backtest_results <- factoRverse::run_ss_backtest(
    config                              = config,
    signals_m_df                        = gold_m_df,
    backtest_returns_m_xts              = returns_cohort_m_xts,
    benchmark_returns_m_xts             = ret_indexes_m_xts,
    signal_themes_m_df                  = themes_m_df,
    priors_m_df                         = NULL,
    custom_signal_universe_metrics_m_df = NULL,
    verbose                             = TRUE,
    winsorization_probs                 = c(0.025, 0.975),
    parallel                            = FALSE
  )
  
  ###Clean metadata at runtime
  ss_freq_backtest_results@ss_backtest_workflow[[
    length(ss_freq_backtest_results@ss_backtest_workflow)
  ]]$call <- NULL
  
  ss_freq_backtest_results@ss_backtest_workflow[[
    length(ss_freq_backtest_results@ss_backtest_workflow)
  ]]$port_backtest_cohort_object_name <- port_cohort_name
  
  ###Return
  return(ss_freq_backtest_results)
  
}

#' Incrementally update a frequency-based signal backtest
#'
#' Iteratively updates an existing frequency-based backtest object by extending
#' the available data up to each date in `batches_dates`. For every batch date,
#' the function truncates the input datasets to the corresponding date and calls
#' \code{factoRverse::update_ss_backtest()} to extend the previous backtest
#' results without recomputing the entire history.
#'
#' @param old_results A `ss_backtest_results` object containing the previously
#' computed backtest to be extended.
#' @param batches_dates A vector of dates indicating the sequential batch
#' updates to apply.
#' @param gold_m_df A `meta_dataframe` containing signal values used in the
#' backtest.
#' @param returns_cohort_m_xts A `meta_xts` object containing cohort-level
#' signal portfolio returns.
#' @param ret_indexes_m_xts A `meta_xts` object containing benchmark index
#' returns.
#' @param themes_m_df A `meta_dataframe` mapping signals to their themes.
#'
#' @return An updated `ss_backtest_results` object including all batch updates.
update_one_freq_backtest <- function(old_results,
                                     batches_dates,
                                     gold_m_df,
                                     returns_cohort_m_xts,
                                     ret_indexes_m_xts,
                                     themes_m_df){
  
  message("Updating Freq backtest: ", old_results@ss_backtest_workflow[[length(old_results@ss_backtest_workflow)]]$config_name, "...")
  
  ##Run through
  for (batch_date in batches_dates){
    
    message("Processing batch date: ", as.Date(batch_date))
    
    ##Prepare obj----------------------------------------------------------------
    gold_m_upd_ref_batch_date <- gold_m_df
    gold_m_upd_ref_batch_date@data <- gold_m_df@data %>% 
      dplyr::filter(dates <= batch_date)
    gold_m_upd_ref_batch_date@current_date <- as.Date(batch_date)
    
    ret_indexes_m_xts_upd_ref      <- restore_meta_xts_dates(ret_indexes_m_xts)
    ret_indexes_m_xts_upd_ref@data <- ret_indexes_m_xts_upd_ref@data[
      zoo::index(ret_indexes_m_xts_upd_ref@data) <= batch_date,
      ]
    ret_indexes_m_xts_upd_ref@current_date <- as.Date(batch_date)
    
    updated_returns_cohort_m_xts_upd_ref      <- restore_meta_xts_dates(returns_cohort_m_xts)
    updated_returns_cohort_m_xts_upd_ref@data <- updated_returns_cohort_m_xts_upd_ref@data[
      zoo::index(updated_returns_cohort_m_xts_upd_ref@data) <= batch_date,
      ]
    updated_returns_cohort_m_xts_upd_ref@current_date <- as.Date(batch_date)
    themes_m_df_upd_ref_batch_date <- themes_m_df
    themes_m_df_upd_ref_batch_date@data <- themes_m_df@data %>%
      dplyr::filter(dates <= batch_date)
    themes_m_df_upd_ref_batch_date@current_date <- as.Date(batch_date)
    
    ##Update--------------------------------------------------------------------
    updated_ss_freq_backtest_results <- factoRverse::update_ss_backtest(
      signals_m_df                        = gold_m_upd_ref_batch_date,
      updated_backtest_returns_m_xts      = updated_returns_cohort_m_xts_upd_ref,
      benchmark_returns_m_xts             = ret_indexes_m_xts_upd_ref,
      signal_themes_m_df                  = themes_m_df_upd_ref_batch_date,
      old_results                         = old_results,
      priors_m_df                         = NULL,
      custom_signal_universe_metrics_m_df = NULL,
      verbose                             = TRUE,
      parallel                            = FALSE
    )
    
    ###Clean metadata at runtime
    updated_ss_freq_backtest_results@ss_backtest_workflow[[
      length(updated_ss_freq_backtest_results@ss_backtest_workflow)
    ]]$call <- NULL
    
    
    ###Pass updated results to next iteration 
    old_results <- updated_ss_freq_backtest_results
    
  }
  
  return(old_results)
  
}

#' Run a single Bayesian SS backtest
#'
#' Internal helper that runs one Bayesian signal-selection backtest with
#' priors, logs runtime and memory diagnostics, and cleans workflow metadata.
#'
#' @param config An `ss_backtest_config` object.
#' @param gold_m_df A `meta_dataframe` containing signal values.
#' @param returns_cohort_m_xts An `xts` object with backtest returns.
#' @param ret_indexes_m_xts An `xts` object with benchmark returns.
#' @param port_cohort_name Character scalar identifying the portfolio cohort.
#' @param themes_m_df A `meta_dataframe` mapping signals to themes.
#' @param prior_m_df A `meta_dataframe` with Bayesian priors.
#' @param parallel Logical; whether to enable parallel execution.
#'
#' @return An `ss_backtest_results` object.
run_one_bayesian_backtest <- function(config,
                                      gold_m_df,
                                      returns_cohort_m_xts,
                                      ret_indexes_m_xts,
                                      port_cohort_name,
                                      themes_m_df,
                                      prior_m_df,
                                      parallel) {
  
  ###Run backtest
  #options(brms.backend = "cmdstanr")
  #Sys.setenv(CMDSTAN = "C:/cmdstan/cmdstan-2.37.0")
  #cmdstanr::set_cmdstan_path("C:/cmdstan/cmdstan-2.37.0")
  
  #p <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  #if (!nzchar(p) || !dir.exists(p)) {
  #  stop("CmdStan path not set or invalid in this process. Got: '", p,
  #       "'. Check CMDSTAN + set_cmdstan_path().")
  #}
  
  ####Initial Logs
  message("Running Bayesian backtest: ", config@config_name)
  message("▶️ Starting config: ", config@config_name, 
          " | PID: ", Sys.getpid(), 
          " | Time: ", Sys.time())
  message("🔧  brms using backend:: ", getOption("brms.backend"))
  message("🧵 Threads set: ", Sys.getenv("STAN_NUM_THREADS", unset = NA))
  mem_before <- pryr::mem_used()
  message("📦 Memory before: ", format(mem_before, units = "auto"))
  
  ##Checkpoint Setup
  checkpoint <- save_checkpoint(
    path = "bayesian_ss_checkpoint", 
    current_date = gold_m_df@current_date
  )
  last_steps_done <- checkpoint$last_steps_done
  checkpoint_file <- checkpoint$checkpoint_file
  if (!is.null(checkpoint$data)){
    message("Resuming from checkpoint. Last steps done: ", 
            paste(last_steps_done, collapse = ", "))
    ss_bayesian_backtest_results <- checkpoint$data #Adjust!!
  }
  
  step <- config@config_name
  if (!skip_block(step, last_steps_done = last_steps_done)){
    ####Run
    ss_bayesian_backtest_results <- factoRverse::run_ss_backtest(
      config                              = config,
      signals_m_df                        = gold_m_df,
      backtest_returns_m_xts              = returns_cohort_m_xts,
      benchmark_returns_m_xts             = ret_indexes_m_xts,
      signal_themes_m_df                  = themes_m_df,
      priors_m_df                         = prior_m_df,
      custom_signal_universe_metrics_m_df = NULL,
      verbose                             = TRUE,
      winsorization_probs                 = c(0.025, 0.975),
      parallel                            = parallel
    )
    
    ####Clean metadata at runtime and add cohort name
    ss_bayesian_backtest_results@ss_backtest_workflow[[
      length(ss_bayesian_backtest_results@ss_backtest_workflow)
    ]]$call <- NULL
    
    ss_bayesian_backtest_results@ss_backtest_workflow[[
      length(ss_bayesian_backtest_results@ss_backtest_workflow)
    ]]$port_backtest_cohort_object_name <- port_cohort_name
    
    
    #Save checkpoint just before leaving the block
    last_steps_done <- dplyr::union(last_steps_done, step)  
    checkpoint <- list(steps = last_steps_done, data = ss_bayesian_backtest_results)
    qs::qsave(checkpoint, checkpoint_file)
    
  }
  
  
  
  #### Final logs
  gc()
  mem_after <- pryr::mem_used()
  message("📦 Memory after: ", format(mem_after, units = "auto"))
  message("Time: ", Sys.time())
  
  
  
  ###Return
  return(ss_bayesian_backtest_results)
  
}

#' Run a Single Signal-Based (SB) Backtest
#'
#' Executes a signal-based model backtest using the configuration and input data provided. 
#' Designed for internal use within pipelines where a single model is run using predefined 
#' SS backtest results and SB configuration. Wraps `factoRverse::run_sb_backtest()` with 
#' additional logging and runtime metadata cleanup.
#'
#' @param ss_backtest_results An object of class `ss_backtest_results`, containing 
#'   the output from a signal selection backtest. Used as a foundation for building 
#'   the SB model.
#' @param gold_m_df A `meta_dataframe` containing engineered features used to train 
#'   the SB model. Typically comes from the output of the `featurize()` step.
#' @param gold_aux_tables A named list of auxiliary tables, expected to include at 
#'   least `fwd_3m_target_act_ibov_m_df`, a target matrix of realized future returns.
#' @param sb_config An object of class `sb_backtest_config` specifying the SB algorithm, 
#'   target name, training/rebalancing parameters, and any custom objective functions.
#' @param backtest_returns_m_xts Optional. A `meta_xts` object of historical signal-based 
#'   returns to reuse in backtest (e.g., to save time or ensure consistency).
#' @param benchmark_returns_m_xts Optional. A `meta_xts` object with benchmark returns 
#'   for relative performance evaluation.
#' @param signal_themes_m_df Optional. A `meta_dataframe` specifying thematic classifications 
#'   of signals, used in diagnostics and interpretability layers of the backtest.
#' @param inner_parallel Logical. Whether to enable parallel computation during model training 
#'   and evaluation within the SB backtest. Should typically be set to `FALSE` when 
#'   called within outer parallel loops.
#'
#' @return An object of class `sb_backtest_results` containing the fitted SB model, 
#'   performance metrics, diagnostics, and metadata about the backtest workflow.
#'
#' @section Notes:
#' - This function prints informative messages to indicate the model being run, 
#'   process ID, and timestamp.
#' - The function also removes the `call` element from the last entry of the 
#'   `sb_backtest_workflow` slot to reduce memory footprint when saving the result.
#' - `gc()` is called at the end to encourage memory cleanup after model execution.
run_one_sb_backtest <- function(ss_backtest_results, gold_m_df, 
                                gold_aux_tables, sb_config,
                                backtest_returns_m_xts = NULL,
                                benchmark_returns_m_xts = NULL,
                                signal_themes_m_df = NULL,
                                inner_parallel = FALSE,
                                verbose = FALSE
){
  
  ###Run backtest
  message("Running sb model backtest: ", sb_config@config_name)
  message("▶️ SS backtest: ", ss_backtest_results@backtest_identifier, 
          " | PID: ", Sys.getpid(), 
          " | Time: ", Sys.time())
  
  sb_model_results <- factoRverse::run_sb_backtest(
    features_m_df           = gold_m_df,
    target_m_df             = gold_aux_tables$fwd_3m_target_act_ibov_m_df,
    config                  = sb_config,
    ss_backtest_results     = ss_backtest_results,
    backtest_returns_m_xts  = backtest_returns_m_xts,
    benchmark_returns_m_xts = benchmark_returns_m_xts,
    signal_themes_m_df      = signal_themes_m_df,
    parallel                = inner_parallel,
    verbose                 = verbose
  )
  
  ###Clean metadata at runtime
  sb_model_results@sb_backtest_workflow[[
    length(sb_model_results@sb_backtest_workflow)
  ]]$call <- NULL
  
  ###Return
  gc()
  return(sb_model_results)
  
}

#' Incrementally update a model-based signal backtest
#'
#' Sequentially updates an existing SB backtest by extending the available
#' dataset to each date in `batches_dates`. For every batch date, the function
#' truncates the input features, targets, signal selection results, and return
#' series to the corresponding date and calls
#' \code{factoRverse::update_sb_backtest()} to extend the previous results
#' without recomputing the full backtest history.
#'
#' @param old_results A `sb_backtest_results` object containing the previously
#' computed model backtest to be extended.
#' @param batches_dates A vector of dates indicating the sequential batch updates
#' @param ss_backtest_results A signal selection backtest results object used
#' to determine the signal universe and associated metadata.
#' @param gold_m_df A `meta_dataframe` containing the feature set used by the
#' model backtest.
#' @param gold_aux_tables A list of auxiliary gold tables, including the target
#' dataset required for the SB backtest.
#' @param sb_config A configuration object describing the SB backtest setup.
#' @param backtest_returns_m_xts Optional `meta_xts` object containing signal
#' portfolio returns used for evaluation.
#' @param benchmark_returns_m_xts Optional `meta_xts` object containing
#' benchmark index returns.
#' @param signal_themes_m_df Optional `meta_dataframe` mapping signals to
#' themes.
#' @param inner_parallel Logical indicating whether the internal SB backtest
#' update should run in parallel.
#' @param verbose Logical indicating whether progress messages should be
#' printed during the update.
#'
#' @return An updated `sb_backtest_results` object including all batch updates.
update_one_sb_backtest <- function(old_results, 
                                   batches_dates,
                                   ss_backtest_results, gold_m_df, 
                                   gold_aux_tables, sb_config,
                                   backtest_returns_m_xts = NULL,
                                   benchmark_returns_m_xts = NULL,
                                   signal_themes_m_df = NULL,
                                   inner_parallel = FALSE,
                                   verbose = FALSE){
  
  
  message("Updating backtest: ", old_results@sb_backtest_workflow[[length(old_results@sb_backtest_workflow)]]$config_name, "...")
  
  ##Run through
  for (batch_date in batches_dates){
    
    message("Processing batch date: ", as.Date(batch_date))
    
    ##Prepare obj----------------------------------------------------------------
    batch_date <- as.Date(batch_date)
    gold_m_upd_ref_batch_date <- gold_m_df
    gold_m_upd_ref_batch_date@data <- gold_m_df@data %>% 
      dplyr::filter(dates <= batch_date)
    gold_m_upd_ref_batch_date@current_date <- as.Date(batch_date)
    
    update_backtest_returns_m_xts_upd_ref      <- restore_meta_xts_dates(backtest_returns_m_xts)
    update_backtest_returns_m_xts_upd_ref@data <- update_backtest_returns_m_xts_upd_ref@data[
      zoo::index(update_backtest_returns_m_xts_upd_ref@data) <= batch_date,
    ]
    update_backtest_returns_m_xts_upd_ref@meta_xts_name <- old_results@sb_backtest_workflow[[1]]$backtest_returns_object_name
    update_backtest_returns_m_xts_upd_ref@current_date <- as.Date(batch_date)
    benchmark_returns_m_xts_upd_ref      <- restore_meta_xts_dates(benchmark_returns_m_xts)
    benchmark_returns_m_xts_upd_ref@data <- benchmark_returns_m_xts_upd_ref@data[
      zoo::index(benchmark_returns_m_xts_upd_ref@data) <= batch_date,
    ]
    benchmark_returns_m_xts_upd_ref@meta_xts_name <- old_results@sb_backtest_workflow[[1]]$benchmark_returns_object_name
    benchmark_returns_m_xts_upd_ref@current_date <- as.Date(batch_date)
    
    signal_themes_m_df_upd_ref_batch_date <- signal_themes_m_df
    signal_themes_m_df_upd_ref_batch_date@data <- signal_themes_m_df@data %>%
      dplyr::filter(dates <= batch_date)
    signal_themes_m_df_upd_ref_batch_date@current_date <- as.Date(batch_date)
    
    target_m_df_upd_ref_batch_date       <- gold_aux_tables$fwd_3m_target_act_ibov_m_df 
    target_m_df_upd_ref_batch_date@data  <- target_m_df_upd_ref_batch_date@data %>% 
      dplyr::filter(dates <= batch_date)
    target_m_df_upd_ref_batch_date@data  <- target_m_df_upd_ref_batch_date@data %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::starts_with("fwd_"),
          .fns = ~ base::ifelse(
            ###Condition
            dates >= lubridate::add_with_rollback(
              as.Date(batch_date), 
              months(-old_results@sb_backtest_workflow[[1]]$target_fwd + 1)
            ),
            ###Replace by NA most recent months
            NA_real_,
            ###Do nothing for older dates
            .
          )
        )
      )
    target_m_df_upd_ref_batch_date@current_date <- as.Date(batch_date)
    
    updated_ss_backtest_results_upd_ref_batch_date <- ss_backtest_results
    updated_ss_backtest_results_upd_ref_batch_date@signal_universe_m_df@data <- updated_ss_backtest_results_upd_ref_batch_date@signal_universe_m_df@data %>% 
      dplyr::filter(dates <= batch_date)
    updated_ss_backtest_results_upd_ref_batch_date@final_signal_universe_m_d_ref@data <- updated_ss_backtest_results_upd_ref_batch_date@signal_universe_m_df@data %>%
      dplyr::filter(dates == max(dates))
    updated_ss_backtest_results_upd_ref_batch_date@ss_backtest_workflow[[length(updated_ss_backtest_results_upd_ref_batch_date@ss_backtest_workflow)]]$current_date <- 
      as.Date(batch_date)

    ##Update--------------------------------------------------------------------
    ###Update backtest
    message("Updating sb model backtest: ", old_results@backtest_identifier)
    message(" | PID: ", Sys.getpid(), 
            " | Time: ", Sys.time())
    
    ###Currently, run_sb_backtest will set backtest_returns_m_xts, benchmark_returns_m_xts and signal_themes_m_df
    ###to NULL if algo is not rp, hrp, mvo or mmaf. Passing them in update context will cause an error when checking
    ###update inputs. Therefore, we will set them to NULL beforehand
    if (!old_results@sb_backtest_config@sb_algorithm %in% c("rp", "hrp", "mvo", "mmaf")){
      update_backtest_returns_m_xts_upd_ref <- NULL
      benchmark_returns_m_xts_upd_ref       <- NULL
      signal_themes_m_df_upd_ref_batch_date <- NULL
    }
    
    updated_sb_model_results <-    factoRverse::update_sb_backtest(
      old_results                    = old_results,
      features_m_df                  = gold_m_upd_ref_batch_date,
      target_m_df                    = target_m_df_upd_ref_batch_date,
      updated_ss_backtest_results    = updated_ss_backtest_results_upd_ref_batch_date,
      updated_backtest_returns_m_xts = update_backtest_returns_m_xts_upd_ref,
      benchmark_returns_m_xts        = benchmark_returns_m_xts_upd_ref,
      signal_themes_m_df             = signal_themes_m_df_upd_ref_batch_date,
      parallel                       = inner_parallel,
      verbose                        = verbose
    )
    
    ###Clean metadata at runtime
    updated_sb_model_results@sb_backtest_workflow[[
      length(updated_sb_model_results@sb_backtest_workflow)
    ]]$call <- NULL
    
    
    ###Pass updated results to next iteration 
    old_results <- updated_sb_model_results
    
  }
  
  return(old_results)
  
}

#' Adjust signal universe for sector treatment and optional metrics alignment
#'
#' Updates the signal universe used in SB backtests by optionally handling forced
#' sector signals (exclude or reclassify) and optionally joining a metric column
#' from `signal_metrics_m_df`, aligned timewise to the signal universe.
#'
#' @param g A group/backtest object containing `ss_backtest_results`,
#'   `signal_themes_m_df`, and `signal_metrics_m_df`.
#' @param metric_col Optional character scalar. Name of a metric column from
#'   `signal_metrics_m_df` to be joined to the signal universe timewise.
#' @param sectors_treatment Character scalar or `NULL`. One of:
#'   `"exclude"` (remove forced sectors from eligibility),
#'   `"reclassify"` (restore original theme classification), or `NULL`
#'   (no sector adjustment).
#'
#' @return The input object `g` with an updated
#'   `ss_backtest_results@signal_universe_m_df`.
adjust_signal_universe <- function(g, metric_col = NULL, sectors_treatment) {
  
  ## Initial Setup--------------------------------------------------------------
  signal_universe_m_df <- g$ss_backtest_results@signal_universe_m_df
  signal_themes_m_df   <- g$signal_themes_m_df
  signal_metrics_m_df  <- g$signal_metrics_m_df
  
  ## Sectors Treatment----------------------------------------------------------
  ### Guard: must be one of "exclude" or "reclassify" or NULL
  if (!sectors_treatment %in% c("exclude", "reclassify", NULL)) {
    stop(
      paste0(
        "Invalid sectors_treatment value: ", sectors_treatment,
        ". Expected one of 'exclude', 'reclassify', or NULL."
      )
    )
  }
  ### For EW/SW, I do not want to force sectors eligibility.
  ### Will only be adjusted if forced signals are currently eligible
  if (sectors_treatment == "exclude"){
    if (any(signal_universe_m_df@data$theme == "forced" & signal_universe_m_df@data$is_eligible == 1, na.rm = TRUE)) {
      message("Adjusting signal universe to exclude forced signals for heuristic SB configurations...")
      
      signal_universe_m_df@data <- signal_universe_m_df@data %>%
        dplyr::mutate(
          is_eligible = dplyr::if_else(
            theme == "forced" & is_eligible == 1,
            0,
            is_eligible
          )
        )
    } else {
      message("No sector inelegibility needed for signal universe.")
    }
  }
  
  ### For HRP/MVO/RP/MMAF, I want all sectors to be included, but not as a "forced" single name
  ### Instead, each sector should be exposed via their original theme
  if (sectors_treatment == "reclassify"){
    message("Adjusting signal universe to reclassify forced signals for heuristic SB configurations...")
    
    forced_sectors <- signal_universe_m_df@data %>% 
      dplyr::filter(theme == "forced") %>%
      dplyr::pull(tickers) %>%
      unique()
    original_classification <- signal_themes_m_df@data %>%
      dplyr::filter(tickers %in% forced_sectors) %>%
      dplyr::pull(theme) %>%
      unique()
    
    #### Only one original classification is expected
    if (length(original_classification) != 1) {  
      stop(
        paste0(
          "Error in reclassifying forced sectors: expected a single original ",
          "classification for forced sectors, but found ",
          length(original_classification),
          ". Please check the signal themes data frame."
        )
      )
    }
    
    ### Reclassify
    signal_universe_m_df@data <- signal_universe_m_df@data %>%
      dplyr::mutate(
        theme = dplyr::if_else(
          tickers %in% forced_sectors,
          original_classification,
          theme
        )
      )
    
    #### Guard: only forced_sectors should have a different theme now
    if (!identical(
      g$ss_backtest_results@signal_universe_m_df@data %>%
      dplyr::filter(theme != "forced") %>%
      dplyr::pull(theme),
      signal_universe_m_df@data %>%
      dplyr::filter(!tickers %in% forced_sectors) %>%
      dplyr::pull(theme)
    )){
      stop(
        paste0(
          "Error in reclassifying forced sectors: unexpected change in themes ",
          "for non-forced sectors. Please check the signal themes data frame."
        )
      )
    }
  }
  
  ## Incorporate custom signal universe------------------------------------------ 
  
  ## Optional join 
  if (!is.null(metric_col)) {
    
    ### Get dates from signal metrics
    signal_metrics_dates <- signal_metrics_m_df@data %>% 
      dplyr::pull(dates) %>% unique() %>% sort() 
    
    #### Guard: First date in signal metrics must be after birst date of signal_universe
    if (min(signal_metrics_dates) < min(signal_universe_m_df@data$dates)) {
      stop(
        paste0(
          "Error: The earliest date in signal metrics (", min(signal_metrics_dates), 
          ") is before the earliest date in the signal universe (", min(signal_universe_m_df@data$dates), 
          "). Please check the data frames."
        )
      )
    }
    
    ### For each signal_metrics_dates, retrieve most up-to-date row from signal_universe_m_df
    signal_universe_m_df <- purrr::map_dfr(signal_metrics_dates, function(curr_signal_metrics_date){
      
      #### Subset for current date
      signal_metrics_m_d_ref <- signal_metrics_m_df@data %>%
        dplyr::filter(dates == curr_signal_metrics_date)
      
      #### Get closest date from signal_universe_m_df
      signal_universe_m_d_ref <- signal_universe_m_df@data %>% 
        dplyr::filter(dates <= curr_signal_metrics_date) %>%
        dplyr::filter(dates == max(dates)) %>% 
        dplyr::mutate(
          dates   = as.Date(curr_signal_metrics_date),
          id      = paste0(tickers, "-", dates) 
        )
      
      #### Join and return,
      signal_universe_m_d_ref_joined <- signal_universe_m_d_ref %>%
        dplyr::left_join(
          signal_metrics_m_d_ref %>%
            dplyr::select(id, !!rlang::sym(metric_col)),
          by = "id"
        )
      
      signal_universe_m_d_ref_joined
      
    }) %>%
      dplyr::arrange(id) %>%
      factoRverse::create_meta_dataframe()
    
  }
  
  ## Return--------------------------------------------------------------
  g$ss_backtest_results@signal_universe_m_df@data <- signal_universe_m_df@data
  g
}

#' Initialize or resume a checkpoint
#'
#' Creates a checkpoint directory structure and optionally resumes a previous
#' checkpoint if a matching `.qs` file exists for the provided date.
#'
#' @param path Character scalar. Identifier used to name the checkpoint folder
#'   and file.
#' @param current_date Character or Date used to version the checkpoint file.
#'
#' @return A list with `last_steps_done`, previously saved `data` (if any),
#'   and the `checkpoint_file` path.
save_checkpoint <- function(path, current_date){
  ##Checkpoint Setup
  checkpoint_root <- getOption("mvm.checkpoint_root", here::here("data", "checkpoints"))
  checkpoint_dir  <- fs::path(checkpoint_root, path)
  checkpoint_file <- fs::path(
    checkpoint_dir, paste0(path, "_", current_date, ".qs")
  )
  if (!fs::dir_exists(checkpoint_root)) fs::dir_create(checkpoint_root)
  
  ###Resume if possible
  if (fs::file_exists(checkpoint_file)) {
    message("🔁  Resuming from checkpoint at ", checkpoint_file)
    checkpoint      <- qs::qread(checkpoint_file)
    last_steps_done <- checkpoint$steps
    data            <- checkpoint$data
  } else {
    message("💾 Checkpoint folder: ", checkpoint_dir)
    fs::dir_create(checkpoint_dir, recurse = TRUE)
    last_steps_done <- character(0)  # start fresh
    data <- NULL
  }
  
  return(list(
    last_steps_done = last_steps_done,
    data            = data,
    checkpoint_file = checkpoint_file
  ))
  
  
}

#' Check whether a pipeline step should be skipped
#'
#' Determines if a step was already completed based on checkpoint history and
#' optionally prints a message when skipping.
#'
#' @param step Character scalar identifying the current step.
#' @param last_steps_done Character vector of previously completed steps.
#'
#' @return Logical scalar indicating whether the step should be skipped.
skip_block <- function(step, last_steps_done){
  done <- step %in% last_steps_done
  if (done) message("✅  Step '", step, "' already completed. Skipping.")
  done
}

#' Get latest or specific manifest file path
#'
#' Identifies the most recent manifest file inside the manifests directory,
#' or retrieves a specific manifest if requested.
#'
#' @param manifests_dir Character. Path to manifests directory.
#' @param manifest_name Character or NULL. Specific manifest file name
#'   (e.g., "manifest_2024_01.qs"). If NULL, the most recent manifest is selected.
#'
#' @return Character. Full path to the selected manifest file.
get_latest_manifest_path <- function(
    manifests_dir = here::here("data", "prod", "manifests"),
    manifest_name = NULL
) {
  
  # List files
  manifest_files <- base::list.files(manifests_dir)
  
  message("Manifest directory: ", manifests_dir)
  message("Files detected: ", paste(manifest_files, collapse = ", "))
  
  # Safety check: no files
  if (length(manifest_files) == 0) {
    stop("No manifest files found in directory.")
  }
  
  # ---- Specific manifest requested -------------------------------------------
  if (!is.null(manifest_name)) {
    
    if (!is.character(manifest_name) || length(manifest_name) != 1) {
      stop("manifest_name must be a single character string.")
    }
    
    if (!(manifest_name %in% manifest_files)) {
      stop(
        "Requested manifest not found: ", manifest_name, "\n",
        "Available manifests: ", paste(manifest_files, collapse = ", ")
      )
    }
    
    message("Specific manifest requested: ", manifest_name)
    return(base::file.path(manifests_dir, manifest_name))
  }
  
  # ---- Default behavior: find most recent -------------------------------------
  
  # Case: only initial manifest exists
  if (length(manifest_files) == 1 && identical(manifest_files, "manifest_initial.qs")) {
    message("Only initial manifest found. Using manifest_initial.qs")
    return(base::file.path(manifests_dir, "manifest_initial.qs"))
  }
  
  message("Multiple manifests detected. Determining most recent...")
  
  # Remove initial manifest if present
  manifest_files <- manifest_files[manifest_files != "manifest_initial.qs"]
  
  # Safety check: no valid dated manifests left
  if (length(manifest_files) == 0) {
    stop("No dated manifest files found after excluding 'manifest_initial.qs'.")
  }
  
  # Extract dates
  manifest_dates_raw <- stringr::str_remove(manifest_files, "^manifest_")
  manifest_dates_raw <- stringr::str_remove(manifest_dates_raw, "\\.qs$")
  
  manifest_dates <- suppressWarnings(
    as.Date(paste0(manifest_dates_raw, "_01"), format = "%Y_%m_%d")
  )
  
  # Validate parsed dates
  if (any(is.na(manifest_dates))) {
    stop(
      "Failed to parse dates from manifest filenames. ",
      "Check naming convention 'manifest_YYYY_MM.qs'."
    )
  }
  
  message("Parsed manifest dates: ", paste(manifest_dates, collapse = ", "))
  
  # Select most recent
  most_recent_idx <- which.max(manifest_dates)
  most_recent_file <- manifest_files[most_recent_idx]
  
  message("Most recent manifest selected: ", most_recent_file)
  
  base::file.path(manifests_dir, most_recent_file)
}

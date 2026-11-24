# run_paper_machine.R ---------------------------------------------------------
#
# Rebuilds the paper's "machine" from the July 2026 PRODUCTION manifest of the
# Multifatorial 3.0 pipeline, under academic (not deployment) settings. It is
# self-contained: the pipeline code it uses is in R/pipeline/ and its inputs are
# in data/inputs/machine_2026_07/. The pipeline repo is not needed.
#
# Relation to the pipeline repo
# -----------------------------
# The pipeline's own paper configuration lives on branch `paper/man-vs-machine`,
# which forked off `challenger/version2` at e8f3c9f. That challenger introduced a
# NEW factor library, and its verdict (d8c37c6) was that the library is *not
# adopted* -- so production (`main` == `challenger/version8`) still runs the OLD
# library, and that is what the latest manifest holds. This script therefore
# re-runs the paper's machine on the PRODUCTION gold objects rather than on the
# paper branch's unadopted library.
#
# Deviations from production, and why
# -----------------------------------
#   (a) Signal library screened: the `macro` theme and the `interactions`
#       cluster are dropped. Macro premia are time-series, not cross-sectional
#       firm characteristics; interactions are composites of other signals in
#       the same library, so they double-count. Industries (the `force` side)
#       and short interest are RETAINED.
#   (b) Signal selection uses BH only, and `initial_sample_size` is set to the
#       smallest window at which BH actually selects a factor on its own,
#       measured by the probe in stage 5 rather than assumed.
#   (c) Signal blending uses the ML pool only (OLS, GLMNET, RF, XGB, NN1-NN3),
#       combined by an equal-weighted meta-blend (`ew_meta`). No heuristic base
#       learners, no election -- every base learner enters the ensemble.
#   (d) Four mandates: ibov, smll, isee, and idiv. The idiv mandate keeps the
#       dividend-payer AND rule as its investable universe (mean DPS over the
#       trailing 36m > 0) and uses IDIV purely as the construction benchmark --
#       it is NOT restricted to IDIV constituents. The `exp_ret_score` scaler is
#       switched off entirely: no inverse-idio-vol tilt and no benchmark-
#       proximity tilt, so the machine's score is the model's prediction.
#
# This script never reads or writes the pipeline repo or its pins board. All
# outputs land under data/derived/machine_run/ here.
#
# Usage
# -----
#   source("run_paper_machine.R")     # runs main(), honouring STOP_AFTER below
#
# Stage 7 (base signal blending) is the expensive one: Bayesian-tuned RF/XGB/NN
# over the rebalancing grid runs for hours to days. Every stage is cached to disk
# and skipped when its output already exists, so the script is resumable.
#
# ---- 1. CONFIG --------------------------------------------------------------

HELPERS_DIR     <- here::here("R", "pipeline")
INPUTS_DIR      <- here::here("data", "inputs", "machine_2026_07")
PIPELINE_COMMIT <- "8739bb0bce966dad8f7c701f0c7413a6fa25f61f"  # origin of R/pipeline/
FACTORVERSE_PIN <- "0.7.0"

# PINNED, not NULL. `NULL` resolves to the latest dated manifest in the
# pipeline's board, which silently changes underneath a partially cached run:
# the pipeline published manifest_2026_08.qs on 2026-08-15 20:56, after the sb
# (08-12), meta (08-13) and mb (08-14) stages had already been cached against
# manifest_2026_07.qs. Any stage added later then loaded 2026-08 gold whose
# meta-dataframes are stamped current_date 2026-08-15, while the cached backtest
# objects carry 2026-07-15, and run_port_backtest()'s check_consistent_dates()
# rejected the mix.
#
# This is the vintage the cached artefacts in data/derived/machine_run were
# actually built from -- provenance.qs records it. Bumping it means re-running
# every stage from `sb` onward, not just the new ones.
MANIFEST_NAME <- "manifest_2026_07.qs"

# Python backend -------------------------------------------------------------
# factoRverse's fit_keras_model() is written against the Keras 2 API, and the
# renv lock pins R keras 2.15.0 to match. But renv does NOT manage the Python
# side. Name the interpreter in this project's .Renviron (not tracked):
#   RETICULATE_PYTHON=<path to python.exe>
# Without it reticulate falls back to its default virtualenv, which may carry
# Keras 3 and fail with:
#   ValueError: Only input tensors may be passed as positional arguments
# (surfacing confusingly as "objeto 'fit_nn' nao encontrado", because
# fit_keras_model()'s tryCatch swallows the real error and then returns a variable
# it never assigned).
#
# See environment/README.md. Set to a path to override.
RETICULATE_PYTHON_OVERRIDE <- NULL

# Stop after this stage. Use "screen" or "probe" for a cheap validation pass
# before committing to the long ML run.
#   "screen" | "probe" | "ss" | "sb" | "mb" | "meta" | "learners"
STOP_AFTER <- "learners"

# (a) Signal groups excluded on academic grounds. CLUSTER names as defined in the
#     pipeline's clusterize_themes.R.
EXCLUDED_CLUSTERS <- c("macro", "interactions")

# (b) Signal selection
SS_CONFIG       <- "freq_np_BH"
SS_REBAL_MONTHS <- 6      # production's create_ss_backtest_configs() setting
# NULL => take the minimum viable size found by the stage-5 probe. Set an integer
# to override the probe and pin the window.
SS_INITIAL_SAMPLE_SIZE <- 81
PROBE_MIN_OBS   <- 24     # floor on candidate window sizes for the probe

# (c) Signal blending
#
# GRID arm (current). Deterministic, no Gaussian-process surrogate, and therefore
# none of the lengthscale failures or d^3 acquisition cost that made xgb_bayes
# intractable. It is also the closer match to the literature: Rubesam (2021) picks
# hyperparameters "based on the MSE in the validation sample", and Hanauer &
# Kalsbach (2023) optimise over ranges in a validation step -- neither uses
# Bayesian optimisation. Swap back to the *_bayes names for the BO arm.
# reticulate cannot rebind Python within a session, so each arm must run in its
# own R process. PAPER_ARM selects the arm from the environment instead of
# requiring this line to be edited between runs; unset means the grid arm.
.ARM_CONFIGS <- list(
  grid  = c("ols", "glmnet_grid", "rf_grid", "xgb_grid",
            "nn1_grid", "nn2_grid", "nn3_grid"),
  bayes = c("ols", "glmnet_bayes", "rf_bayes", "xgb_bayes",
            "nn1_bayes", "nn2_bayes", "nn3_bayes")
)
.paper_arm <- Sys.getenv("PAPER_ARM", "grid")
if (!.paper_arm %in% names(.ARM_CONFIGS)) {
  stop("PAPER_ARM must be 'grid' or 'bayes'; got '", .paper_arm, "'.")
}
SB_CONFIGS <- .ARM_CONFIGS[[.paper_arm]]

# (c2) TUNING-METRIC ARM -- robustness, orthogonal to the grid/bayes arm.
#
# Motivation. `rmse` scores the whole cross-section, but the strategy only
# trades one tercile of it, and the two disagree: across the seven learners the
# rank correlation between full-sample OOS RMSE and realised portfolio Sharpe is
# +0.11 (CS) / +0.18 (RP) -- near zero and the WRONG sign. See
# ML_MODEL_DIAGNOSTICS.md S4 for the evidence.
#
# What each arm changes, and what it deliberately does NOT:
#
#   mpe   pinball (quantile) loss. calculate_eval_metrics() computes
#           mean(ifelse(error >= 0, tau * error, (1 - tau) * (-error)))
#         with error = target - pred, so `error >= 0` is UNDER-prediction and
#         takes weight tau. At tau = 0.67 under-prediction is penalised 0.67
#         against 0.33, the fitted object is the 67th conditional quantile, and
#         predictions shift up.
#
#         CAVEAT worth keeping in view when reading the results: because the
#         portfolio uses only the cross-sectional RANKING, a uniform upward
#         shift changes nothing. The ranking moves only where the shift is
#         UNEVEN -- i.e. where conditional variance differs across stocks. So
#         tau > 0.5 is substantially a VOLATILITY TILT, pushing high-dispersion
#         names up the ranking. That may be desirable, but it is not the same
#         mechanism as "care more about the stocks actually bought", and the
#         paper should not describe it as tail-focus.
#
#   mphe  pseudo-Huber, delta = 1, applied at BOTH the selection metric and the
#         training objective. A SYMMETRIC robust loss: it downweights outliers
#         relative to squared error but still scores the whole distribution
#         evenly, so it tests outlier sensitivity rather than tail focus.
#
# THE TWO ARMS ARE DELIBERATELY NOT SYMMETRIC, and the asymmetry is the design:
#
#   mpe   SELECTION ONLY. custom_objective stays "squared_error", so training is
#         bit-identical to the baseline and the only thing that moves is which
#         hyperparameters win at each refit. That isolation is what makes the
#         comparison attributable.
#   mphe  SELECTION AND TRAINING. custom_objective = "pseudo_huber_error", which
#         translate_metrics() maps to "reg:pseudohubererror" for xgb and
#         keras::loss_huber(delta = huber_delta) for the neural networks. It is
#         NULL for ols, glmnet and rf, which take no custom objective -- so on
#         this arm those three learners change through SELECTION only while
#         xgb and nn1-nn3 change through both. Read the two groups accordingly:
#         a difference confined to glmnet/rf is a selection effect, one that
#         appears only in xgb/nn is a training-objective effect.
#
# CAVEAT ON EARLY STOPPING, mpe arm. translate_metrics() supports rmse, mae,
# mphe and mape as in-training metrics for xgb and nn, but NOT mpe; it falls
# back to rmse for the early-stop criterion while still applying mpe to
# hyperparameter selection (factoRverse warns about this at runtime). So on the
# mpe arm, xgb's and the NNs' early stopping still watches rmse. Selection is
# unaffected, which is the channel being tested, but the arm is not a pure
# pinball run and the paper should not claim it is.
#
# OLS is untuned and has no tuning_strategy, and class validity forbids ols
# configs from carrying any custom_objective other than the "squared_error"
# default at all ("Custom objectives are only allowed for 'sw', 'rp', 'hrp',
# 'mvo', 'mmaf', 'xgb' or 'nn' algorithms" -- confirmed by validObject()
# rejecting an attempted ols/pseudo_huber_error assignment outright). So OLS is
# bit-identical, and a free control, across ALL THREE arms including mphe: if
# the OLS portfolio moves at all, that is a plumbing bug, not a finding.
#
# quantile_tau feeds `mpe` only; huber_delta feeds `mphe` at both the metric and
# the Keras loss. Both are set on every arm so the config carries the value that
# was actually in force.
.EVAL_METRIC_ARMS <- list(
  rmse = list(metric = "rmse", tau = 0.5,  delta = 1, objective = "squared_error"),
  mpe  = list(metric = "mpe",  tau = 0.67, delta = 1, objective = "squared_error"),
  mphe = list(metric = "mphe", tau = 0.5,  delta = 1, objective = "pseudo_huber_error")
)
.paper_eval <- Sys.getenv("PAPER_EVAL_METRIC", "rmse")
if (!.paper_eval %in% names(.EVAL_METRIC_ARMS)) {
  stop("PAPER_EVAL_METRIC must be one of ",
       paste(names(.EVAL_METRIC_ARMS), collapse = ", "),
       "; got '", .paper_eval, "'.")
}
EVAL_METRIC    <- .EVAL_METRIC_ARMS[[.paper_eval]]$metric
EVAL_TAU       <- .EVAL_METRIC_ARMS[[.paper_eval]]$tau
EVAL_DELTA     <- .EVAL_METRIC_ARMS[[.paper_eval]]$delta
EVAL_OBJECTIVE <- .EVAL_METRIC_ARMS[[.paper_eval]]$objective
# Empty for the baseline so existing artefacts keep their current filenames and
# are NOT invalidated: sb_grid.qs stays sb_grid.qs, and the mpe/mphe arms write
# alongside as sb_grid_mpe.qs / sb_grid_mphe.qs.
EVAL_TAG <- if (identical(.paper_eval, "rmse")) "" else paste0("_", .paper_eval)

MB_CONFIG  <- "ew_meta"
SB_DATASET <- "ind_signals"   # pre-committed; no lockbox spent electing it
SB_VALIDATION_SAMPLE_SIZE <- 36

# Bayesian-optimization budget. See section 2b for the reasoning and the source
# evidence (challenger_version4/BAYESIAN_OPT_TRAINING_NOTES.md).
#   "balanced" (recommended) | "lean" | "production" (no patch)
TUNING_PRESET <- "balanced"

# production's nn2_bayes is configured with units = c(32, 16, 8) -- IDENTICAL to
# nn3_bayes -- while nn2_grid uses c(32, 16). TRUE restores the two-layer
# architecture so the H7 complexity ladder has three distinct NN rungs.
FIX_NN2_ARCHITECTURE <- TRUE

# Neural-network ensembling (factoRverse >= 0.7.0). A single network fit is one
# draw from the distribution over networks induced by the random weight
# initialisation; averaging k independently initialised members cuts that
# variance component by roughly k. GKX and Hanauer & Kalsbach both average 10,
# Rubesam 50; 1 reproduces pre-0.7.0 behaviour.
#
# Ensembling applies to the REFIT only -- tuning always fits a single network --
# so the search cost is unchanged and only refits scale with k. An ensemble mean
# is less dispersed cross-sectionally than a single draw, which would shift
# weight in an equal-weighted meta-blend; ew_meta sets
# normalize_base_predictions = TRUE, which is what handles that.
NN_ENSEMBLES <- 10L

# Grid realignment -----------------------------------------------------------
#
# The pipeline's *_grid configs are production artefacts, and for xgb in particular
# they tune the wrong things relative to both reference studies:
#
#   Hanauer & Kalsbach (2023), Appendix C.5, GBRT : learning rate, n trees, depth
#   Rubesam (2021), boosting                      : learning rate, n iterations, splits
#   pipeline xgb_grid                             : FIXES nrounds at 500, and tunes
#                                                   min_child_weight, colsample and
#                                                   alpha, which neither paper tunes
#
# So the pipeline fixes the one hyperparameter both papers tune and tunes three they
# do not. The overrides below restore the literature's three-parameter boosting
# design, hold the rest at conventional defaults, and stay inside the papers' ranges
# while trimming for a panel far smaller than theirs.
#
# For random forest, Hanauer & Kalsbach tune the sampled-characteristic share, tree
# count and depth; Rubesam fixes trees at 1,000 and tunes only mtry, on the grounds
# that forests do not overfit in the number of trees. Neither tunes min.bucket.
#
# For neural nets, Hanauer & Kalsbach tune only l1 and the learning rate, fixing
# batch size and epochs and leaving early stopping to govern training length. The
# pipeline's grid reaches outside their ranges at both ends (l1 = 1e-2 above their
# 1e-3 ceiling; lr = 1e-4 below their 1e-3 floor).
#
# glmnet is left untouched: it is cheap (21 fits/date) and its `lambda.min.ratio`
# parameterises a lambda path rather than lambda itself, so it is not directly
# comparable to their elastic-net grid.
#
# Set to NULL to run the pipeline's grids unmodified.
GRID_OVERRIDES <- list(
  xgb_grid = list(                        # 4 x 3 x 3 = 36 fits/date (was 72)
    max_depth        = c(1, 2, 4, 6),     # H&K D: [1..8]; shallow suits a small panel
    eta              = c(0.01, 0.05, 0.10),  # H&K LR: [0.01..0.1]
    nrounds          = c(100, 250, 500),  # H&K T: [50..500] -- tuned, not fixed
    min_child_weight = 1,                 # fixed: tuned by neither paper
    subsample        = 0.8,
    colsample_bytree = 0.8,
    alpha            = 0,
    gamma            = 0
  ),
  rf_grid = list(                         # 4 x 1 x 4 x 1 = 16 fits/date (was 48)
    mtry       = c(0.25, 0.50, 0.75, 1.00),
    num.trees  = 500,                     # fixed, per Rubesam
    max.depth  = c(2, 4, 6, 8),           # H&K D: [1..8]; pipeline's 10 exceeds it
    min.bucket = 5                        # fixed: tuned by neither paper
  ),
  nn1_grid = list(                        # 3 x 3 = 9 fits/date (was 36)
    regularizer_l1   = c(1e-05, 1e-04, 1e-03),  # H&K l1: [1e-5..1e-3]
    lr               = c(0.001, 0.01, 0.10),    # H&K LR: [0.001, 0.1]
    regularizer_l2   = 0,
    droprate         = 0.25,
    size_of_batch    = 256,
    number_of_epochs = 100
  )
)
# nn2/nn3 share nn1's grid.
GRID_OVERRIDES$nn2_grid <- GRID_OVERRIDES$nn1_grid
GRID_OVERRIDES$nn3_grid <- GRID_OVERRIDES$nn1_grid

# Bayesian-opt constants ------------------------------------------------------
#
# factoRverse 0.7.0 allows a hyperparameter to be declared constant under
# bayesian_opt, which removes it from the Gaussian process's input space
# instead of leaving it there as a wasted dimension.
#
# What the reference studies actually tune (HYPERPARAMETER_GRID_COMPARISON.md):
#   RF       GKX depth+trees; H&K trees+depth+feature share; Rubesam depth+
#            features, trees FIXED at 300           -> mtry, max.depth
#   Boosting GKX/H&K trees+LR+depth; Rubesam iterations+splits, LR FIXED
#                                                   -> nrounds, eta, max_depth
#   NN       GKX/H&K l1+LR; Rubesam neither, early stopping only
#                                                   -> regularizer_l1, lr
#   Enet     GKX/H&K lambda+alpha                   -> both already searched
#
# NO source tunes min.bucket, min_child_weight, subsample, colsample_bytree,
# alpha/gamma, regularizer_l2, droprate, batch size or epochs; GKX and H&K
# explicitly FIX batch size and epochs. Each constant below is pinned to the
# value the grid arm already fixes, so the two arms tune an identical parameter
# set and the grid-vs-bayes contrast isolates the search method rather than
# confounding it with a different model class.
#
# Secondary effect: the GP is fitted in 2-3 dimensions instead of 4-8. Fitting a
# surrogate in 8-D from ~48 points is the lengthscale-failure regime recorded in
# challenger_version4/BAYESIAN_OPT_TRAINING_NOTES.md. gsPoints, which defaults
# to pmax(100, n_searched^3), drops from 512 to 100 for xgb and 216 to 100 for
# the NNs. The number of model fits is unchanged -- n_iter governs that.
#
# Set to NULL to search every hyperparameter, as before 0.7.0.
BAYES_CONSTANTS <- list(
  rf_bayes  = list(num.trees = 500L, min.bucket = 5),
  xgb_bayes = list(min_child_weight = 1, subsample = 0.8,
                   colsample_bytree = 0.8, alpha = 0, gamma = 0),
  nn1_bayes = list(regularizer_l2 = 0, droprate = 0.25,
                   size_of_batch = 256L, number_of_epochs = 100L)
)
# nn2/nn3 pin the same set as nn1.
BAYES_CONSTANTS$nn2_bayes <- BAYES_CONSTANTS$nn1_bayes
BAYES_CONSTANTS$nn3_bayes <- BAYES_CONSTANTS$nn1_bayes

# Bayes bounds
BAYES_BOUNDS <- list(
  nn1_bayes = list(
    regularizer_l1 = c(1e-5, 1e-3),
    lr             = c(1e-3, 1e-1)
  )
)

BAYES_BOUNDS$nn2_bayes <- BAYES_BOUNDS$nn1_bayes
BAYES_BOUNDS$nn3_bayes <- BAYES_BOUNDS$nn1_bayes

# A pinned entry is list(distribution_choice = "constant", value = <scalar>);
# a searched one is a length-2 numeric c(lower, upper).
.is_bayes_constant <- function(h) {
  is.list(h) && identical(h$distribution_choice, "constant")
}

# xgb_bayes searches nrounds over c(200, 1000) upstream. With early_stop = 25 on
# its tuning strategy the refit does NOT use the tuned nrounds -- it uses
# optimal_hyper["best_iteration"] (utils-fit_sb_model.R:187) -- and the grid
# arm's checkpoint measured best_iteration at 3-130 against caps of 100-500, so
# nrounds is non-binding in practice.
#
# This was previously narrowed to c(200, 300) purely to cut the cost of an
# 8-dimensional search (xgb spent 6h without finishing the first of 15 rebalance
# dates, against 1h23m for rf_bayes over all 15). BAYES_CONSTANTS now pins five
# of those eight, so that cost pressure is gone. The bound is instead matched to
# the span the grid arm enumerates -- {100, 250, 500} -- so both arms search the
# same nrounds range and differ only in HOW they search it.
# Set to NULL to restore production's c(200, 1000).
XGB_NROUNDS_BOUNDS <- c(100L, 500L)

# The paper's machine predicts cumulative 3m active return, not the geometric
# mean. gold_aux ships the geometric target; the cumulative one is derived.
# gold_aux ships ONLY fwd_act_geom_mean_ret_3m, which despite the "3m" suffix is a
# per-DAY rate: process_fwd_returns_tables.R:218 recovers a cumulative return as
# ((1 + geom_mean/100)^n_trading_days - 1)*100. The paper predicts the cumulative
# three-month active return instead, so the column is derived below and the target
# is rewritten on BOTH the base learners (via patch_sb_configs) and the meta-blend
# (via the inline ew_meta config in the mb stage). Setting this constant alone is
# not enough -- three places have to agree, which is why an earlier version of this
# script silently kept training on the geometric mean.
ML_TARGET <- "fwd_act_cum_ret_3m"   # or "fwd_act_geom_mean_ret_3m"

# Trading days per quarter used to compound the daily rate. Flat 63 reproduces
# paper/man-vs-machine exactly. It is an approximation: the pipeline's own 1m
# conversion uses the realised n_trading_days per period, but that column is joined
# only to fwd_1m_returns_m_df and is absent from the 3m target table.
TARGET_TRADING_DAYS <- 63

# (d) Meta portfolios
META_PORT_CONFIGS <- c("rp", "rp_box", "hrp", "cs", "sw", "ew", "mvo", "mvo_box")

# Per-mandate settings. `and_rule` names the element of gold_aux_tables holding
# the universe restriction (NULL = benchmark universe only).
META_MANDATES <- list(
  ibov = list(benchmark = "ibov", and_rule = NULL,
              eligibility_quantile_range = c(0.67, 1), strategy_aum = 150e6),
  smll = list(benchmark = "smll", and_rule = "small_cap_rule_m_df",
              eligibility_quantile_range = c(0.67, 1), strategy_aum = 50e6),
  isee = list(benchmark = "isee", and_rule = "esg_rule_m_df",
              eligibility_quantile_range = c(0.50, 1), strategy_aum = 25e6),
  # IDIV: dividend-payer universe, IDIV as construction benchmark only.
  idiv = list(benchmark = "idiv", and_rule = "dividends_rule_m_df",
              eligibility_quantile_range = c(0.67, 1), strategy_aum = 50e6)
)

# FALSE => chosen_scaler / scaler_m_df / scaler_shrinkage all NULL, so
# exp_ret_score == exp_ret_score_raw. `use_raw_for_eligibility` must then also be
# NULL (factoRverse validity rule, class_definitions.R:3376) -- which is moot,
# because with no scaler the raw and scaled scores are identical.
USE_SCALER <- FALSE

TC_PARS_COMMON <- list(direct_transaction_cost = 0.07, alpha = 0.5,
                       lambda = "dynamic")
SIGNIFICANCE_THRESHOLD <- 0.05
SEED     <- 20250618

# Parallelism -----------------------------------------------------------------
# Keep these FALSE. backtest_signal_selection() parallelises with furrr over its
# two dataset groups, and the anonymous worker closure is enclosed by the
# function's own frame -- which at that point still holds the gold panels, both
# extracted return matrices and the prepared aux tables. future measures the
# export at ~20.6 GiB and refuses to launch (default cap 8 GiB, settable via
# FUTURE_GLOBALS_MAXSIZE_GB).
#
# Raising the cap is the wrong fix: it would copy ~20 GiB into each of the 2
# workers. The stage only has two groups to split, and the frequentist BH path is
# cheap, so sequential is faster in wall-clock than the serialisation alone would
# be. Production gets away with it only because it runs under targets with a
# different memory profile and a raised env var.
#
# PARALLEL_MVO has the same exposure inside run_port_backtest's resampling loop
# and is off by default; flip it only if the meta stage proves slow AND memory
# headroom has been checked.
PARALLEL_SS  <- FALSE
PARALLEL_MVO <- FALSE

OUT_DIR <- here::here("data", "derived", "machine_run")

# (e) Named reruns -- see runs/README.md. With PAPER_RUN unset nothing below
# changes anything. With PAPER_RUN = "<name>", runs/<name>.R is applied on top of
# the settings above, outputs and checkpoints go to data/runs/<name>/, and
# BASE_DIR (the submitted results) is only ever read.
BASE_DIR       <- OUT_DIR
CHECKPOINT_DIR <- here::here("data", "checkpoints")
RUN_NAME       <- Sys.getenv("PAPER_RUN", "")
RUN_FILE       <- NULL
RERUN_FROM     <- NULL  # first stage to recompute; earlier ones come from BASE_DIR
RERUN_LEARNERS <- NULL  # at "sb": learners to refit; the others come from BASE_DIR

if (nzchar(RUN_NAME)) {
  if (!grepl("^[A-Za-z0-9_-]+$", RUN_NAME)) {
    stop("PAPER_RUN may contain only letters, digits, '_' and '-'.")
  }
  RUN_FILE <- here::here("runs", paste0(RUN_NAME, ".R"))
  source(RUN_FILE, local = TRUE, encoding = "UTF-8")
  OUT_DIR        <- here::here("data", "runs", RUN_NAME)
  CHECKPOINT_DIR <- file.path(OUT_DIR, "checkpoints")
}
options(mvm.checkpoint_root = CHECKPOINT_DIR)


# ---- 2. ENVIRONMENT ---------------------------------------------------------

stopifnot(dir.exists(HELPERS_DIR), dir.exists(INPUTS_DIR))

# Bind the Python backend BEFORE anything can initialise it. reticulate binds on
# first use and cannot rebind within a session, so this must precede library()
# calls and any tf$/keras call.
.project_python <- function() {
  f <- here::here(".Renviron")
  from_env <- Sys.getenv("RETICULATE_PYTHON", NA_character_)
  if (!file.exists(f)) return(from_env)
  ln <- grep("^\\s*RETICULATE_PYTHON\\s*=", readLines(f, warn = FALSE), value = TRUE)
  if (length(ln) == 0) return(from_env)
  trimws(sub("^\\s*RETICULATE_PYTHON\\s*=", "", ln[1]))
}

.py <- if (!is.null(RETICULATE_PYTHON_OVERRIDE)) {
  RETICULATE_PYTHON_OVERRIDE
} else {
  .project_python()
}

if (!is.na(.py) && nzchar(.py) && file.exists(.py)) {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      isTRUE(reticulate::py_available(initialize = FALSE))) {
    # Already bound -- verify it is the right one rather than silently proceeding.
    .bound <- tryCatch(reticulate::py_config()$python, error = function(e) NA_character_)
    if (!is.na(.bound) &&
        !identical(normalizePath(.bound, "/", FALSE),
                   normalizePath(.py, "/", FALSE))) {
      stop("Python is already bound to\n  ", .bound,
           "\nbut this run needs\n  ", .py,
           "\nreticulate cannot rebind within a session -- restart R and source again.")
    }
  } else {
    Sys.setenv(RETICULATE_PYTHON = .py)
    message("RETICULATE_PYTHON <- ", .py)
  }
} else {
  warning("Could not resolve the project's Python interpreter. Neural-network ",
          "stages will use reticulate's default environment, which may carry ",
          "Keras 3 and is incompatible with factoRverse's Keras 2 code.")
}

library(dplyr)
library(factoRverse)

# NN_ENSEMBLES and BAYES_CONSTANTS both use API introduced in 0.7.0 -- the
# n_ensembles slot and constant hyperparameters under bayesian_opt. On an older
# build the constant declaration fails validObject() with a message about bounds
# that gives no hint of the real cause, so fail here instead, with one.
if (utils::packageVersion("factoRverse") < "0.7.0") {
  stop("This script needs factoRverse >= 0.7.0 (installed: ",
       utils::packageVersion("factoRverse"), "). Set NN_ENSEMBLES <- 1L and ",
       "BAYES_CONSTANTS <- NULL to run it against an older build.")
}

# The pipeline functions this paper uses, copied into this repo at PIPELINE_COMMIT.
invisible(lapply(
  list.files(HELPERS_DIR, pattern = "\\.R$", full.names = TRUE),
  source
))

`%||%` <- function(x, y) if (is.null(x)) y else x

STAGES <- c("screen", "probe", "ss", "sb", "mb", "meta", "learners")
stopifnot(STOP_AFTER %in% STAGES)
if (nzchar(RUN_NAME)) {
  if (length(RERUN_FROM) != 1L || !RERUN_FROM %in% STAGES) {
    stop("runs/", RUN_NAME, ".R must set RERUN_FROM to one of: ",
         paste(STAGES, collapse = ", "))
  }
  if (!is.null(RERUN_LEARNERS) &&
      (!identical(RERUN_FROM, "sb") || !all(RERUN_LEARNERS %in% SB_CONFIGS))) {
    stop("RERUN_LEARNERS needs RERUN_FROM = 'sb' and names taken from SB_CONFIGS: ",
         paste(SB_CONFIGS, collapse = ", "))
  }
}


# ---- 2b. BAYESIAN-OPTIMIZATION BUDGET ---------------------------------------
#
# Source: research/challenger_version4/BAYESIAN_OPT_TRAINING_NOTES.md on branch
# `challenger_version4`, which logs a real sweep of these exact configs.
#
# What the notes establish:
#   * Model fits per rebalance date = init_points + n_iter, run fresh at every
#     date -- the tuner is never warm-started across dates.
#   * Epochs = ceiling(n_iter / k_iter). EACH epoch pays one GP fit plus one
#     acquisition-function optimization, and those two steps are the entire
#     failure surface (notes sections 2a-2d). Epoch count, not evaluation count,
#     is what production is actually spending.
#   * When the GP fails to fit, the tuner falls back to random sampling but pays
#     the failed fit and three retries first. Cost scales brutally with
#     dimensionality: ~10-20s at 2 hyperparameters, ~1,500-1,840s at 8.
#   * The fallback is common, not exceptional. nn1_bayes/cum_ret hit it on 36 of
#     65 epochs on 2018-06-15; nn1_bayes/xs_z on 28 of 65 on 2015-06-15.
#   * ParBayesianOptimization's own stopping rule ("Could not meet minimum
#     required (0) utility") fires on several dates -- the optimizer itself
#     concluding the remaining budget buys nothing.
#
# The diagnosis: production's budget is not overtrained in evaluations so much as
# misallocated across epochs. nn1/nn2/nn3 use k_iter = 1, so 65-75 adaptive
# evaluations cost 65-75 GP fits -- the worst possible ratio. And because a large
# fraction of those epochs fall back to random sampling anyway, the nominal
# "65 GP-guided epochs" is really ~29 guided plus ~36 random. Raising k_iter
# keeps the real model fits GP-proposed while collapsing the GP overhead and the
# number of chances to trip the pathology.
#
# Per rebalance date, summed over the six tuned models:
#   production: 416 evaluations, 247 epochs
#   balanced  : 232 evaluations,  17 epochs   (-44% evals, -93% epochs)
#   lean      : 154 evaluations,  12 epochs   (-63% evals, -95% epochs)
#
# NOTE on how far this can be pushed. Setting k_iter == n_iter (one GP fit per
# date) would cut the serial phase further, but it is not defensible: batch BO
# (Ginsbourger q-EI, Desautels GP-BUCB, Gonzalez local penalisation) is motivated
# by PARALLEL evaluation, and these candidates are scored sequentially -- so it
# takes the batch penalty without the wall-clock benefit that justifies it. It
# would also make the surrogate maximally load-bearing while fitting it on the
# least data. If xgb needs to be faster, enlarge init_points instead: a GP over 8
# hyperparameters conditioned on 12 points is badly under-determined, which is
# exactly the "Could not obtain meaningful lengthscales" failure the
# challenger-v4 notes record. More random design, fewer guided epochs.
#
# init_points must exceed the hyperparameter count. That rule is enforced inside
# add_hyperparameter(), NOT in class validity (notes section 1), so writing the
# slot directly bypasses it -- patch_tuning() re-checks it below.

TUNING_BUDGETS <- list(
  # model         dim  init  n_iter  k_iter   evals  evals/dim  epochs
  balanced = list(
    glmnet_bayes = c(init_points =  8, n_iter = 16, k_iter =  8), # 2  24  12.0  2
    rf_bayes     = c(init_points = 10, n_iter = 30, k_iter = 10), # 4  40  10.0  3
    xgb_bayes    = c(init_points = 12, n_iter = 36, k_iter = 12), # 8  48   6.0  3
    nn1_bayes    = c(init_points = 10, n_iter = 30, k_iter = 10), # 6  40   6.7  3
    nn2_bayes    = c(init_points = 10, n_iter = 30, k_iter = 10), # 6  40   6.7  3
    nn3_bayes    = c(init_points = 10, n_iter = 30, k_iter = 10)  # 6  40   6.7  3
  ),
  lean = list(
    glmnet_bayes = c(init_points =  6, n_iter = 12, k_iter =  6), # 2  18   9.0  2
    rf_bayes     = c(init_points =  8, n_iter = 16, k_iter =  8), # 4  24   6.0  2
    xgb_bayes    = c(init_points = 10, n_iter = 30, k_iter = 15), # 8  40   5.0  2
    nn1_bayes    = c(init_points =  8, n_iter = 16, k_iter =  8), # 6  24   4.0  2
    nn2_bayes    = c(init_points =  8, n_iter = 16, k_iter =  8), # 6  24   4.0  2
    nn3_bayes    = c(init_points =  8, n_iter = 16, k_iter =  8)  # 6  24   4.0  2
  ),
  production = list()
)
stopifnot(TUNING_PRESET %in% names(TUNING_BUDGETS))

patch_sb_configs <- function(cfgs, training_sample_size = NULL,
                             validation_sample_size = NULL) {

  budgets <- TUNING_BUDGETS[[TUNING_PRESET]]

  for (i in seq_along(cfgs)) {
    cfg <- cfgs[[i]]
    if (!methods::is(cfg, "sb_backtest_config")) next
    nm <- cfg@config_name

    # nn2 architecture repair -- see FIX_NN2_ARCHITECTURE.
    if (FIX_NN2_ARCHITECTURE && identical(nm, "nn2_bayes")) {
      cfg <- factoRverse::add_keras_architecture(
        cfg, nn_optimizer = "Adam", units = c(32L, 16L),
        activation = rep("relu", 2), batch_norm_option = rep(TRUE, 2)
      )
      message("  nn2_bayes: architecture set to units = c(32, 16)")
    }

    # NN ensembling -- see NN_ENSEMBLES. Placed after the nn2 repair so the
    # rebuilt architecture carries it too (add_keras_architecture() defaults
    # n_ensembles to 1 when the argument is absent). Refit-only: tuning always
    # fits a single network, so the search cost is unchanged.
    if (NN_ENSEMBLES > 1L && identical(cfg@sb_algorithm, "nn")) {
      kap <- cfg@keras_architecture_parameters
      if (is.null(kap)) {
        stop(nm, " is an nn config but carries no keras_architecture_parameters.")
      }
      kap@n_ensembles <- as.numeric(NN_ENSEMBLES)
      cfg@keras_architecture_parameters <- kap
      message(sprintf("  %-13s n_ensembles -> %d", nm, NN_ENSEMBLES))
    }

    # Retarget every base learner. create_sb_configs hardcodes
    # target_fwd_name = "fwd_act_geom_mean_ret_3m" on all of them
    # (backtest_base_signal_blending.R:951+), with no argument to override it.
    if (!identical(cfg@target_fwd_name, ML_TARGET)) {
      cfg@target_fwd_name <- ML_TARGET
      message("  ", nm, ": target -> ", ML_TARGET)
    }

    # nn1_grid hardcodes training_sample_size = 120 and validation_sample_size = 60
    # (backtest_base_signal_blending.R:1266/1274) while nn2_grid and nn3_grid take the
    # passed values. Left alone, NN1's out-of-sample window would start 120 + 60 = 180
    # against 82 + 36 = 118 for every other learner -- 62 months later, silently, and
    # only at the rung the H7 complexity ladder is most sensitive to.
    if (identical(nm, "nn1_grid") &&
        !is.null(training_sample_size) && !is.null(validation_sample_size)) {
      if (!identical(cfg@training_sample_size, training_sample_size)) {
        message(sprintf("  %-13s training_sample_size: %s -> %s (hardcoded upstream)",
                        nm, cfg@training_sample_size, training_sample_size))
        cfg@training_sample_size <- training_sample_size
      }
      ts0 <- cfg@tuning_strategy
      if (!is.null(ts0) && !identical(ts0@validation_sample_size, validation_sample_size)) {
        message(sprintf("  %-13s validation_sample_size: %s -> %s (hardcoded upstream)",
                        nm, ts0@validation_sample_size, validation_sample_size))
        ts0@validation_sample_size <- validation_sample_size
        cfg@tuning_strategy <- ts0
      }
    }

    # Tuning-metric arm -- see .EVAL_METRIC_ARMS. Applied to every config in
    # SB_CONFIGS (the ML pool this paper machine dispatches) regardless of
    # tuning_method, so the grid and bayes arms can each be run under any
    # metric. Gated on SB_CONFIGS, NOT on every cfg in cfgs: create_sb_configs()
    # returns the pipeline's FULL base-learner pool, which also includes
    # heuristic configs (e.g. sw_lt_perf) carrying a heuristic custom_objective
    # like "max_lt_perf_agg_z_score". Without this gate, the EVAL_OBJECTIVE
    # overwrite below hits those too and fails class validity, which requires
    # 'max_'/'min_' + a heuristic metric name for that config type.
    if (nm %in% SB_CONFIGS) {
      tsm <- cfg@tuning_strategy
      if (!is.null(tsm) && !identical(tsm@chosen_eval_metric, EVAL_METRIC)) {
        message(sprintf("  %-13s chosen_eval_metric: %s -> %s",
                        nm, tsm@chosen_eval_metric, EVAL_METRIC))
        tsm@chosen_eval_metric <- EVAL_METRIC
        cfg@tuning_strategy    <- tsm
      }
      # Set on the config even when unchanged: quantile_tau and huber_delta are
      # read by calculate_eval_metrics() at scoring time, so the value stored
      # here is the one the reported metric was computed under.
      if (!identical(cfg@quantile_tau, EVAL_TAU)) {
        message(sprintf("  %-13s quantile_tau: %s -> %s",
                        nm, cfg@quantile_tau, EVAL_TAU))
        cfg@quantile_tau <- EVAL_TAU
      }
      if (!identical(cfg@huber_delta, EVAL_DELTA)) {
        message(sprintf("  %-13s huber_delta: %s -> %s",
                        nm, cfg@huber_delta, EVAL_DELTA))
        cfg@huber_delta <- EVAL_DELTA
      }
      # On the mpe arm this is a no-op (objective stays "squared_error") and the
      # arm isolates selection. On the mphe arm it changes the TRAINING loss for
      # xgb and nn1-nn3 only. This is gated on sb_algorithm, not just left to
      # translate_metrics() returning NULL for ols/glmnet/rf: class validity
      # forbids those three from carrying ANY non-default custom_objective at
      # all ("Custom objectives are only allowed for 'sw', 'rp', 'hrp', 'mvo',
      # 'mmaf', 'xgb' or 'nn' algorithms") -- assigning "pseudo_huber_error" to
      # an ols/glmnet/rf config fails validObject(), it doesn't silently no-op.
      # So ols/glmnet/rf move through selection only, exactly as intended, but
      # by never touching the slot rather than by the translation being inert.
      if (cfg@sb_algorithm %in% c("xgb", "nn") &&
          !identical(cfg@custom_objective, EVAL_OBJECTIVE)) {
        message(sprintf("  %-13s custom_objective: %s -> %s",
                        nm, cfg@custom_objective, EVAL_OBJECTIVE))
        cfg@custom_objective <- EVAL_OBJECTIVE
      }
    }

    # Grid realignment -- see GRID_OVERRIDES.
    if (!is.null(GRID_OVERRIDES) && !is.null(GRID_OVERRIDES[[nm]])) {
      tsg <- cfg@tuning_strategy
      if (is.null(tsg) || !identical(tsg@tuning_method, "grid_search")) {
        stop("GRID_OVERRIDES names '", nm, "' but it is not a grid_search config.")
      }
      hgd  <- tsg@hyper_grid_domain
      hl   <- hgd@hyperparameter_list
      new  <- GRID_OVERRIDES[[nm]]
      unknown <- setdiff(names(new), names(hl))
      if (length(unknown) > 0) {
        stop("GRID_OVERRIDES[['", nm, "']] names hyperparameters absent from the ",
             "config: ", paste(unknown, collapse = ", "))
      }
      before <- prod(vapply(hl, length, numeric(1)))
      for (h in names(new)) hl[[h]] <- new[[h]]
      hgd@hyperparameter_list <- hl
      tsg@hyper_grid_domain   <- hgd
      cfg@tuning_strategy     <- tsg
      after <- prod(vapply(hl, length, numeric(1)))
      message(sprintf("  %-13s grid realigned | fits/date %3.0f -> %3.0f", nm, before, after))
    }

    # Pin the hyperparameters no reference study tunes -- see BAYES_CONSTANTS.
    # Applied BEFORE the budget block so the init_points guard below sees the
    # searched dimension, and before the nrounds block, which touches a
    # searched parameter and so cannot collide with anything pinned here.
    if (!is.null(BAYES_CONSTANTS) && !is.null(BAYES_CONSTANTS[[nm]])) {
      tsc <- cfg@tuning_strategy
      if (is.null(tsc) || !identical(tsc@tuning_method, "bayesian_opt")) {
        stop("BAYES_CONSTANTS names '", nm, "' but it is not a bayesian_opt config.")
      }
      hgc <- tsc@hyper_grid_domain
      hlc <- hgc@hyperparameter_list
      pin <- BAYES_CONSTANTS[[nm]]
      unknown <- setdiff(names(pin), names(hlc))
      if (length(unknown) > 0) {
        stop("BAYES_CONSTANTS[['", nm, "']] names hyperparameters absent from ",
             "the config: ", paste(unknown, collapse = ", "))
      }
      for (h in names(pin)) {
        hlc[[h]] <- list(distribution_choice = "constant", value = pin[[h]])
      }
      hgc@hyperparameter_list <- hlc
      tsc@hyper_grid_domain   <- hgc
      cfg@tuning_strategy     <- tsc
      searched <- names(hlc)[!vapply(hlc, .is_bayes_constant, logical(1))]
      message(sprintf("  %-13s pinned %d | searches %d: %s", nm,
                      length(pin), length(searched),
                      paste(searched, collapse = ", ")))
    }
    
    if (!is.null(BAYES_BOUNDS[[nm]])) {
      ts  <- cfg@tuning_strategy
      hgd <- ts@hyper_grid_domain
      new <- BAYES_BOUNDS[[nm]]
      
      unknown <- setdiff(names(new), names(hgd@hyperparameter_list))
      if (length(unknown) > 0) {
        stop("BAYES_BOUNDS[['", nm, "']] has unknown hyperparameters: ",
             paste(unknown, collapse = ", "))
      }
      
      for (h in names(new)) {
        hgd@hyperparameter_list[[h]] <- new[[h]]
      }
      
      ts@hyper_grid_domain <- hgd
      cfg@tuning_strategy  <- ts
    }

    ts <- cfg@tuning_strategy
    if (!is.null(ts) && identical(ts@tuning_method, "bayesian_opt") &&
        !is.null(budgets[[nm]])) {

      b     <- budgets[[nm]]
      # 0.7.0 requires init_points to exceed the number of SEARCHED
      # hyperparameters, not the number declared, so pinned constants must
      # not count towards it. BAYES_CONSTANTS is applied just above.
      n_dim <- sum(!vapply(ts@hyper_grid_domain@hyperparameter_list,
                           .is_bayes_constant, logical(1)))

      if (b[["init_points"]] <= n_dim) {
        stop("init_points (", b[["init_points"]], ") must exceed the ", n_dim,
             " hyperparameters of ", nm, ".")
      }
      if (b[["k_iter"]] > b[["n_iter"]]) {
        stop("k_iter must be <= n_iter for ", nm, ".")
      }

      old <- c(ts@init_points, ts@n_iter, ts@k_iter)
      ts@init_points <- b[["init_points"]]
      ts@n_iter      <- b[["n_iter"]]
      ts@k_iter      <- b[["k_iter"]]
      cfg@tuning_strategy <- ts

      message(sprintf(
        "  %-13s init/n_iter/k: %2.0f/%2.0f/%2.0f -> %2.0f/%2.0f/%2.0f | evals %3.0f -> %3.0f | epochs %3.0f -> %2.0f",
        nm, old[1], old[2], old[3],
        b[["init_points"]], b[["n_iter"]], b[["k_iter"]],
        old[1] + old[2], b[["init_points"]] + b[["n_iter"]],
        ceiling(old[2] / old[3]), ceiling(b[["n_iter"]] / b[["k_iter"]])
      ))
    }

    # Narrow xgb's nrounds search -- see XGB_NROUNDS_BOUNDS. Done outside the
    # budget block so it applies regardless of TUNING_PRESET. Bounds live in
    # tuning_strategy@hyper_grid_domain@hyperparameter_list as c(lower, upper);
    # the entry is integer and must stay integer.
    if (!is.null(XGB_NROUNDS_BOUNDS) && identical(nm, "xgb_bayes")) {
      ts2 <- cfg@tuning_strategy
      hgd <- ts2@hyper_grid_domain
      if (!is.null(hgd@hyperparameter_list$nrounds)) {
        old_b <- hgd@hyperparameter_list$nrounds
        hgd@hyperparameter_list$nrounds <- as.integer(XGB_NROUNDS_BOUNDS)
        ts2@hyper_grid_domain <- hgd
        cfg@tuning_strategy   <- ts2
        message(sprintf("  %-13s nrounds bounds: %d..%d -> %d..%d",
                        nm, old_b[1], old_b[2],
                        XGB_NROUNDS_BOUNDS[1], XGB_NROUNDS_BOUNDS[2]))
      } else {
        warning("xgb_bayes has no `nrounds` hyperparameter; cap not applied.")
      }
    }

    methods::validObject(cfg)
    cfgs[[i]] <- cfg
  }
  cfgs
}

# Shadowing pipeline helpers -------------------------------------------------
#
# Several pipeline helpers are replaced below. They were source()d into the global
# environment in section 2, so the pipeline functions resolve the replacements
# there. Nothing in the pipeline repo is modified.
#
# These MUST be installed unconditionally on every source(). Section 2 re-sources
# the helper files each time, which restores the pipeline originals -- so an
# `if (!exists(...))` guard would silently leave the originals in place on the
# second source() in a live R session. That is exactly the failure that let
# update_workflow_with_versions() reach the production board again, nine minutes
# into the SB stage. main() now asserts the shadows are live before doing any work.

.mark_shadow <- function(f) { attr(f, "paper_run_shadow") <- TRUE; f }

.capture_original <- function(name) {
  f <- get(name, envir = globalenv())
  if (isTRUE(attr(f, "paper_run_shadow"))) {
    stop("`", name, "` is already a paper-run shadow -- the pipeline helpers were ",
         "not re-sourced before this block. Restart R and source the script again.")
  }
  f
}

.PAPER_RUN_SHADOWS <- c("create_sb_configs", "update_workflow_with_versions")

# patch_sb_configs() is a no-op when TUNING_PRESET is "production" and
# FIX_NN2_ARCHITECTURE is FALSE, so this is always safe to install.
.create_sb_configs_pipeline <- .capture_original("create_sb_configs")
create_sb_configs <- .mark_shadow(function(...) {
  args <- list(...)
  message("\nPatching SB configs (preset: ", TUNING_PRESET, ")")
  patch_sb_configs(
    .create_sb_configs_pipeline(...),
    # forwarded so the nn1_grid hardcoding can be repaired against the real values
    training_sample_size   = args$training_sample_size,
    validation_sample_size = args$validation_sample_size
  )
})

# Pinned gold_results element names differ between the initial and update
# workflows (`*_initial` vs `*_refreshed`); the pipeline resolves this by regex
# in deploy_gold_m_df.R:282, and so do we.
pluck_by_name <- function(lst, pattern) {
  pos <- which(stringr::str_detect(names(lst), pattern))
  if (length(pos) != 1L) {
    stop("Expected exactly one element matching '", pattern, "' in: ",
         paste(names(lst), collapse = ", "))
  }
  lst[[pos]]
}

git_hash_of <- function(dir) {
  out <- try(system2("git", c("-C", shQuote(dir), "rev-parse", "--short", "HEAD"),
                     stdout = TRUE, stderr = FALSE), silent = TRUE)
  if (inherits(out, "try-error") || length(out) == 0) NA_character_ else out[1]
}

# Pre-flight: can the pipeline actually DISPATCH every config in SB_CONFIGS?
#
# The pipeline keeps three lists that must agree -- the config builders, the
# allowable-config check, and `sb_registry` (the dispatch table in
# run_all_sb_backtests). Only sb_registry is hand-maintained. A config missing
# from it is still built and still passes the allowable check, and is then
# skipped by the run loop with NO backtest and NO log line. That is not
# hypothetical: with the pipeline checked out at a commit lacking the nn*_grid
# entries, a grid run fitted ols/glmnet/rf/xgb over ~64 minutes and only failed
# at the very end, on the base-learner count, with nothing in the log to say the
# three NN models had never run.
#
# Checked here against the pipeline SOURCE rather than a branch name, so this
# stays correct wherever the fix lives. Costs about a second.
local({
  reg_file <- file.path(HELPERS_DIR, "backtest_base_signal_blending.R")
  if (!file.exists(reg_file)) {
    warning("Pipeline backtest helper not found; dispatch pre-flight skipped.")
    return(invisible(NULL))
  }
  pat        <- 'list\\(\\s*name\\s*=\\s*"'
  src        <- readLines(reg_file, warn = FALSE)
  registered <- unique(sub(paste0(".*", pat, '([^"]+)".*'), "\\1",
                           grep(pat, src, value = TRUE)))
  if (length(registered) == 0L) {
    # Formatting drifted past the pattern. Warn rather than block: a guard that
    # cannot read the registry must not stop a run that would otherwise work.
    warning("Could not parse sb_registry from ", basename(reg_file),
            "; dispatch pre-flight skipped.")
    return(invisible(NULL))
  }
  missing <- setdiff(SB_CONFIGS, registered)
  if (length(missing) > 0L) {
    stop("The pipeline cannot dispatch: ", paste(missing, collapse = ", "),
         "\n  registry : ", reg_file,
         "\n  These configs would be built, silently skipped, and the run would",
         " fail only after every other model had been fitted.",
         "\n  Add each one to `sb_registry` in that file.", call. = FALSE)
  }
  message(sprintf("Pipeline dispatch OK: %d/%d configs registered (%s @ %s)",
                  length(SB_CONFIGS), length(SB_CONFIGS),
                  "R/pipeline", substr(PIPELINE_COMMIT, 1, 7)))
})

# The manifest records pin locations as RELATIVE paths
# (versions_table$latest_version = "data/prod/<pin>/<version>", and
# manifest$board$path = "data/prod"). INPUTS_DIR mirrors that layout, so every
# load runs from there. Read-only; the directory is always restored, including
# on error.
load_pipeline_object <- function(manifest, object_name) {
  old_wd <- setwd(INPUTS_DIR)
  on.exit(setwd(old_wd), add = TRUE)
  load_old_object(manifest, object_name)
}


# ---- 2c. PROVENANCE STAMPING OVERRIDE ---------------------------------------
#
# The pipeline stamps each backtest's workflow slot with the pin version of every
# upstream object, reading them off the production board via
#   get_latest_version(here::here("data", "prod", "gold_<vintage>_<source>_..."))
# (tools.R:319-347). `here::here()` is pinned to the Man vs Machine project root,
# where no such board exists, so it errors with "No folders found in the specified
# path." setwd() does NOT help -- `here` fixes its root at load time and ignores
# the working directory afterwards.
#
# This is pure provenance metadata for a board this run never writes to, and it is
# called from all three remaining stages (backtest_signal_selection.R:372/382,
# backtest_base_signal_blending.R:1908, backtest_meta_portfolios.R:917), so it is
# shadowed once here rather than worked around per stage.
#
# The replacement keeps the original's structure and key names exactly, but sources
# versions from the manifest actually in use -- which is strictly more accurate than
# the original, since get_latest_version() returns whatever is newest on disk rather
# than what the manifest pinned. Objects produced by this run are marked as unpinned,
# and screened inputs are marked as screened, so the stamp never overstates
# comparability with production.

.PAPER_RUN <- new.env(parent = emptyenv())

.manifest_pin_version <- function(object_name) {
  m <- .PAPER_RUN$manifest
  if (is.null(m)) return(NA_character_)
  row <- m$versions_table[grepl(paste0("^", object_name), m$versions_table$object), ]
  if (nrow(row) != 1L) return(NA_character_)
  basename(row$latest_version)
}

.stamp <- function(object_name, suffix = "") {
  v <- .manifest_pin_version(object_name)
  if (is.na(v)) "paper_run__unresolved" else paste0(v, suffix)
}

.update_workflow_with_versions_pipeline <- .capture_original("update_workflow_with_versions")

update_workflow_with_versions <- .mark_shadow(
  function(workflow, object_name, current_date, source) {

    allowed <- c("signal_port_backtest_cohorts", "ss_backtest_results_bundles",
                 "mb_backtest_results_bundles", "meta_port_backtest_results_bundles")
    if (!object_name %in% allowed) {
      stop("object_name must be one of: ", paste(allowed, collapse = ", "))
    }
    if (!is.list(workflow)) stop("workflow must be a list.")

    is_cohort <- identical(object_name, "signal_port_backtest_cohorts")
    if (is_cohort) {
      n_workflows      <- 1L
      current_workflow <- workflow
    } else {
      n_workflows      <- length(workflow)
      current_workflow <- workflow[[n_workflows]]
    }

    if (!is.null(current_workflow$current_date) &&
        as.Date(current_workflow$current_date) != as.Date(current_date)) {
      stop("current_date in current_workflow does not match current_date argument.")
    }

    # gold_results is passed through untouched; gold_aux and the cohorts are screened.
    current_workflow$gold_results_version    <- .stamp("gold_results")
    current_workflow$gold_aux_tables_version <- .stamp("gold_aux_tables", "__screened")

    if (!is_cohort) {
      current_workflow$signal_port_backtest_cohorts_version <-
        .stamp("signal_port_backtest_cohorts", "__screened")
      if (object_name != "ss_backtest_results_bundles") {
        current_workflow$ss_backtest_results_bundles_version <- "paper_run__not_pinned"
        if (object_name != "mb_backtest_results_bundles") {
          current_workflow$mb_backtest_results_bundles_version <- "paper_run__not_pinned"
        }
      }
    }

    if (is_cohort) {
      workflow <- current_workflow
    } else {
      workflow[[n_workflows]] <- current_workflow
      if (length(workflow) != n_workflows) stop("Workflow length changed after update.")
    }
    workflow
  }
)


# ---- MAIN -------------------------------------------------------------------

main <- function() {

  # Fail in one second, not nine minutes. Every shadow from sections 2b/2c must be
  # live before any stage runs; if a helper re-source has clobbered one, stop here
  # rather than discovering it deep inside a long stage.
  missing_shadows <- Filter(
    function(n) !isTRUE(attr(get(n, envir = globalenv()), "paper_run_shadow")),
    .PAPER_RUN_SHADOWS
  )
  if (length(missing_shadows) > 0) {
    stop("Pipeline helper shadow(s) not installed: ",
         paste(missing_shadows, collapse = ", "),
         ". Restart R and source run_paper_machine.R again.")
  }
  message("shadows active: ", paste(.PAPER_RUN_SHADOWS, collapse = ", "))

  # Verify the Keras backend before spending hours reaching the NN stages.
  # factoRverse's fit_keras_model() targets the Keras 2 API; Keras 3 rejects it
  # with "Only input tensors may be passed as positional arguments", surfacing as
  # a misleading missing-object error. Only checked when an NN learner is
  # actually requested, since it forces Python initialisation.
  if (any(grepl("^nn", SB_CONFIGS))) {
    kv <- tryCatch(
      as.character(reticulate::py_get_attr(reticulate::import("keras"), "__version__")),
      error = function(e) NA_character_
    )
    if (is.na(kv)) {
      warning("Could not determine the Python Keras version; NN stages may fail.")
    } else {
      message("python keras: ", kv, "  (", reticulate::py_config()$python, ")")
      if (as.integer(sub("[.].*$", "", kv)) != 2L) {
        stop("Python Keras ", kv, " is bound, but factoRverse's fit_keras_model() ",
             "requires the Keras 2 API. Point RETICULATE_PYTHON at an interpreter ",
             "with keras 2.x (see environment/README.md) and restart R.")
      }
    }
  }

  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

  # A run's cached stages are valid only for the run file that produced them.
  if (nzchar(RUN_NAME)) {
    saved <- file.path(OUT_DIR, "run.R")
    if (!file.exists(saved)) {
      file.copy(RUN_FILE, saved)
    } else if (!identical(readLines(saved), readLines(RUN_FILE))) {
      stop("runs/", RUN_NAME, ".R changed after this run started. Give the new ",
           "specification a new name, so cached stages are not mixed.")
    }
  }

  # The grid and BO arms must not share cache files. screen/probe/ss are
  # tuning-independent and shared; sb/mb/meta are arm-specific and would otherwise
  # silently overwrite one another -- a later BO run would read the grid arm's
  # sb.qs and never retrain. Both arms coexist on disk, which is what a robustness
  # section needs.
  ARM <- if (all(grepl("_grid$", setdiff(SB_CONFIGS, "ols")))) {
    "grid"
  } else if (all(grepl("_bayes$", setdiff(SB_CONFIGS, "ols")))) {
    "bayes"
  } else {
    stop("SB_CONFIGS mixes _grid and _bayes learners; the arm is ambiguous and the ",
         "two tuning methods would not be comparable across the H7 ladder.")
  }
  # The tuning-metric arm is a SECOND, orthogonal dimension. ARM_TAG is what
  # names artefacts: "grid", "grid_mpe", "grid_mphe", "bayes", ... The baseline
  # tag is empty so existing sb_grid.qs / mb_grid.qs / meta_grid.qs keep their
  # names and remain valid; the metric arms write alongside rather than over.
  ARM_TAG <- paste0(ARM, EVAL_TAG)
  message("tuning arm: ", ARM, " | eval metric: ", EVAL_METRIC,
          if (identical(EVAL_METRIC, "mpe")) paste0(" (tau = ", EVAL_TAU, ")")
          else if (identical(EVAL_METRIC, "mphe")) paste0(" (delta = ", EVAL_DELTA, ")")
          else "",
          " | objective: ", EVAL_OBJECTIVE,
          " | artefact tag: ", ARM_TAG)

  stage_path <- function(name) {
    suffix <- if (name %in% c("sb", "mb", "meta", "learners")) paste0("_", ARM_TAG) else ""
    file.path(OUT_DIR, paste0(name, suffix, ".qs"))
  }

  run_stage <- function(name, expr) {
    path <- stage_path(name)
    if (file.exists(path)) {
      message("[skip] ", name, " -- reading ", path)
      return(qs::qread(path))
    }
    if (nzchar(RUN_NAME) && match(name, STAGES) < match(RERUN_FROM, STAGES)) {
      submitted <- file.path(BASE_DIR, basename(path))
      message("[reuse] ", name, " -- reading ", submitted)
      return(qs::qread(submitted))
    }
    message("\n==== stage: ", name, " ====")
    out <- force(expr)
    qs::qsave(out, path)
    message("[done] ", name, " -> ", path)
    out
  }

  stop_here <- function(name) {
    if (match(name, STAGES) >= match(STOP_AFTER, STAGES)) {
      message("\nSTOP_AFTER = '", STOP_AFTER, "'. Stopping after '", name, "'.")
      TRUE
    } else FALSE
  }

  # ---- 3. LOAD THE MANIFEST AND GOLD OBJECTS --------------------------------

  manifest_path <- get_latest_manifest_path(
    manifests_dir = file.path(INPUTS_DIR, "data", "prod", "manifests"),
    manifest_name = MANIFEST_NAME
  )
  manifest <- qs::qread(manifest_path)
  .PAPER_RUN$manifest <- manifest   # consumed by the section-2c workflow stamper

  message("Manifest: ", basename(manifest_path))
  message("  vintage git hash : ", manifest$git_hash)
  message("  factoRverse      : ", paste(manifest$factoRverse_semver, collapse = "."))
  message("  source           : ", manifest$source)
  message("  production winner: ", manifest$winner_ss_config, " / ",
          manifest$winner_dataset, " / ", manifest$winner_meta_port_config)

  # Do NOT trust manifest$factoRverse_semver. `_targets_update.R:367` passes the
  # previous manifest's value straight through instead of recomputing it, so the
  # field has been frozen at "0.3" since the initial run and every manifest on
  # disk repeats it. The meaningful comparison is against the version that
  # produced the paper's results, vendored in factoRverse/.
  installed <- as.character(utils::packageVersion("factoRverse"))
  message("  factoRverse      : installed ", installed, " | paper used ",
          FACTORVERSE_PIN, " | manifest field ",
          paste(manifest$factoRverse_semver, collapse = "."), " (stale, ignored)")
  if (!identical(installed, FACTORVERSE_PIN)) {
    warning("factoRverse mismatch -- the paper used ", FACTORVERSE_PIN,
            ", this session has ", installed,
            ". Results may not be comparable to the submitted ones.")
  }

  gold_results        <- load_pipeline_object(manifest, "gold_results")
  gold_aux_tables     <- load_pipeline_object(manifest, "gold_aux_tables")
  pregold_m_df        <- load_pipeline_object(manifest, "pregold_m_df")
  signal_port_cohorts <- load_pipeline_object(manifest, "signal_port_backtest_cohorts")

  gold_signals_m_df  <- pluck_by_name(gold_results, "signals")
  gold_clusters_m_df <- pluck_by_name(gold_results, "clusters")
  chosen_signals_and_positions <- manifest$chosen_signals_and_positions

  gold_dates <- sort(unique(gold_signals_m_df@data$dates))
  message("Gold panel: ", length(gold_dates), " dates, ",
          format(min(gold_dates)), " .. ", format(max(gold_dates)))

  # ---- 4. SCREEN THE SIGNAL LIBRARY ----------------------------------------
  #
  # The screen is applied to gold_aux_tables (signal_clusters / signal_themes /
  # cluster_themes) and to chosen_signals_and_positions. Everything downstream
  # follows from those: backtest_signal_selection derives the cluster-dataset
  # universe from signal_clusters_m_df, and prepare_gold_aux_to_backtest derives
  # adj_cluster_themes_m_df from all three.
  #
  # gold_signals_m_df is left untouched on purpose: select_and_correct_signals()
  # subsets it by chosen_signals_and_positions, and its @meta_dataframe_name is
  # checked against the cohort by extract_returns_m_xts().

  screen <- run_stage("screen", {

    clusters_tbl <- gold_aux_tables$signal_clusters_m_df@data

    # signal_clusters_m_df carries CORRECTED names (shorts prefixed "low_").
    excluded_corrected <- clusters_tbl %>%
      filter(cluster %in% EXCLUDED_CLUSTERS) %>%
      pull(tickers) %>% unique()

    if (length(excluded_corrected) == 0) {
      stop("No signals matched EXCLUDED_CLUSTERS (",
           paste(EXCLUDED_CLUSTERS, collapse = ", "), "). Clusters present: ",
           paste(sort(unique(clusters_tbl$cluster)), collapse = ", "))
    }

    corrected_names <- correct_chosen_signals_and_positions(chosen_signals_and_positions)
    kept_signals_and_positions <- chosen_signals_and_positions[
      !corrected_names %in% excluded_corrected
    ]

    message("Excluded clusters : ", paste(EXCLUDED_CLUSTERS, collapse = ", "))
    message("Excluded signals  : ", paste(sort(excluded_corrected), collapse = ", "))
    message("Library           : ", length(chosen_signals_and_positions), " -> ",
            length(kept_signals_and_positions), " signals")

    aux <- gold_aux_tables
    aux$signal_clusters_m_df@data <- aux$signal_clusters_m_df@data %>%
      filter(!cluster %in% EXCLUDED_CLUSTERS)
    aux$signal_themes_m_df@data <- aux$signal_themes_m_df@data %>%
      filter(!tickers %in% excluded_corrected)
    aux$cluster_themes_m_df@data <- aux$cluster_themes_m_df@data %>%
      filter(!tickers %in% EXCLUDED_CLUSTERS)

    # Every surviving signal must still carry a theme -- select_and_correct_
    # signals() errors otherwise, and it errors late.
    missing_theme <- setdiff(
      correct_chosen_signals_and_positions(kept_signals_and_positions),
      unique(aux$signal_themes_m_df@data$tickers)
    )
    if (length(missing_theme) > 0) {
      stop("Signals kept but with no theme: ", paste(missing_theme, collapse = ", "))
    }

    # Clusters dataset. backtest_base_signal_blending keeps contains("fac_") plus
    # the interaction and industry tickers; excluding `interactions` from
    # signal_clusters drops those automatically, but `fac_macro` survives
    # contains("fac_") and must go by name.
    excluded_fac_cols <- paste0("fac_", EXCLUDED_CLUSTERS)
    gold_clusters_screened <- gold_clusters_m_df
    drop_cols <- intersect(names(gold_clusters_screened@data),
                           c(excluded_fac_cols, excluded_corrected))
    gold_clusters_screened@data <- gold_clusters_screened@data %>%
      select(-all_of(drop_cols))
    message("Clusters dataset  : dropped ", paste(drop_cols, collapse = ", "))

    # Signal-portfolio cohorts. Members are named via
    # chosen_score_metric_and_position, which extract_returns_m_xts() reproduces;
    # rebuild from the survivors so the return matrices match the screened library.
    subset_cohort <- function(cohort, drop_corrected) {
      keep_idx <- vapply(cohort@port_backtest_results_list, function(x) {
        csm <- x@port_backtest_config@chosen_score_metric_and_position
        if (is.null(csm)) return(TRUE)   # not a single-signal member: keep
        corrected <- if (identical(unname(csm), "short")) {
          paste0("low_", names(csm))
        } else names(csm)
        !corrected %in% drop_corrected
      }, logical(1))

      n_dropped <- sum(!keep_idx)
      message("  ", cohort@cohort_name, ": dropped ", n_dropped, " of ",
              length(keep_idx), " backtests")
      if (n_dropped == 0) return(cohort)

      new_cohort <- factoRverse::create_port_backtest_cohort(
        cohort@port_backtest_results_list[keep_idx],
        cohort_name = cohort@cohort_name
      )
      # compute_signal_metrics() reads @port_metrics_m_xts_list and
      # @port_stats_m_xts_nested_list, so the rebuilt cohort must carry them.
      missing_slots <- setdiff(methods::slotNames(cohort),
                               methods::slotNames(new_cohort))
      if (length(missing_slots) > 0) {
        stop("Rebuilt cohort is missing slots: ",
             paste(missing_slots, collapse = ", "))
      }
      new_cohort
    }

    cohorts <- signal_port_cohorts
    cohorts$signal_port_backtest_cohort_ind_signals <- subset_cohort(
      cohorts$signal_port_backtest_cohort_ind_signals, excluded_corrected
    )
    cohorts$signal_port_backtest_cohort_clusters <- subset_cohort(
      cohorts$signal_port_backtest_cohort_clusters,
      c(excluded_corrected, excluded_fac_cols)
    )

    list(
      chosen_signals_and_positions = kept_signals_and_positions,
      excluded_signals             = excluded_corrected,
      gold_aux_tables              = aux,
      gold_clusters_m_df           = gold_clusters_screened,
      signal_port_cohorts          = cohorts
    )
  })

  # ---- 4b. DERIVE THE ML TARGET ---------------------------------------------
  #
  # Done here rather than inside the cached `screen` stage so that changing
  # ML_TARGET does not require rebuilding a 1 GB cache, and applied ONCE so that
  # the SB and MB stages see the same target table.

  if (identical(ML_TARGET, "fwd_act_cum_ret_3m")) {
    tgt <- screen$gold_aux_tables$fwd_3m_target_act_ibov_m_df
    if (!"fwd_act_geom_mean_ret_3m" %in% names(tgt@data)) {
      stop("fwd_act_geom_mean_ret_3m absent from the 3m target table; cannot derive ",
           ML_TARGET, ".")
    }
    tgt@data <- tgt@data %>%
      mutate(fwd_act_cum_ret_3m =
               (((1 + fwd_act_geom_mean_ret_3m / 100)^TARGET_TRADING_DAYS) - 1) * 100)
    screen$gold_aux_tables$fwd_3m_target_act_ibov_m_df <- tgt
    message("ML target: ", ML_TARGET, " derived over ", TARGET_TRADING_DAYS,
            " trading days (n non-NA: ", sum(!is.na(tgt@data$fwd_act_cum_ret_3m)), ")")
  } else {
    message("ML target: ", ML_TARGET, " (as shipped by gold_aux)")
  }

  if (stop_here("screen")) return(invisible(list(screen = screen)))

  # ---- 5. MINIMUM TRAINING SAMPLE FOR BH -----------------------------------
  #
  # `enable_theme_representativeness = TRUE` backfills one signal per theme even
  # when nothing is significant, so the SS output alone cannot answer "did BH
  # select anything?". The honest quantity is `pre_eligible_assets`, computed at
  # classify_investment_universe.R:242 as
  #     adjusted_p_value <= threshold & alpha > 0
  # BEFORE the representativeness backstop. run_ss_backtest also hard-errors when
  # nothing is eligible (classify_investment_universe.R:521), so a window that
  # yields zero does not merely lack information -- it does not run.
  #
  # This probe reproduces that path exactly (summarize_performance -> p.adjust on
  # UNIQUE p-values, matching define_signal_eligibility.R:150) on the signal
  # portfolio returns already in the cohort. Minutes, not the hours a repeated SS
  # backtest would take.
  #
  # Candidates are derived from the data: only window sizes landing on an SS
  # rebalancing month are admissible. Note production's initial_sample_size = 81
  # does NOT land on one; the paper branch's 78 does.

  probe <- run_stage("probe", {

    aux <- prepare_gold_aux_to_backtest(screen$gold_aux_tables, gold_dates)

    ret_indexes_m_xts      <- restore_meta_xts_dates(aux$ret_indexes_m_xts)
    ret_indexes_m_xts@data <- ret_indexes_m_xts@data[, "ibov", drop = FALSE]

    cohort <- screen$signal_port_cohorts$signal_port_backtest_cohort_ind_signals
    cohort@port_returns_m_xts_list$net_returns_m_xts <-
      restore_meta_xts_dates(cohort@port_returns_m_xts_list$net_returns_m_xts)

    extracted <- factoRverse:::extract_returns_m_xts(
      port_backtest_cohort    = cohort,
      signals_m_df            = gold_signals_m_df,
      benchmark_returns_m_xts = ret_indexes_m_xts,
      verbose                 = FALSE
    )

    selected <- factoRverse::select_and_correct_signals(
      signals_m_df                 = gold_signals_m_df@data,
      chosen_signals_and_positions = screen$chosen_signals_and_positions,
      signal_themes_m_df           = aux$signal_themes_m_df@data,
      backtest_returns_m_xts       = extracted$backtest_returns_m_xts@data
    )

    signal_returns <- selected$selected_backtest_returns_corrected_positions_m_xts
    themes_all     <- selected$selected_signal_themes_m_df
    bench_returns  <- extracted$benchmark_returns_m_xts@data
    ret_dates      <- as.Date(zoo::index(signal_returns))

    admissible <- which(lubridate::month(ret_dates) %in% SS_REBAL_MONTHS)
    admissible <- admissible[admissible >= PROBE_MIN_OBS &
                               admissible < length(ret_dates)]
    if (length(admissible) == 0) {
      stop("No admissible window sizes. Check SS_REBAL_MONTHS / PROBE_MIN_OBS.")
    }
    message("Admissible window sizes: ", paste(admissible, collapse = ", "))

    bh_count_at <- function(n_obs) {
      ref_date   <- ret_dates[n_obs]
      theme_date <- max(themes_all$dates[themes_all$dates <= ref_date])
      themes_ref <- themes_all %>% filter(dates == theme_date)

      perf <- factoRverse:::summarize_performance(
        selected_backtest_returns_corrected_positions_m_xts_upd_ref =
          signal_returns[seq_len(n_obs), , drop = FALSE],
        selected_market_factor_proxy_m_xts_upd_ref =
          bench_returns[seq_len(n_obs), , drop = FALSE],
        model_structure                = "no_pooled",
        model_spec_theme_level         = NULL,
        lmer_control                   = NULL,
        selected_signal_themes_m_d_ref = themes_ref,
        active_returns                 = TRUE,
        verbose                        = FALSE
      )$signal_universe_m_d_ref

      # p.adjust over UNIQUE p-values, as define_signal_eligibility.R does.
      pv <- data.frame(p_value = unique(perf$p_value))
      pv$adjusted_p_value <- stats::p.adjust(pv$p_value, method = "BH")
      perf <- dplyr::left_join(perf, pv, by = "p_value")

      tibble::tibble(
        n_obs         = n_obs,
        date          = ref_date,
        n_signals     = nrow(perf),
        n_bh          = sum(perf$adjusted_p_value <= SIGNIFICANCE_THRESHOLD &
                              perf$alpha > 0, na.rm = TRUE),
        n_uncorrected = sum(perf$p_value <= SIGNIFICANCE_THRESHOLD &
                              perf$alpha > 0, na.rm = TRUE),
        min_adj_p     = suppressWarnings(min(perf$adjusted_p_value, na.rm = TRUE))
      )
    }

    grid <- purrr::map_dfr(admissible, bh_count_at)
    print(as.data.frame(grid))

    viable     <- grid %>% filter(n_bh >= 1)
    min_viable <- if (nrow(viable) == 0) NA_integer_ else min(viable$n_obs)

    if (is.na(min_viable)) {
      warning("BH never selects a signal at any admissible window. The SS step ",
              "would rely entirely on theme representativeness.")
    } else {
      message("Minimum training sample with BH selecting >=1 signal: ", min_viable,
              "  (", format(grid$date[grid$n_obs == min_viable]), ")")
    }

    list(grid = grid, min_viable = min_viable)
  })

  utils::write.csv(probe$grid,
                   file.path(OUT_DIR, "bh_minimum_training_sample.csv"),
                   row.names = FALSE)

  if (stop_here("probe")) {
    message("Review bh_minimum_training_sample.csv before running the ML stages.")
    return(invisible(list(screen = screen, probe = probe)))
  }

  # ---- 6. SIGNAL SELECTION -------------------------------------------------
  #
  # One config, so there is nothing to elect: elect_ss_config() is skipped, which
  # also avoids the ranking port backtests it runs purely to break ties.

  ss_initial_sample_size <- SS_INITIAL_SAMPLE_SIZE %||% probe$min_viable
  if (is.na(ss_initial_sample_size)) {
    stop("No viable SS training sample. Set SS_INITIAL_SAMPLE_SIZE explicitly.")
  }
  message("SS initial_sample_size: ", ss_initial_sample_size)

  ss <- run_stage("ss", {
    set.seed(SEED)
    res <- backtest_signal_selection(
      gold_signals_m_df            = gold_signals_m_df,
      gold_clusters_m_df           = screen$gold_clusters_m_df,
      gold_aux_tables              = screen$gold_aux_tables,
      signal_port_backtest_cohorts = screen$signal_port_cohorts,
      initial_sample_size          = ss_initial_sample_size,
      chosen_signals_and_positions = screen$chosen_signals_and_positions,
      chosen_ss_configs            = SS_CONFIG,
      source                       = manifest$source,
      parallel                     = PARALLEL_SS
    )
    res$winner <- SS_CONFIG
    res
  })

  if (stop_here("ss")) {
    return(invisible(list(screen = screen, probe = probe, ss = ss)))
  }

  # ---- 7. SIGNAL BLENDING + EW META-BLEND ----------------------------------
  #
  # evaluate_sb_configs() is bypassed. Its job is to elect a subset of base
  # learners and a dataset on a lockbox; here every base learner enters the
  # ensemble by design and the dataset is pre-committed, so election would spend
  # lockbox months answering a question we are not asking. The flattening and the
  # backtest_identifier suffix it applies are reproduced so naming is unchanged.
  #
  # ind_signals only. `winner_dataset = SB_DATASET` makes
  # backtest_base_signal_blending() subset its `groups` list before training
  # (backtest_base_signal_blending.R:261), so no model is ever fitted on the
  # clusters dataset, and backtest_meta_blending() feeds gold_signals_m_df rather
  # than gold_clusters_m_df. The clusters SS result is still REQUIRED as an input
  # -- compute_signal_metrics() takes both datasets and is called before the
  # subset -- but its output (signal_metrics_clusters_m_df) is then unused. That
  # is why stage 6 still runs SS for both; it is frequentist BH and cheap. The
  # assertions below fail loudly if a clusters object ever reaches a model.

  # backtest_base_signal_blending() checkpoints per model and skips any already in
  # the checkpoint (skip_block, backtest_base_signal_blending.R:1666). A checkpoint
  # left over from a run with a DIFFERENT ML_TARGET would silently be reused, giving
  # an ensemble whose members were trained on two different targets. Refuse to start
  # in that case rather than produce a quietly incoherent result.
  .sb_ckpt <- file.path(CHECKPOINT_DIR,
                        "backtest_base_signal_blending_checkpoint",
                        paste0("backtest_base_signal_blending_checkpoint_",
                               gold_signals_m_df@current_date, ".qs"))
  #
  # THE SAME HAZARD APPLIES TO THE TUNING METRIC, and is worse because it is
  # invisible. The checkpoint path is keyed on the gold vintage ALONE -- not on
  # the arm and not on the metric -- while the config names inside it
  # ("glmnet_grid", ...) are identical across metric arms. Running the mpe arm
  # against a checkpoint left by the rmse arm would therefore skip every model
  # and hand back rmse-tuned learners wearing an mpe label: the experiment would
  # silently measure nothing. The target check below cannot catch it, because
  # target_fwd_name is the same in both arms.
  if (!file.exists(stage_path("sb")) && file.exists(.sb_ckpt)) {
    probe <- tryCatch({
      chk <- qs::qread(.sb_ckpt)
      pull <- function(f) unique(unlist(rapply(chk$data, function(x) {
        if (methods::is(x, "sb_backtest_results")) f(x) else NULL
      }, how = "unlist")))
      list(
        target = pull(function(x) x@sb_backtest_config@target_fwd_name),
        metric = pull(function(x) {
          ts <- x@sb_backtest_config@tuning_strategy
          if (is.null(ts)) NULL else ts@chosen_eval_metric   # OLS is untuned
        }),
        objective = pull(function(x) x@sb_backtest_config@custom_objective)
      )
    }, error = function(e) NULL)

    if (is.null(probe)) {
      stop("Unreadable SB checkpoint at\n  ", .sb_ckpt,
           "\nIt cannot be verified against this run and would be silently ",
           "reused. Delete it and re-run.")
    }
    if (length(probe$target) > 0 && !all(probe$target == ML_TARGET)) {
      stop("Stale SB checkpoint at\n  ", .sb_ckpt,
           "\nIt holds models trained on a different target than ML_TARGET ('",
           ML_TARGET, "'), and would be silently reused. Delete it and re-run.")
    }
    if (length(probe$metric) > 0 && !all(probe$metric == EVAL_METRIC)) {
      stop("SB checkpoint metric mismatch at\n  ", .sb_ckpt,
           "\nIt holds models tuned on '", paste(probe$metric, collapse = "', '"),
           "' but this run requests PAPER_EVAL_METRIC = '", EVAL_METRIC, "'.",
           "\nThe checkpoint is keyed on the gold vintage only, so those models",
           " would be skipped and reused, and the arm would silently measure",
           " nothing. Delete the checkpoint (or move it aside) and re-run.")
    }
    if (length(probe$objective) > 0 && !all(probe$objective == EVAL_OBJECTIVE)) {
      stop("SB checkpoint objective mismatch at\n  ", .sb_ckpt,
           "\nIt holds models trained on '",
           paste(probe$objective, collapse = "', '"), "' but this run requests '",
           EVAL_OBJECTIVE, "'. Those models would be skipped and reused.",
           " Delete the checkpoint (or move it aside) and re-run.")
    }
  }

  sb <- run_stage("sb", {
    set.seed(SEED)
    if (requireNamespace("tensorflow", quietly = TRUE)) {
      tensorflow::tf$random$set_seed(SEED)
    }

    winner_ss <- list(
      ind_signals = ss$ss_backtest_results_lists$freq_ss_backtest_results_lists$ind_signals[[SS_CONFIG]],
      clusters    = ss$ss_backtest_results_lists$freq_ss_backtest_results_lists$clusters[[SS_CONFIG]]
    )

    to_fit <- RERUN_LEARNERS %||% SB_CONFIGS
    base <- backtest_base_signal_blending(
      gold_signals_m_df               = gold_signals_m_df,
      gold_clusters_m_df              = screen$gold_clusters_m_df,
      gold_aux_tables                 = screen$gold_aux_tables,
      signal_port_backtest_cohorts    = screen$signal_port_cohorts,
      winner_ss_backtest_results_list = winner_ss,
      extracted_returns_m_xts_list    = list(
        returns_ind_signals_m_xts = ss$returns_ind_signals_m_xts,
        returns_clusters_m_xts    = ss$returns_clusters_m_xts,
        ret_indexes_m_xts         = ss$ret_indexes_m_xts
      ),
      chosen_sb_configs               = to_fit,
      winner_dataset                  = SB_DATASET,
      validation_sample_size          = SB_VALIDATION_SAMPLE_SIZE,
      source                          = manifest$source,
      parallel                        = FALSE   # ML stage: keep deterministic
    )

    # The per-model pipeline checkpoint is shared by the grid and Bayes arms.
    # It returns every completed learner, although this arm must feed only its
    # pre-committed configs into the equal-weighted meta-blend.
    base_results <- base$base_sb_backtest_results_lists
    base_config_names <- stringr::str_remove(
      names(base_results), "_backtest_results_list$"
    )
    missing_base_configs <- setdiff(to_fit, base_config_names)
    if (length(missing_base_configs) > 0L) {
      stop("Checkpoint is missing required ", ARM, " base learner(s): ",
           paste(missing_base_configs, collapse = ", "))
    }
    base_results <- base_results[base_config_names %in% to_fit]

    # Assertion 1: no selected model was trained on anything but ind_signals.
    trained_on <- unique(unlist(lapply(base_results, names)))
    if (!identical(sort(trained_on), SB_DATASET)) {
      stop("Base learners were trained on datasets {",
           paste(trained_on, collapse = ", "), "}; expected only '",
           SB_DATASET, "'.")
    }

    # Flatten to the naming evaluate_sb_configs() would have produced.
    elected <- purrr::imap(base_results,
                           function(sublist, model_name) {
      clean <- stringr::str_remove(model_name, "_backtest_results_list$")
      obj   <- sublist[[SB_DATASET]]
      obj@backtest_identifier <- paste0(obj@backtest_identifier, "_", SB_DATASET)
      stats::setNames(list(obj),
                      paste0(clean, "_", SB_DATASET, "_backtest_results"))
    }) %>% purrr::flatten()

    # Partial rerun: substitute the refitted learners into the submitted set,
    # keeping its order. `base` then holds the refitted learners only.
    if (!is.null(RERUN_LEARNERS)) {
      submitted <- qs::qread(file.path(BASE_DIR, basename(stage_path("sb"))))$elected
      wanted <- paste0(SB_CONFIGS, "_", SB_DATASET, "_backtest_results")
      submitted <- submitted[names(submitted) %in% wanted]
      submitted[names(elected)] <- elected
      elected <- submitted
      if (!setequal(names(elected), wanted)) {
        stop("No result, refitted or submitted, for: ",
             paste(setdiff(wanted, names(elected)), collapse = ", "))
      }
    }

    if (length(elected) != length(SB_CONFIGS)) {
      stop("Expected ", length(SB_CONFIGS), " base learners, got ", length(elected),
           ": ", paste(names(elected), collapse = ", "))
    }

    # Assertion 2: every ensemble member is an ind_signals learner.
    if (any(stringr::str_detect(names(elected), "clusters")) ||
        !all(stringr::str_detect(names(elected), SB_DATASET))) {
      stop("Non-ind_signals learner in the ensemble: ",
           paste(names(elected), collapse = ", "))
    }
    message("Base learners into EW ensemble: ", paste(names(elected), collapse = ", "))

    # Record the tuning actually used, per learner, for the methods appendix.
    tuning_used <- purrr::imap_dfr(base$base_sb_configs_list, function(cfg, nm) {
      if (!methods::is(cfg, "sb_backtest_config")) return(NULL)
      if (!cfg@config_name %in% SB_CONFIGS) return(NULL)
      ts <- cfg@tuning_strategy
      if (is.null(ts) || !identical(ts@tuning_method, "bayesian_opt")) return(NULL)
      tibble::tibble(
        config      = cfg@config_name,
        n_hyper     = length(ts@hyper_grid_domain@hyperparameter_list),
        init_points = ts@init_points,
        n_iter      = ts@n_iter,
        k_iter      = ts@k_iter,
        evals       = ts@init_points + ts@n_iter,
        evals_per_dim = round((ts@init_points + ts@n_iter) /
                                length(ts@hyper_grid_domain@hyperparameter_list), 1),
        epochs      = ceiling(ts@n_iter / ts@k_iter)
      )
    })
    print(as.data.frame(tuning_used))

    list(base = base, elected = elected, tuning_used = tuning_used)
  })

  # backtest_meta_blending() cannot be reused: it hardcodes
  # target_fwd_name = "fwd_act_geom_mean_ret_3m" (backtest_meta_blending.R:37) via a
  # NAMESPACE-QUALIFIED call to factoRverse::create_sb_backtest_config, which the
  # globalenv shadowing trick cannot reach. Its `ew_meta` branch is short, so it is
  # reproduced here with the target taken from ML_TARGET. Only the ew branch is
  # needed; the sw branch and the clusters path are unused under this config.
  stopifnot(identical(MB_CONFIG, "ew_meta"))

  mb <- run_stage("mb", {
    aux <- prepare_gold_aux_to_backtest(
      screen$gold_aux_tables,
      sort(unique(gold_signals_m_df@data$dates))
    )

    tgt_cols <- names(aux$fwd_3m_target_act_ibov_m_df@data)
    if (!ML_TARGET %in% tgt_cols) {
      stop("ML target '", ML_TARGET, "' absent from the meta target table. ",
           "Available: ", paste(setdiff(tgt_cols, c("id", "tickers", "dates")),
                                collapse = ", "))
    }

    ew_meta_config <- factoRverse::create_sb_backtest_config(
      sb_algorithm         = "ew",
      target_fwd_name      = ML_TARGET,
      training_sample_size = 6,
      rebalancing_months   = c(2, 4, 5, 8, 11),
      config_name          = "ew_meta"
    ) %>%
      factoRverse::create_sb_metabacktest_config(
        features_passthrough       = "none",
        config_name                = "ew_meta",
        normalize_base_predictions = TRUE,
        winsorize_base_predictions = TRUE
      )

    # backtest_meta_blending does this before handing the learners to run_sb_backtest.
    elected <- sb$elected
    for (b in names(elected)) {
      elected[[b]]@oos_testing_eval_metrics_m_xts <-
        restore_meta_xts_dates(elected[[b]]@oos_testing_eval_metrics_m_xts)
    }

    message("meta-blend target: ", ML_TARGET)
    set.seed(SEED)
    factoRverse::run_sb_backtest(
      features_m_df                 = gold_signals_m_df,   # SB_DATASET is ind_signals
      target_m_df                   = aux$fwd_3m_target_act_ibov_m_df,
      config                        = ew_meta_config,
      base_sb_backtest_results_list = elected,
      parallel                      = FALSE
    )
  })

  # Assertion 3: the meta-blend carries only ind_signals base learners.
  mb_members <- names(mb@base_sb_backtest_results_list)
  if (any(stringr::str_detect(mb_members, "clusters")) ||
      !all(stringr::str_detect(mb_members, SB_DATASET))) {
    stop("Meta-blend contains non-ind_signals learners: ",
         paste(mb_members, collapse = ", "))
  }
  message("Meta-blend members verified ind_signals-only: ",
          paste(mb_members, collapse = ", "))

  if (stop_here("mb")) {
    return(invisible(list(screen = screen, probe = probe, ss = ss, sb = sb, mb = mb)))
  }

  # ---- 8. META PORTFOLIOS --------------------------------------------------
  #
  # The pipeline's backtest_meta_portfolios() hardcodes the mandate set
  # c("ibov","smll","large_payers","isee") in three places, so it cannot express
  # the idiv mandate. Rather than edit the pipeline repo (which Codex also works
  # in), the driver is reproduced here in paper form: same factoRverse calls,
  # same config-factory shape, mandates and scaler taken from CONFIG.

  paper_meta_port_config <- function(mandate_name, mandate, port_name,
                                     initial_buffer) {

    method    <- stringr::str_remove_all(port_name, "_box|_group")
    add_box   <- stringr::str_detect(port_name, "box")
    add_group <- stringr::str_detect(port_name, "group")

    cfg <- factoRverse::create_port_backtest_config(
      eligibility_quantile_range = mandate$eligibility_quantile_range,
      selected_benchmark         = mandate$benchmark,
      initial_buffer_period      = initial_buffer,
      chosen_scaler              = if (USE_SCALER) "adj_scaler" else NULL,
      scaler_shrinkage           = if (USE_SCALER) 0.5 else NULL,
      use_raw_for_eligibility    = if (USE_SCALER && add_box) TRUE else NULL,
      rebalancing_months         = c(2, 4, 5, 8, 11),
      port_construction_method   = method,
      enable_group_representativeness = TRUE,
      main_liquidity_metric      = "mean_volfin_3m",
      config_name                = paste0("meta_", port_name, "_", mandate_name)
    ) %>%
      factoRverse::add_liquidity_floor_cutoffs(
        metric_name    = colnames(manifest$liquidity_floor_cutoffs)[-1],
        metric_cutoffs = list(
          setNames(manifest$liquidity_floor_cutoffs$mean_volfin_3m,
                   manifest$liquidity_floor_cutoffs$liquidity_classification),
          setNames(manifest$liquidity_floor_cutoffs$presence,
                   manifest$liquidity_floor_cutoffs$liquidity_classification)
        )
      ) %>%
      factoRverse::add_liquidity_constraint_policy(
        liquidity_floor_rule = manifest$liquidity_floor_rule
      ) %>%
      factoRverse::add_transaction_costs_parameters(
        direct_transaction_cost = TC_PARS_COMMON$direct_transaction_cost,
        alpha                   = TC_PARS_COMMON$alpha,
        lambda                  = TC_PARS_COMMON$lambda,
        strategy_aum            = mandate$strategy_aum
      )

    if (method %in% c("rp", "hrp", "mvo")) {
      cfg <- cfg %>% factoRverse::add_cov_est_method(
        cov_estimation_method  = "shrink_cc",
        cov_matrix_sample_size = 126,
        active_returns         = TRUE
      )
    }

    if (method == "mvo") {
      cfg <- cfg %>% factoRverse::add_mvo_parameters(
        n_random_ports = 1000, opt_objective = "sharpe", n_resamples = 50,
        exp_ret_score_jitter = 0.50, cov_eigval_jitter = 0.25
      )
      if (port_name == "mvo_box") {
        cfg <- cfg %>% factoRverse::add_turnover_constraint_policy(
          quantile_range_buffer = 0.1,
          turnover_cap_rules    = c(micro_caps = 0.01, small_caps = 0.025)
        )
      }
    }

    if (add_box && method %in% c("mvo", "rp")) {
      cfg <- cfg %>% factoRverse::add_concentration_constraint_policy(
        max_abs_active_individual_weight = 0.05
      )
    }
    if (add_group && method == "mvo") {
      cfg <- cfg %>% factoRverse::add_concentration_constraint_policy(
        max_abs_active_group_weight = c(sectors_c2 = 0.10)
      )
    }

    cfg
  }

  # Shared OOS input preparation for the two portfolio stages.
  #
  # `meta` and `learners` build portfolios over the same base-learner OOS window
  # from the same gold panels. run_stage() caches by file, so on a re-run where
  # meta_<arm>.qs already exists the meta stage body never executes and its
  # locals do not exist -- `learners` therefore cannot borrow them and must
  # rebuild the inputs itself. Extracted here so the two stages cannot drift.
  prepare_oos_port_inputs <- function() {

    base_oos_dates <- mb@base_learners_oos_predictions_m_df@data %>%
      pull(dates) %>% unique() %>% sort()
    message("Base-learner OOS window: ", format(min(base_oos_dates)), " .. ",
            format(max(base_oos_dates)), " (", length(base_oos_dates), " periods)")

    # The meta learner consumes the first `training_sample_size` periods of the
    # base-learner OOS panel to train, so its own predictions start later. The
    # aux tables below are filtered to base_oos_dates (as the production driver
    # does), which leaves leading periods that carry signals but no meta
    # prediction -- run_port_backtest() must be told to skip them, or
    # check_inputs_port_backtest() rejects the run. backtest_meta_portfolios()
    # hardcodes initial_buffer_period = 6 to match its training_sample_size = 6;
    # derive it here instead so the two cannot drift apart.
    meta_oos_dates <- mb@meta_sb_backtest_results@oos_sb_outputs_m_df@data %>%
      pull(dates) %>% unique() %>% sort()
    meta_buffer <- match(min(meta_oos_dates), base_oos_dates)
    if (is.na(meta_buffer)) {
      stop("First meta-learner OOS date (", format(min(meta_oos_dates)),
           ") is absent from the base-learner OOS window.")
    }
    message("Meta-learner OOS window: ", format(min(meta_oos_dates)), " .. ",
            format(max(meta_oos_dates)), " (", length(meta_oos_dates),
            " periods); port initial_buffer_period = ", meta_buffer)

    aux <- screen$gold_aux_tables
    subset_dates <- function(obj, dts) {
      obj@data <- obj@data %>% filter(dates %in% dts); obj
    }

    oos_gold       <- subset_dates(gold_signals_m_df, base_oos_dates)
    oos_bench_w    <- subset_dates(aux$bench_weights_m_df, base_oos_dates)
    oos_liquidity  <- subset_dates(aux$liquidity_m_df, base_oos_dates)
    oos_volatility <- subset_dates(aux$volatility_m_df, base_oos_dates)
    oos_fwd_1m     <- subset_dates(aux$fwd_1m_returns_m_df, base_oos_dates)
    oos_sectors    <- aux$sectors_m_df
    oos_sectors@data <- oos_sectors@data %>%
      filter(dates %in% base_oos_dates) %>%
      select(id, tickers, dates, sectors_c2)

    # IDIV benchmark weights begin 2011-04, SMLL 2008-07, ISEE 2005-12. A mandate
    # whose benchmark has no weights over part of the OOS window cannot be built,
    # so the mandates will not all share a start date if the window reaches back.
    for (nm in names(META_MANDATES)) {
      b <- META_MANDATES[[nm]]$benchmark
      covered <- oos_bench_w@data %>%
        filter(!is.na(.data[[b]]), .data[[b]] > 0) %>%
        pull(dates) %>% unique()
      uncovered <- setdiff(base_oos_dates, covered)
      if (length(uncovered) > 0) {
        warning("Mandate '", nm, "' (benchmark ", b, ") has no weights for ",
                length(uncovered), " of ", length(base_oos_dates),
                " OOS periods; earliest covered: ", format(min(covered)))
      }
    }

    daily_stocks <- restore_meta_xts_dates(aux$daily_returns_stocks_m_xts)
    daily_bench  <- restore_meta_xts_dates(aux$daily_returns_bench_m_xts)
    ret_indexes  <- restore_meta_xts_dates(aux$ret_indexes_m_xts)

    daily_dates <- as.Date(zoo::index(daily_stocks@data))
    oos_daily   <- daily_dates[
      daily_dates >= as.Date(lubridate::add_with_rollback(min(base_oos_dates),
                                                          months(-12))) &
        daily_dates <= max(base_oos_dates)
    ]
    daily_stocks@data <- daily_stocks@data[oos_daily, ]
    daily_bench@data  <- daily_bench@data[oos_daily, ]
    ret_indexes@data  <- ret_indexes@data[as.Date(base_oos_dates), ]
    daily_stocks@current_date <- update(daily_stocks@current_date, day = 15)
    daily_bench@current_date  <- update(daily_bench@current_date, day = 15)

    oos_port_metrics <- compute_meta_port_metrics(
      pregold_m_df = pregold_m_df, gold_dates = base_oos_dates
    )

    list(base_oos_dates = base_oos_dates, meta_oos_dates = meta_oos_dates,
         meta_buffer = meta_buffer, aux = aux, subset_dates = subset_dates,
         gold = oos_gold, bench_w = oos_bench_w, liquidity = oos_liquidity,
         volatility = oos_volatility, fwd_1m = oos_fwd_1m, sectors = oos_sectors,
         daily_stocks = daily_stocks, daily_bench = daily_bench,
         ret_indexes = ret_indexes, port_metrics = oos_port_metrics)
  }

  meta <- run_stage("meta", {

    inp <- prepare_oos_port_inputs()
    base_oos_dates   <- inp$base_oos_dates
    meta_oos_dates   <- inp$meta_oos_dates
    meta_buffer      <- inp$meta_buffer
    aux              <- inp$aux
    subset_dates     <- inp$subset_dates
    oos_gold         <- inp$gold
    oos_bench_w      <- inp$bench_w
    oos_liquidity    <- inp$liquidity
    oos_volatility   <- inp$volatility
    oos_fwd_1m       <- inp$fwd_1m
    oos_sectors      <- inp$sectors
    daily_stocks     <- inp$daily_stocks
    daily_bench      <- inp$daily_bench
    ret_indexes      <- inp$ret_indexes
    oos_port_metrics <- inp$port_metrics

    results <- list()
    for (mandate_name in names(META_MANDATES)) {
      mandate <- META_MANDATES[[mandate_name]]

      and_rule <- NULL
      if (!is.null(mandate$and_rule)) {
        and_rule <- subset_dates(aux[[mandate$and_rule]], base_oos_dates)
        # The rule flag sits in the penultimate column and must be character,
        # matching the production driver.
        col_pos <- ncol(and_rule@data) - 1
        and_rule@data[, col_pos] <- as.character(and_rule@data[, col_pos])
      }

      for (port_name in META_PORT_CONFIGS) {
        key <- paste0("meta_", port_name, "_", mandate_name)
        message("  running ", key)
        set.seed(SEED)
        results[[key]] <- factoRverse::run_port_backtest(
          signals_m_df                = oos_gold,
          fwd_return_m_df             = oos_fwd_1m,
          liquidity_m_df              = oos_liquidity,
          volatility_m_df             = oos_volatility,
          config                      = paper_meta_port_config(mandate_name,
                                                               mandate, port_name,
                                                               meta_buffer),
          benchmark_weights_m_df      = oos_bench_w,
          benchmark_returns_m_xts     = ret_indexes,
          daily_stock_returns_m_xts   = daily_stocks,
          daily_bench_returns_m_xts   = daily_bench,
          stock_groups_m_df           = oos_sectors,
          custom_stock_metrics_m_df   = oos_port_metrics,
          sb_backtest_results         = mb,
          user_defined_AND_rules_m_df = and_rule,
          scaler_m_df                 = NULL,   # scaler off; see USE_SCALER
          verbose                     = TRUE,
          parallel                    = if (grepl("mvo", port_name)) PARALLEL_MVO else FALSE
        )
      }
    }

    cohorts <- purrr::map(names(META_MANDATES), function(mandate_name) {
      keys <- names(results)[endsWith(names(results), paste0("_", mandate_name))]
      factoRverse::create_port_backtest_cohort(
        results[keys], cohort_name = paste0(mandate_name, "_meta_cohort")
      )
    }) %>% setNames(names(META_MANDATES))

    list(results = results, cohorts = cohorts, oos_dates = base_oos_dates,
         meta_oos_dates = meta_oos_dates, buffer = meta_buffer)
  })

  if (stop_here("meta")) {
    return(invisible(list(screen = screen, probe = probe, ss = ss, sb = sb,
                          mb = mb, meta = meta)))
  }

  # ---- 8b. INDIVIDUAL BASE-LEARNER PORTFOLIOS ------------------------------
  #
  # The `meta` stage turns the ENSEMBLE into portfolios. This stage does the
  # same for each base learner on its own, so the paper can ask whether the
  # ensemble is doing work that any single model already does -- the H7
  # "systematisation, not machine learning" exhibit.
  #
  # Follows the pipeline's evaluate_sb_configs.R (R/helpers), which builds a
  # cap-scaled (cs) and a risk-parity (rp) portfolio per candidate config. Two
  # deliberate departures from that reference:
  #
  #   * The configs come from paper_meta_port_config() rather than being
  #     hand-rolled. The paper's ibov mandate already carries the reference's
  #     eligibility_quantile_range = c(0.67, 1) and selected_benchmark =
  #     "ibov", but going through the paper helper ALSO inherits its covariance
  #     estimator for rp, its transaction-cost parameters and its manifest
  #     liquidity cutoffs. Without that the learner portfolios would not be
  #     comparable to meta_cs_ibov / meta_rp_ibov, which is the only comparison
  #     this stage exists to support.
  #
  #   * initial_buffer_period = 1, not the meta stage's meta_buffer. That
  #     buffer exists because the meta learner spends the first periods of the
  #     base-learner OOS panel training, so it has no predictions there. A base
  #     learner has predictions for every date in base_oos_dates by
  #     construction -- the window is DEFINED as its own OOS panel -- so there
  #     is nothing to skip.
  #
  # Scope is ibov only. The exhibit compares MODELS, not mandates; running all
  # four mandates would be 56 backtests instead of 14 to answer a question
  # nobody asked of it.
  LEARNER_PORT_CONFIGS <- c("cs", "rp")

  learners <- run_stage("learners", {

    inp <- prepare_oos_port_inputs()
    base_oos_dates <- inp$base_oos_dates

    mandate_name <- "ibov"
    mandate      <- META_MANDATES[[mandate_name]]
    if (!is.null(mandate$and_rule)) {
      stop("The learners stage assumes the ibov mandate has no AND rule.")
    }

    elected <- sb$elected
    message("Base learners to evaluate: ", length(elected), " (",
            paste(names(elected), collapse = ", "), ")")

    # The gold panels are loaded live from the pipeline board; the base learners
    # come out of a cached stage. When those two vintages diverge,
    # run_port_backtest() rejects the mix inside check_consistent_dates() with a
    # bare "Inconsistent current_date values" and no hint about which side
    # moved. Fail here instead, naming the knob that fixes it.
    gold_cd    <- inp$gold@current_date
    learner_cd <- elected[[1L]]@oos_sb_outputs_m_df@current_date
    if (!identical(gold_cd, learner_cd)) {
      stop("Vintage mismatch: gold panels are stamped ", format(gold_cd),
           " but the cached base learners are stamped ", format(learner_cd),
           ". MANIFEST_NAME (", if (is.null(MANIFEST_NAME)) "NULL = latest"
           else MANIFEST_NAME, ") does not match the manifest sb_", ARM_TAG,
           ".qs was built from -- see provenance.qs$manifest. Pin MANIFEST_NAME",
           " to that vintage, or re-run every stage from `sb` onward.")
    }

    results <- list()
    for (learner_name in names(elected)) {
      for (port_name in LEARNER_PORT_CONFIGS) {
        key <- paste0("learner_", port_name, "_", learner_name)
        message("  running ", key)

        cfg <- paper_meta_port_config(mandate_name, mandate, port_name,
                                      initial_buffer = 1L)
        # Rename for traceability and uniqueness -- paper_meta_port_config()
        # names it meta_<port>_<mandate>, which would collide across learners
        # and read as an ensemble portfolio in every downstream table.
        cfg@config_name <- key

        set.seed(SEED)
        results[[key]] <- factoRverse::run_port_backtest(
          signals_m_df                = inp$gold,
          fwd_return_m_df             = inp$fwd_1m,
          liquidity_m_df              = inp$liquidity,
          volatility_m_df             = inp$volatility,
          config                      = cfg,
          benchmark_weights_m_df      = inp$bench_w,
          benchmark_returns_m_xts     = inp$ret_indexes,
          daily_stock_returns_m_xts   = inp$daily_stocks,
          daily_bench_returns_m_xts   = inp$daily_bench,
          stock_groups_m_df           = inp$sectors,
          custom_stock_metrics_m_df   = inp$port_metrics,
          sb_backtest_results         = elected[[learner_name]],
          user_defined_AND_rules_m_df = NULL,
          scaler_m_df                 = NULL,   # scaler off; see USE_SCALER
          verbose                     = TRUE,
          parallel                    = FALSE
        )
      }
    }

    cohort <- factoRverse::create_port_backtest_cohort(
      results, cohort_name = "learner_cohort"
    )

    list(results = results, cohort = cohort,
         port_configs = LEARNER_PORT_CONFIGS,
         learners = names(elected), mandate = mandate_name,
         oos_dates = base_oos_dates)
  })

  # ---- 9. PROVENANCE -------------------------------------------------------

  provenance <- list(
    timestamp              = Sys.time(),
    manifest               = basename(manifest_path),
    manifest_git_hash      = manifest$git_hash,
    manifest_factoRverse   = manifest$factoRverse_semver,
    pipeline_git_hash      = PIPELINE_COMMIT,
    paper_git_hash         = git_hash_of(here::here()),
    excluded_clusters      = EXCLUDED_CLUSTERS,
    excluded_signals       = screen$excluded_signals,
    n_signals              = length(screen$chosen_signals_and_positions),
    ss_config              = SS_CONFIG,
    ss_initial_sample_size = ss_initial_sample_size,
    bh_probe_grid          = probe$grid,
    run_name               = RUN_NAME,
    rerun_from             = RERUN_FROM,
    rerun_learners         = RERUN_LEARNERS,
    sb_configs             = SB_CONFIGS,
    sb_dataset             = SB_DATASET,
    tuning_arm             = ARM,
    tuning_eval_metric     = EVAL_METRIC,
    tuning_quantile_tau    = EVAL_TAU,
    tuning_huber_delta     = EVAL_DELTA,
    tuning_custom_objective = EVAL_OBJECTIVE,
    artefact_tag           = ARM_TAG,
    tuning_preset          = TUNING_PRESET,
    tuning_used            = sb$tuning_used,
    grid_overrides         = GRID_OVERRIDES,
    xgb_nrounds_bounds     = XGB_NROUNDS_BOUNDS,
    fix_nn2_architecture   = FIX_NN2_ARCHITECTURE,
    mb_config              = MB_CONFIG,
    ml_target              = ML_TARGET,
    meta_port_configs      = META_PORT_CONFIGS,
    mandates               = META_MANDATES,
    use_scaler             = USE_SCALER,
    meta_oos_dates         = meta$oos_dates,
    meta_learner_oos_dates = meta$meta_oos_dates,
    meta_port_buffer       = meta$buffer,
    learner_port_configs   = LEARNER_PORT_CONFIGS,
    learner_mandate        = learners$mandate,
    learner_names          = learners$learners,
    session                = utils::sessionInfo()
  )
  # Tagged like the stage artefacts, so a metric arm does not overwrite the
  # baseline's provenance record. The untagged name stays the baseline's.
  qs::qsave(provenance,
            file.path(OUT_DIR, paste0("provenance", EVAL_TAG, ".qs")))

  message("\nDone. Outputs in ", OUT_DIR)
  invisible(list(screen = screen, probe = probe, ss = ss, sb = sb,
                 mb = mb, meta = meta, provenance = provenance))
}

paper_machine <- main()

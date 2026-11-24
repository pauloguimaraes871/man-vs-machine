# Check: change one elastic-net setting. Only that learner may differ.
RERUN_FROM     <- "sb"
RERUN_LEARNERS <- "glmnet_grid"
STOP_AFTER     <- "mb"
GRID_OVERRIDES$glmnet_grid$alpha <- c(0.5, 1)  # submitted: c(0.25, 0.5, 1)

# Reruns and analysis

## Rerun with one change

Create `runs/<name>.R` with only what changes. Any setting from section 1 of
`run_paper_machine.R` can be set there.

    RERUN_FROM     <- "sb"         # first stage to recompute
    RERUN_LEARNERS <- "xgb_grid"   # optional, only with "sb": learners to refit
    GRID_OVERRIDES$xgb_grid$eta <- c(0.01, 0.05)

Then, in a fresh R session:

    Sys.setenv(PAPER_RUN = "<name>")
    source("run_paper_machine.R")

Outputs, checkpoints and a copy of the run file go to `data/runs/<name>/`.
`data/derived/machine_run/` (the submitted results) is only read. Use a new name
for each specification: an edited run file is refused once its run has started.

| What changes | RERUN_FROM |
|---|---|
| Signal exclusions | `screen` |
| Signal selection | `ss` |
| A learner, its grid, the target, the loss | `sb` |
| Meta-blend | `mb` |
| Portfolio settings | `meta` |

- The script cannot tell which stages a change affects: choose the earliest one.
- Without `RERUN_LEARNERS` all seven learners are refitted (about a day with NNs).
- Arms are chosen with `PAPER_ARM` and `PAPER_EVAL_METRIC`, set before sourcing.
- `nn2_grid` and `nn3_grid` copy NN1's grid before the run file is read: change
  each one explicitly.
- A setting hard-coded in `R/pipeline/` is edited there, on a branch. A new
  algorithm also needs an entry in `sb_registry` in that folder.
- In a partial rerun, `sb$elected` has all learners and `sb$base` only the refitted.

`check_ols.R` and `check_glmnet_alpha.R` are worked examples that double as tests.

Only OLS has been shown to reproduce the submitted predictions exactly. Before
attributing a difference to a change in another learner, refit that learner once
with no change (copy `check_ols.R` and change the name in `RERUN_LEARNERS`).

## Analysing a run

Exhibits, gap analysis and model inspection are covered in
[REPRODUCIBILITY.md](../REPRODUCIBILITY.md). Read its warning before regenerating
exhibits: the scripts overwrite hand-edited captions.

To feed a run into those scripts, point their input paths at `data/runs/<name>/`
(`ML_BACKTEST_FILE` in `build_gap_panel.R`, `MACHINE_RUN` in `plots.R`). Files the
run did not produce must still come from `data/derived/machine_run/`.

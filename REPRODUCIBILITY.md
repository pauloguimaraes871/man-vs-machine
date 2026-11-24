# Reproducibility

How to regenerate the paper's exhibits, inspect its models and rerun one with a
change. Last verified on 2026-11-22.

## What has been verified

| Claim | Status |
|---|---|
| Gap panel, rebuilt from the raw fund files | Identical |
| The manuscript's 20 generated images | Byte-identical |
| Regenerated CSV tables | Byte-identical |
| Regenerated `.tex` exhibits (30) | Identical outside captions and notes. 16 have hand-edited wording, see the warning below |
| Test suite | 245 pass, 0 fail |
| R environment, restored into an empty library | All 236 locked versions match |
| OLS refit, and the ensemble rebuilt from it | Identical predictions |
| A one-parameter change (elastic-net alpha) | Only that setting, that learner and the ensemble change |
| Refits of elastic net, RF, XGB and neural networks | **Not tested** for exact reproduction |
| Screen and signal selection recomputed | **Not tested** |
| Portfolio stages recomputed, Bayes and loss-function arms rerun | **Not tested** |
| LaTeX compilation | **Not tested** |

The exhibits reproduce from the cached model results. Re-estimating every model
from scratch has not been shown to reproduce them.

## Warning: regenerating overwrites the submitted wording

Captions and notes in 16 files under `tables/` and `figures/` were edited by hand
on 1-2 September 2026, after generation. `plots.R` and `analysis_gap.R` overwrite
them with the older wording, and neither folder is tracked by git.

Before running either script, copy the exhibits (PowerShell, project root):

    $d = "data/exhibits_$(Get-Date -Format yyyyMMdd_HHmmss)"
    New-Item -ItemType Directory $d
    Copy-Item tables, figures -Destination $d -Recurse
    Copy-Item latex_paper/figures -Destination "$d/latex_figures" -Recurse

Afterwards compare the `.tex` files and carry the wording over, or move the
wording into the scripts.

## What you need

Git holds the code. `data/` is not tracked (confidential) and must be copied too.

| Path | Content | Needed for |
|---|---|---|
| `data/*_refined.xlsx`, `data/CDI.xlsx` | Comdinheiro fund, index and CDI exports | Gap panel, plots |
| `data/derived/*.qs` | Gap and industry panels | Gap analysis |
| `data/derived/machine_run/` | Submitted model and portfolio results (S4) | Plots, inspection, reruns |
| `data/inputs/machine_2026_07/` | July 2026 manifest and gold inputs, with `SHA256SUMS` | Model reruns |
| `R/pipeline/` (tracked) | The pipeline functions the paper uses | Model reruns |
| `factoRverse/v.0.7.0/` (tracked) | Package tarball | Everything |

The pipeline repository is not needed.

Under OneDrive, mark `data/` "Always keep on this device". Scripts can hang on
cloud-only files.

## Setup

1. Open the project in R 4.3.0 and run `renv::restore()`. Windows needs Rtools43,
   because some packages build from source.
2. Only for `run_paper_machine.R`: create the Python environment and the project
   `.Renviron`, as described in [environment/README.md](environment/README.md).

A project `.Renviron` replaces your personal one for sessions started here. Add
any personal variable you need to it. It is not tracked.

## Regenerate the exhibits

After making the copy above, run each in a fresh R session, in this order:

| Script | Time | Writes |
|---|---|---|
| `build_gap_panel.R` | 3-4 min | `data/derived/gap_panel.qs`, `industry_panel.qs` |
| `analysis_gap.R` | 6 min | `tables/gap_*`, gap and fund-industry figures, `data/derived/gap_raw_fund_month_full.qs` |
| `plots.R` | 6 min | All other tables and figures, copies in `latex_paper/figures/` |
| `analysis_rmse_gap.R` | under 1 min | `output/rmse_gap_diagnostics/` (not in the paper) |

    source("build_gap_panel.R", encoding = "UTF-8")

To change only the regressions, start at `analysis_gap.R`. Then run the tests:

    testthat::test_dir("tests/testthat")

`plots_and_tables.R` is a legacy script for an older data layout. Do not use it.

## Inspect the models

    library(factoRverse)
    sb <- qs::qread("data/derived/machine_run/sb_grid.qs")
    names(sb$elected)                      # S4 results, one per learner
    sb$elected[[1]]@sb_backtest_config     # the settings actually used
    sb$elected[[1]]@oos_sb_outputs_m_df    # out-of-sample predictions

| File | Content |
|---|---|
| `screen.qs`, `probe.qs`, `ss.qs` | Screened library, sample-size probe, signal selection |
| `sb_<arm>.qs` | Base learners |
| `mb_<arm>.qs` | Equal-weight ensemble |
| `meta_<arm>.qs` | Ensemble portfolios |
| `learners_<arm>.qs` | Per-learner portfolios (not run for the Bayes arm) |

The arms are `grid` (main), `bayes`, `grid_mpe` and `grid_mphe`.

In `sb_bayes.qs`, `$base` also holds six grid learners left in a shared
checkpoint. Use `$elected`, which has the correct seven.

## Rerun with one change

Always set `PAPER_RUN`. Without it the script works inside the submitted results
folder: it skips cached stages but rewrites the provenance and probe CSV files,
and writes any stage that is missing.

1. Create `runs/<name>.R` containing only the change:

       RERUN_FROM     <- "sb"
       RERUN_LEARNERS <- "xgb_grid"
       GRID_OVERRIDES$xgb_grid$eta <- c(0.01, 0.05)

2. In a fresh R session:

       Sys.setenv(PAPER_RUN = "<name>")
       source("run_paper_machine.R", encoding = "UTF-8")

3. Results go to `data/runs/<name>/`. Commit the run file with the response to
   the request, so the specification is on record.

For any learner other than OLS, first run the same file without the change,
under another name, and compare it with the submitted learner. That separates
the effect of the change from refit noise. See [runs/README.md](runs/README.md)
for which stage to restart from.

For a full re-estimation, use a run file containing only
`RERUN_FROM <- "screen"`. Allow more than a day for each arm.

## Origin of the submitted results

| Item | Value |
|---|---|
| Estimated | 9-27 August 2026 |
| Data vintage | `manifest_2026_07.qs` |
| Pipeline code | `8739bb0bce966dad8f7c701f0c7413a6fa25f61f` |
| factoRverse | 0.7.0 |
| Seeds | 20250618 in the script, 123 before each base learner |

The pipeline commit was reconstructed from fit timestamps, the pipeline's reflog,
diffs and fingerprints of the cached objects. Uncommitted edits in August cannot
be ruled out.

Known defects in the saved records:

- `pipeline_git_hash` in `provenance*.qs` is the commit checked out when a run
  ended, not the one that produced it. Use the pin above.
- `provenance.qs` describes the grid arm only. There is no record for Bayes.
- `tuning_used$n_hyper` counts fixed hyperparameters as searched, so
  `evals_per_dim` is also wrong. Do not quote either.
- The untracked `provenance/README.md` describes an older run (`bf886b1`,
  factoRverse 0.6.1) and does not apply to the submitted results.
- The Python package versions of August were not recorded. TensorFlow's oneDNN
  optimisations can also introduce round-off differences in neural networks.

# Man vs Machine

Research repository for **Guimaraes, P. R.**, *Man vs Machine: can AI-based
stock-picking outperform human skill?* — a decomposition of the return gap
between Brazilian discretionary equity fund managers and a systematic,
literature-anchored machine-learning benchmark.

The machine is not the paper's subject; it is its **measuring instrument**: a
benchmark that embodies the published frontier of cross-sectional return
prediction (linear and non-linear, tuned and selected out-of-sample, with
multiple-testing control), against which human managers' skill is measured.
The question the paper asks is what managers do that the instrument cannot
capture, and what structural frictions prevent them from capturing it.

## Relation to other repositories

- **[`factoRverse`](https://github.com/pauloguimaraes871/factoRverse)** is the
  R package that does the actual estimation: signal engineering, signal
  selection under multiple-testing control, ML/heuristic signal blending, and
  constraint-aware portfolio construction. This repo consumes it as a
  dependency; it does not reimplement any workflow.
- **`pipeline`** (private) is the production Multifatorial 3.0 pipeline that
  built the July 2026 gold data. It is not needed to run this repo: the functions
  the paper uses are in [R/pipeline/](R/pipeline/) and the data snapshot is in
  `data/inputs/machine_2026_07/`. [run_paper_machine.R](run_paper_machine.R)
  runs the paper's machine on that snapshot, under academic settings that
  deliberately diverge from deployment in a few documented ways (see the header
  comment of that script).

## Layout

| File | Role |
|---|---|
| [`REPRODUCIBILITY.md`](REPRODUCIBILITY.md) | **Start here** to set up, regenerate exhibits, inspect models or rerun with a change. |
| [`run_paper_machine.R`](run_paper_machine.R) | End-to-end driver of the paper's machine (signal screening → selection → blending → meta-ensemble → portfolios), from the saved July 2026 inputs. Resumable; every stage is cached to disk. |
| [`R/pipeline/`](R/pipeline/) | The pipeline functions the paper uses. |
| [`runs/`](runs/) | One file per named rerun. |
| [`environment/`](environment/) | R and Python environment notes and pins. |
| [`build_gap_panel.R`](build_gap_panel.R) | Builds the fund-period panel underlying the human-vs-machine gap analysis, with point-in-time controls and leakage guards. |
| [`analysis_gap.R`](analysis_gap.R) | Gap regressions and correlations against the panel above. |
| [`plots.R`](plots.R) | Manuscript figures and tables. (`plots_and_tables.R` is a legacy script; do not use it.) |
| [`DESIGN_NOTE.md`](DESIGN_NOTE.md) | Working specification for the paper's current framing. |
| [`HYPERPARAMETER_GRID_COMPARISON.md`](HYPERPARAMETER_GRID_COMPARISON.md) | Model hyperparameter grids compared line-by-line against GKX, Hanauer & Kalsbach, and Rubesam. |
| [`COMDINHEIRO_NOTES.md`](COMDINHEIRO_NOTES.md) | Notes on the Comdinheiro data source and its API quirks. |
| [`HANDOFF.md`](HANDOFF.md) | Operational handoff state for whichever piece of work is in progress; not durable documentation. |
| `latex_paper/` | Manuscript source. |
| `data/`, `provenance/` | Gitignored: raw/derived data and pipeline manifest snapshots, both potentially confidential or too large for git. |

## Requirements

- R 4.3.0 and `renv::restore()`, which also installs the bundled factoRverse
  0.7.0.
- For model reruns, Python with Keras 2 (not 3): see
  [environment/README.md](environment/README.md).

## Usage

See [REPRODUCIBILITY.md](REPRODUCIBILITY.md). Model reruns always use a named
run, so the submitted results under `data/derived/machine_run/` are left intact:

```r
Sys.setenv(PAPER_RUN = "<name>")   # applies runs/<name>.R
source("run_paper_machine.R", encoding = "UTF-8")
```

## Citation

If you use this repository's methodology, please cite the paper above and,
for the underlying estimation machinery, `factoRverse` (`citation("factoRverse")`).

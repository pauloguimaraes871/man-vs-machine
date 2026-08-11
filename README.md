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
  builds and pins the gold data objects this repo reads. [run_paper_machine.R](run_paper_machine.R)
  re-runs the paper's machine on the pipeline's latest production manifest,
  under academic settings that deliberately diverge from deployment in a few
  documented ways (see the header comment of that script).

## Layout

| File | Role |
|---|---|
| [`run_paper_machine.R`](run_paper_machine.R) | End-to-end driver: reads the pipeline's production manifest and reproduces the paper's machine (signal screening → selection → blending → meta-ensemble → portfolios) end to end. Resumable; every stage is cached to disk. |
| [`build_gap_panel.R`](build_gap_panel.R) | Builds the fund-period panel underlying the human-vs-machine gap analysis, with point-in-time controls and leakage guards. |
| [`analysis_gap.R`](analysis_gap.R) | Gap regressions and correlations against the panel above. |
| [`comdinheiro_fundscreener.R`](comdinheiro_fundscreener.R) | Comdinheiro FundScreener data-collection helper. |
| [`plots.R`](plots.R), [`plots_and_tables.R`](plots_and_tables.R) | Manuscript figures and tables. |
| [`DESIGN_NOTE.md`](DESIGN_NOTE.md) | Working specification for the paper's current framing. |
| [`HYPERPARAMETER_GRID_COMPARISON.md`](HYPERPARAMETER_GRID_COMPARISON.md) | Model hyperparameter grids compared line-by-line against GKX, Hanauer & Kalsbach, and Rubesam. |
| [`COMDINHEIRO_NOTES.md`](COMDINHEIRO_NOTES.md) | Notes on the Comdinheiro data source and its API quirks. |
| [`HANDOFF.md`](HANDOFF.md) | Operational handoff state for whichever piece of work is in progress; not durable documentation. |
| `latex_paper/` | Manuscript source. |
| `data/`, `provenance/` | Gitignored: raw/derived data and pipeline manifest snapshots, both potentially confidential or too large for git. |

## Requirements

- R with [`factoRverse`](https://github.com/pauloguimaraes871/factoRverse)
  installed; see that package's README for the pinned `renv` lockfile it
  ships with each release.
- `renv::restore()` in this repo restores the analysis-side dependencies
  (`renv.lock`).
- `run_paper_machine.R`'s signal-blending stage uses `factoRverse`'s
  neural-network blender, which needs a Python environment with Keras 2 (not
  3) — see that script's header comment for how the interpreter is resolved.

## Usage

```r
source("run_paper_machine.R")   # runs main(), honouring STOP_AFTER inside the script
```

Every stage (screen, probe, signal selection, signal blending, meta-ensemble,
meta portfolios) is cached under `data/derived/machine_run/`, so a rerun
skips whatever already exists.

## Citation

If you use this repository's methodology, please cite the paper above and,
for the underlying estimation machinery, `factoRverse` (`citation("factoRverse")`).

# Handoff: `build_gap_panel.R`

## Objective

Build a reproducible fund-period panel for the human-versus-machine gap
analysis, with temporally valid controls, defensive input checks, and a test
suite aimed at alignment and leakage failures.

## Branch and scope

- Branch: `codex/add-gap-panel-scaffold`
- Baseline commit: `1e175fd feat: add human-machine gap panel scaffold`
- Implementation commit: `81c191a fix: harden gap panel timing and validation`
- Relevant files: `build_gap_panel.R`, `DESIGN_NOTE.md`, and `tests/`.
- Do not include the unrelated `latex_paper/manuscript.pdf` modification or
  untracked `provenance/` directory in this work.

## Implemented changes

### Timing and aggregation

- The panel labels each observation by the return-window closing 15th.
- AUM and investor counts are sampled at the opening 15th, so they cannot
  contain the window's realised return or flows.
- Net flows are summed over `(opening 15th, closing 15th]`. Fund-level windows
  with no flow observations remain `NA`; they are not imputed as zero.
- `flow_rate_lag` and `flow_vol_12` are strictly lagged. The flow-rate
  denominator is the opening AUM.
- Manager and industry aggregations use an explicit `sum_or_zero()` policy:
  an all-missing manager-period aggregate is zero, as approved, while a
  fund-period flow with no information remains missing.
- Daily level sampling rejects observations older than `MAX_LEVEL_AGE_DAYS =
  7`, preventing stale AUM from silently entering the panel.

### Holdings and concentration

- Month-end holdings snapshots map to the first complete future
  15th-to-15th return window, avoiding partial-window look-ahead.
- Top-X holdings inputs validate ranks and descending order while retaining
  signed weights as possible short positions. Duplicate headers are accepted
  only when at most one duplicate column contains data; the current repeated
  `S0001198076` columns are blank and are dropped.
- Top-15 positive exposure above 110% issues a warning rather than ruling out
  leverage. NAV-based residual, minimum-position, and effective-bets bounds
  are withheld for short books or positive exposure above 102%.
- The current source has 18 malformed fund-month snapshots (absurd absolute
  weights above 1,000% or broken rank ordering). They are explicitly excluded
  from concentration measures with a warning, never converted to zero.

### Fund identity and classification

- Code reconciliation is explicit across all four daily panels; no positional
  alignment is used.
- Duplicate screening now finds transitive return-correlation components. It
  retains every legal vehicle for AUM and flow aggregation; representative
  selection remains optional and is only for strategy-return analyses.
- The static fund table now retains `fund_name` and contains an owner-supplied
  validation set of 20 known quantitative/factor funds, keyed by ANBIMA code.
  These labels validate the broad name-based quant diagnostic; they are not a
  paper classifier or a headline estimand.

### Rolling return routines and production entry point

- Rolling factor routines require aligned factor periods and default to a
  36-month trailing history for risk-adjusted gaps. Realised residual gaps,
  rather than overlapping rolling alphas, are the intended dependent variable.
- `build_gap_panel.R` is standalone: `read_ml_index()` loads the pinned ML
  backtest result from `data/meta_port_ind_signals`, restores serialised dates,
  and validates consecutive calendar-15th endpoints before building the panel.
- Tests set `options(gap_panel.run = FALSE)`, so sourcing helpers loads
  functions without triggering the 72 MB production build. A normal direct
  `source("build_gap_panel.R")` still runs the build.
- `methods::hasSlot()` was replaced because it is not exported. Slot presence
  now uses `methods::slotNames()` alongside `base::isS4()`.
- Removed the obsolete `stop("RONALDO")` debugging stop.

## Tests

The test suite is in `tests/testthat/` and covers:

- identifier reconciliation independent of source column order;
- owner-supplied quant-code labels;
- opening-date AUM and flow timing;
- stale AUM rejection and missing-flow preservation;
- all-missing manager aggregation;
- transitive duplicate clusters while retaining legal fund identities;
- duplicate headers, short books, malformed, and levered concentration inputs;
- 36-month rolling-factor eligibility and factor alignment;
- ML endpoint validation.

The owner ran `source("tests/testthat.R")` successfully before the final
standalone-ML-index refinement. R is not available on this agent's PATH, so
the final regression run must be performed in the owner's R installation:

```r
source("tests/testthat.R")
source("build_gap_panel.R")
```

## Next action

1. Run the two R commands above in an interactive R session. The agent's
   headless R runner stalls while activating `renv`, before any tests begin.
2. Review the panel output and decide whether to uncomment the two `qs::qsave`
   lines at the end of `build_gap_panel.R` for durable derived-data output.
3. Keep `latex_paper/manuscript.pdf` and `provenance/` outside any follow-up
   panel commit unless the owner deliberately expands scope.

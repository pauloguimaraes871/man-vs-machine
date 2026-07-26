# HANDOFF: Independent review of the plots, tables, and manuscript objects

## Purpose and evidence labels

This handoff is for Claude to review the current uncommitted implementation in
`plots.R`, the generated tables and figures, and their integration into
`latex_paper/manuscript.tex`.

Claims use the following labels:

- **[FACT]** Directly observed in code, generated output, data headers, command
  output, or the compiled PDF.
- **[INFERENCE]** An economic or statistical interpretation of those facts.
- **[JUDGMENT]** A defensible design choice, but not a fact established by the
  output.
- **[OPEN REVIEW ITEM]** A specific issue Claude should validate or resolve.

The manuscript prose is known to be stale. The current review priority is the
return pipeline, plots, generated tables, and the new analyses.

## Git state and approved scope

- **[FACT]** Current branch: `main`.
- **[FACT]** Current HEAD: `4bef2e6 Initialize project with 15th-to-15th return analysis`.
- **[FACT]** The repository owner explicitly requested that this work remain on
  `main`, overriding the repository's usual default-branch restriction.
- **[FACT]** All work described here is uncommitted. Review the working-tree diff
  against `HEAD`; do not assume a commit boundary exists.
- **[FACT]** The working tree already contained the earlier Claude implementation.
  Codex edited it in place and did not revert unrelated work.

Primary edited source files:

- `plots.R`
- `latex_paper/manuscript.tex`
- `HANDOFF.md`

Generated or regenerated objects include:

- `tables/active_performance.tex`
- `tables/model_performance.tex`
- `tables/factor_attribution.tex`
- `tables/combination_frontier.tex`
- `tables/fund_picker_robustness.tex` (new)
- `tables/spanning_tests.tex` (new)
- existing PNGs under `figures/` and `latex_paper/figures/`
- `figures/fund_picker_quantile_sensitivity.png` (new)
- `figures/fund_picker_basket_size.png` (new)
- `figures/fund_picker_precision.png` (new)
- `figures/fund_picker_robustness_figures.tex` (new)
- `latex_paper/manuscript.pdf`

## Superseded statements from the old handoff

The previous `HANDOFF.md` described an older implementation. The following are
no longer true:

- **[FACT]** Fund returns are no longer calendar-month returns from
  `bases_fundos.xlsx`. They are compounded from daily returns to the ML
  backtest's calendar-15th endpoints.
- **[FACT]** The Dimson lead/lag market adjustment was removed. Fund, ML, IBOV,
  CDI, and factor returns now share the same 15th-to-15th convention.
- **[FACT]** `restore_meta_xts_dates()` is now defined near the start of
  `plots.R`; it is not an external interactive-session helper.
- **[FACT]** The full pipeline completed from `Rscript --vanilla` in the current
  environment, so the formerly described interactive-session setup is stale.
- **[FACT]** Generated figure PNGs are copied into `latex_paper/figures/` by the
  final section of `plots.R`; manual copying is no longer required.
- **[FACT]** CDI covers every evaluated endpoint in the current daily file; the
  code now aborts if either CDI or IBOV is missing for an evaluation period.

## Return ingestion and timing

### Current implementation

- **[FACT]** Inputs are:
  - `data/bases_fundos_diarios.xlsx`
  - `data/ibov_diarios.xlsx`
  - `data/CDI_diarios.xlsx`
- **[FACT]** Each workbook currently has one sheet named `459666`.
- **[FACT]** Current workbook inspection found metadata in rows 1-3, headers in
  row 4, Excel serial dates in the first column, percentage returns thereafter,
  and `"-"` for missing values.
- **[FACT]** The current IBOV and CDI headers identify the source as
  `Economatica`, despite the earlier warning that CDI came from a different
  provider. Sample CDI daily values are approximately `0.0525`, which the parser
  treats as percent per day.
- **[FACT]** `read_economatica_daily_returns()` skips three rows, requires at
  least one return column, converts `"-"` to `NA`, checks unique sorted dates,
  extracts the last newline-delimited header component as the series ID, and
  rejects nonnumeric values and returns below -100 percent.
- **[FACT]** `compound_daily_returns_15th()` requires consecutive endpoints that
  all fall on the calendar 15th. For endpoint `t`, it geometrically compounds
  simple daily percentage returns over `(t-1, t]`:
  `prod(1 + r_d / 100) - 1`.
- **[FACT]** The monthly endpoint vector comes directly from the ML benchmark
  index.
- **[FACT]** Daily-compounded IBOV is reconciled against the stored ML benchmark
  and the script aborts if the maximum difference exceeds 1 basis point.
- **[FACT]** IBOV and CDI must have a finite compounded return for every
  evaluation period.
- **[FACT]** Funds require at least 36 nonmissing compounded monthly returns to
  enter the common eligible universe.
- **[FACT]** Current generated outputs imply 1,128 eligible funds and 282
  top-quartile funds.

### Edge cases for review

- **[FACT]** Within a monthly period, missing daily fund observations are simply
  removed before compounding. One finite daily return is enough to produce a
  monthly return; there is no minimum-day or endpoint-coverage requirement.
- **[INFERENCE]** Launch, closure, suspension, or partial-reporting months may
  therefore be treated as valid partial-period returns. Claude should determine
  whether the Economatica export semantics make this harmless or whether those
  boundary months should be excluded.
- **[FACT]** The parser validates structure and numeric plausibility but does not
  prove that a column contains returns rather than levels, nor whether units are
  percent rather than decimal.
- **[OPEN REVIEW ITEM]** Independently verify CDI provenance, units, and whether
  the first worksheet is the intended series. Passing structural checks is not
  semantic validation.
- **[FACT]** If IBOV or CDI contains multiple return columns, only the first is
  used. No explicit single-series assertion is made.
- **[FACT]** Returns equal to -100 percent and arbitrarily large positive returns
  pass the current range check.
- **[FACT]** `rowMeans(..., na.rm = TRUE)` can produce `NaN` if every selected
  fund is missing in a month. The later fund-picker helper converts nonfinite
  values to `NA`, but the initial `FUNDS_MEAN` and `FUNDS_Q75` construction does
  not explicitly normalize or abort on an all-missing month.

## Fees, costs, and units

- **[FACT]** ML return objects are already the backtest's `net_returns` objects,
  which are treated as net of transaction costs.
- **[FACT]** A 1 percent annual management fee is converted geometrically to a
  monthly fee:
  `(1 + 0.01)^(1/12) - 1`.
- **[FACT]** The monthly ML portfolio return is adjusted as
  `(1 + r/100)/(1 + monthly_fee) - 1`.
- **[FACT]** The fee is not deducted from the IBOV benchmark.
- **[FACT]** Both ensemble-scheme returns and model-level returns used in
  return-based plots/tables receive the fee deduction.
- **[FACT]** `ml_ports_net_m_xts` is the downstream ensemble return input for
  performance, cumulative returns, correlations, combinations, fund-picker
  exercises, and spanning.
- **[JUDGMENT]** A constant monthly equivalent of a 1 percent annual fee is a
  transparent convention for monthly 15th-to-15th periods.
- **[INFERENCE]** Human fund returns are being treated as investor net returns.
  That is economically plausible for reported quota returns, but the workbook
  metadata inspected here does not independently establish the treatment of all
  fund-level fees.
- **[OPEN REVIEW ITEM]** Confirm that the stored ML `net_returns` series already
  deduct transaction costs exactly once. The current script assumes this rather
  than recomputing transaction costs.

## Human-manager estimands and aggregation

- **[FACT]** Daily returns are geometrically compounded into monthly returns.
- **[FACT]** Funds are then ranked by the arithmetic mean of those monthly
  percentage returns over the evaluation window (`colMeans`).
- **[OPEN REVIEW ITEM]** Some figure language says "full-window geometric
  return." If the intended ranking statistic is full-window compounded or
  annualized geometric return, the implementation does not currently do that.
  If "geometric" only describes daily-to-monthly aggregation, clarify the
  caption.
- **[FACT]** The main Q75 threshold uses `quantile(score, 0.75)` and includes
  scores greater than or equal to the threshold. Ties can make the retained
  fraction differ slightly from exactly 25 percent.
- **[FACT]** Oracle sensitivity uses a sorted list and retains
  `ceiling(top_share * N)`, so its tie behavior differs from the main quantile
  threshold.
- **[FACT]** `FUNDS_MEAN` is the monthly mean return among available eligible
  funds.
- **[FACT]** `FUNDS_Q75` is the monthly mean return among available ex-post
  top-quartile funds.
- **[FACT]** These are "metric of an aggregated return series" estimands and
  benefit from cross-fund diversification.
- **[FACT]** `MED` and `MED_Q75` calculate each metric per fund and then report
  the cross-fund median. They are not tradable aggregate portfolios.
- **[FACT]** Median aggregation is also used for individual-fund combination
  results.
- **[JUDGMENT]** Median is preferable to mean for cross-fund nonlinear ratios
  because near-zero denominators can dominate the mean.
- **[FACT]** Fixed "equal-weight" baskets dynamically renormalize over reporting
  funds through `rowMeans(..., na.rm = TRUE)`. They are fixed membership
  baskets, not fixed weights on unavailable constituents.
- **[INFERENCE]** This is defensible as an available-fund portfolio, but captions
  should not imply that unavailable fund weights remain in cash.

## Combination frontier

- **[FACT]** The combination grid is deterministic:
  `w = 0, 0.05, ..., 1`, where `w` is the ML weight.
- **[FACT]** Sharpe ratios use CDI; Information Ratios and active frontiers use
  IBOV. All inputs now use 15th-to-15th returns.
- **[FACT]** For a missing single-fund return, `blend()` allocates that month
  entirely to ML, regardless of the nominal grid weight.
- **[FACT]** Consequently, `w = 1` is exactly standalone ML, while `w = 0` in a
  single-fund row means "fund when reporting, ML otherwise," not a pure fund
  history.
- **[FACT]** When several grid values tie for a maximum, `which.max()` chooses
  the first, which is the lowest ML weight because the grid is ascending.
- **[JUDGMENT]** A deterministic grid is preferable to random two-asset weights:
  the feasible set is one-dimensional and the grid includes both endpoints.
- **[OPEN REVIEW ITEM]** Decide whether 5-percentage-point resolution is
  sufficient or whether reported optima should be checked on a finer grid.
- **[FACT]** Table labels were shortened to `MED` and `MED_Q75` to prevent line
  breaks.
- **[FACT]** Figure 12's caption was shortened substantially.

## Plot and table corrections

- **[FACT]** Shared plot base text size is 19 points.
- **[FACT]** Figure 4 uses a local base size of 16, 14-point axis text, 12-point
  legend text, and a widened horizontal color bar to avoid overlap.
- **[FACT]** Figure 5 removes `_bayes` from displayed base-learner names by
  mutating the data in the `ggplot2::last_plot()` object returned by
  factoRverse.
- **[OPEN REVIEW ITEM]** The Figure 5 adjustment depends on factoRverse plot
  internals (`last_plot()`, a `name` column, and text layers). It currently works
  but is version-fragile.
- **[FACT]** Figure 6 now has two panels only: surrogate intercept and surrogate
  `R^2`. The residual-variance panel was removed and `R^2` is rendered with a
  superscript through `label_parsed`.
- **[FACT]** Figure 11 now includes both `AVG_FUND` and `AVG_FUND_Q75`.
- **[FACT]** These are correlation summaries, not return series. Their diagonal
  entries are average distinct within-cohort fund correlations and need not
  equal one.
- **[FACT]** Table 3's return column is labeled `Ret.` rather than
  `Ann. Act. Geom. Ret.`.
- **[FACT]** Generated captions for the new analyses were expanded to define
  samples, uncertainty, optimization, and ex-post assumptions.

## Fund-picker analyses

### Design facts

- **[FACT]** `primary_ml_scheme <- "RP"` is fixed in code.
- **[JUDGMENT]** RP was chosen as the main anchor because it is a transparent,
  risk-controlled construction, not because it was selected as the ex-post best
  performer.
- **[FACT]** Figure 13 still reports all eight ML construction schemes, so the
  quantile pattern is not shown only for RP.
- **[FACT]** The RP bootstrap uses 999 circular 12-month block samples with seed
  `20260727`; funds are re-ranked inside every resample.
- **[FACT]** Figure 14 uses 1,000 without-replacement basket draws per size, seed
  `20260728`, except the "all 282" basket, which is deterministic.
- **[FACT]** Figure 15 uses basket sizes 4, 8, 20, and 40; precision values 0,
  25, 50, 75, and 100 percent; 500 draws per cell; and seed `20260729`.
- **[FACT]** The past-only diagnostic uses the preceding 12 months, requires at
  least 9 finite observations in that history, and evaluates the next month.
- **[FACT]** The global fund universe for that past-only diagnostic is still
  restricted to funds with at least 36 observations over the full evaluation
  sample.
- **[INFERENCE]** The past-only ranking has no direct return look-ahead, but the
  full-sample 36-observation eligibility screen is not fully point-in-time.

### Numerical results

- **[FACT]** At the ex-post top quartile, RP-human IRs are approximately:
  human `0.58`, RP `0.49`, best combination `0.59`, RP weight `20%`, and
  incremental IR about `0.008` before table rounding.
- **[FACT]** The 999-sample RP top-quartile interval for incremental IR is
  approximately `[0.000, 0.059]`, displayed as `[0.00, 0.06]`.
- **[FACT]** The median incremental IR in the basket-size experiment is about
  `0.01` from basket size 5 onward; the fraction of draws with a strictly
  positive increment rises from 47 percent for one fund to 100 percent for all
  282 top-quartile funds.
- **[FACT]** Under the current threshold code, every reported basket size needs
  100 percent picker precision for positive median incremental IR.
- **[FACT]** The past-only row reports human IR `0.25`, best IR `0.72`,
  incremental IR rounded to `0.00`, and RP weight `100%`.

### Interpretation and edge cases

- **[INFERENCE]** The oracle figures suggest that most of the apparent human
  advantage comes from knowing future winners, while implementable trailing
  selection does not improve on RP in this specification.
- **[INFERENCE]** This is a robustness/calibration result, not evidence that all
  realistic fund-selection methods fail.
- **[FACT]** Because both endpoints are in the grid, incremental IR is
  nonnegative by construction, apart from numerical noise.
- **[INFERENCE]** A percentile interval with lower bound zero is therefore not a
  conventional two-sided significance interval. `P(delta > 0)` measures the
  frequency of a strict interior improvement under the experiment, not the
  probability that an unconstrained population effect is positive.
- **[OPEN REVIEW ITEM]** No numerical tolerance is used for `delta > 0`.
  Near-ties can count as gains even if they round to `0.00`.
- **[OPEN REVIEW ITEM]** The Table 6 caption defines "Min. likely" as requiring
  `P(delta IR > 0) >= 75%`, but the code at `plots.R` lines approximately
  2213-2215 uses `>= 0.5`. Resolve the intended threshold and regenerate.
- **[JUDGMENT]** Re-ranking funds inside the block bootstrap is preferable to
  conditioning on the original ranking because it includes ranking
  instability.
- **[FACT]** The bootstrap uses percentile bands and re-optimizes the weight in
  each sample; it is not studentized and does not correct for maximization bias.

## Mean-variance spanning

### Design facts

- **[FACT]** The regression is `Y_t = alpha + B X_t + epsilon_t`.
- **[FACT]** The joint null for each test asset is `alpha = 0` and
  `B 1 = 1`.
- **[FACT]** "ML spans human" uses RP, CS, and MVO_BOX as benchmark assets and
  `FUNDS_MEAN`, `FUNDS_Q75` as test assets.
- **[FACT]** "Human spans ML" reverses those sets.
- **[JUDGMENT]** RP, CS, and MVO_BOX were selected ex ante as low-dimensional
  representatives of risk-controlled, direct signal-weighted, and constrained
  mean-variance constructions. This is a modeling choice, not an empirically
  proven sufficient representation of all ML portfolios.
- **[FACT]** The asymptotic test uses the standard system covariance implied by
  common regressors.
- **[FACT]** The bootstrap uses 999 restricted-residual circular 12-month
  replications with seeds `20260730` and `20260731`.
- **[FACT]** Benchmark returns and the benchmark design are held fixed while
  restricted test-asset residuals are resampled.
- **[FACT]** Long-only maximum Sharpe and Information Ratios are estimated with
  SLSQP from the equal-weight start and each vertex.

### Numerical results

- **[FACT]** ML spans human: Wald `71.69`, df `4`, asymptotic p-value below
  displayed precision, bootstrap p-value `0.001`, design condition number
  `13.90`, incremental maximum long-only Sharpe `0.028`, incremental maximum
  long-only IR `0.095`.
- **[FACT]** Human spans ML: Wald `28.90`, df `6`, asymptotic p-value below
  displayed precision, bootstrap p-value `0.001`, design condition number
  `16.89`, incremental maximum long-only Sharpe `0.000`, incremental maximum
  long-only IR `0.008`.

### Interpretation and edge cases

- **[INFERENCE]** Under the selected assets and full-window sample, both nulls
  are rejected, so neither selected set spans the other.
- **[INFERENCE]** This does not establish that the complete universe of ML
  portfolios and human managers are mutually non-spanning. The conclusion is
  conditional on the chosen representatives.
- **[FACT]** `FUNDS_Q75` is ex-post, so the human opportunity set contains an
  oracle object.
- **[INFERENCE]** The resulting spanning comparison is best described as an
  opportunity-set diagnostic or upper-bound exercise, not a fully investable
  test.
- **[OPEN REVIEW ITEM]** Assess whether conditioning on observed benchmark
  returns in the residual bootstrap is the desired inferential design. A joint
  block bootstrap of all returns or a HAC/GMM spanning test would answer a
  somewhat different question.
- **[FACT]** `max(0, expanded - benchmark)` is applied to reported ratio gains.
  The expanded opportunity set is theoretically nested, so negative gains
  should only arise from numerical optimization.
- **[OPEN REVIEW ITEM]** The ratio objective is nonconvex. The current
  equal-weight-plus-vertices starts are sensible but do not prove a global
  optimum. Check with a denser set of deterministic starts if these economic
  deltas are emphasized.

## Manuscript integration and numbering

- **[FACT]** `latex_paper/manuscript.tex` now uses `\input{...}` for generated
  table and figure snippets rather than embedding large generated blocks.
- **[FACT]** New objects are included in the compiled manuscript.
- **[FACT]** The compiled PDF is 58 pages.
- **[FACT]** Visual inspection covered PDF pages 35-37, 44-46, and 50-52,
  including Figures 4-6, Figures 13-15, the combination table, Table 6, and
  Table 7.
- **[FACT]** No clipping or overlapping text was observed on those pages.
- **[OPEN REVIEW ITEM]** Generated optional caption arguments contain hard-coded
  table/figure numbers, while LaTeX also auto-numbers floats. For example, the
  active-performance snippet's optional caption says "Table 3" but the compiled
  PDF currently displays it as Table 2. Prefer removing hard-coded numbers from
  optional captions or making the generation order authoritative.
- **[FACT]** The manuscript still emits pre-existing warnings for multiply
  defined labels, overfull boxes, float relocation, and MiKTeX update/log
  housekeeping.
- **[FACT]** Manuscript prose was only minimally adjusted for object references.
  A full narrative rewrite was explicitly deferred.

## Reproduction and verification performed

R executable used:

```powershell
C:\Users\paulo.fonteles\AppData\Local\Programs\R\R-4.3.0\bin\Rscript.exe
```

Pipeline command:

```powershell
& 'C:\Users\paulo.fonteles\AppData\Local\Programs\R\R-4.3.0\bin\Rscript.exe' `
  --vanilla -e ".libPaths(c('renv/library/R-4.3/x86_64-w64-mingw32', .libPaths())); source('plots.R')"
```

LaTeX command, run twice from `latex_paper/`:

```powershell
pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex
```

Verification record:

- **[FACT]** `plots.R` parsed successfully.
- **[FACT]** The full R pipeline completed successfully multiple times after the
  final edits.
- **[FACT]** Expected-shortfall warnings appeared for one fund series; these
  were warnings from the performance function, not fatal pipeline errors.
- **[FACT]** Both final `pdflatex` passes completed and wrote the 58-page PDF.
- **[FACT]** `git diff --check` reported no whitespace errors, only LF-to-CRLF
  conversion warnings for `plots.R` and `manuscript.tex`.
- **[FACT]** No `bibtex` pass was run during the final verification because the
  current changes did not modify citations.

## Claude review checklist, in priority order

1. Resolve the 75 percent caption versus 50 percent code mismatch for
   `min_precision_likely`.
2. Confirm whether fund ranking should use arithmetic mean monthly return or
   full-window geometric return, then align captions and code.
3. Validate the new CDI file semantically: provider, units, first intended
   series, and daily-return interpretation.
4. Review partial-month fund-return handling and decide whether a minimum daily
   coverage/endpoints rule is required.
5. Audit every return-based table and plot to confirm it derives from
   fee-adjusted ML returns and that transaction costs are not deducted twice.
6. Recheck fund aggregation semantics: dynamic available-fund averaging,
   ex-post Q75 membership, cross-fund medians, and missing-fund allocation to ML.
7. Review the oracle bootstrap interpretation, strict `delta > 0` comparison,
   and whether a tolerance or different interval is warranted.
8. Review the past-only exercise for point-in-time universe eligibility; the
   ranking is lagged, but the 36-observation universe screen is full-sample.
9. Independently validate both spanning directions, the restricted-residual
   bootstrap, and long-only ratio optimization.
10. Remove hard-coded float numbers from generated optional captions or verify
    all numbering after manuscript reordering.
11. Re-run the full pipeline, compile twice, and visually inspect the affected
    PDF pages before accepting generated binary changes.

## Single next action

Claude should begin with the two concrete code/documentation mismatches in items
1 and 2 above, then perform an independent quantitative review of the daily
return and spanning implementations before any commit is created.

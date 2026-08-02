# HANDOFF: Status after Claude's review of Codex's 15th-to-15th rewrite

## Purpose and evidence labels

This handoff originally asked Claude to independently review Codex's rewrite
of the return pipeline, the new fund-picker and spanning analyses, and their
integration into `latex_paper/manuscript.tex`. That review is now complete:
every item Codex flagged has been independently verified, and every
correctness or presentation issue found along the way has been fixed and
committed. This document now records what was checked, what changed, and what
is left for the repository owner to decide. It is not yet retired to
`DESIGN_DECISIONS.md` because the prose in Section 5 still needs a rewrite
against the corrected numbers (see "What is still open" below).

Claims use the following labels:

- **[FACT]** Directly observed in code, generated output, data headers, command
  output, or the compiled PDF.
- **[INFERENCE]** An economic or statistical interpretation of those facts.
- **[JUDGMENT]** A defensible design choice, but not a fact established by the
  output.
- **[RESOLVED]** An item the earlier handoff flagged for review; the review
  is complete and the resolution is stated.
- **[OPEN]** Still unresolved; requires further work or an owner decision.

## Git state

- **[FACT]** Current branch: `main`. Working tree clean except for a byte-level
  diff in `latex_paper/manuscript.pdf` (see "Loose end" below) and the
  pre-existing untracked `provenance/` directory, which predates this review
  and was not created or touched by it.
- **[FACT]** Three commits landed on `main` during this review, in order:
  1. `7ed54ef fix: correct fund-picker threshold, spanning interpretation, and frontier consistency`
  2. `ee987ff docs: correct ranking caption, delta-IR interval label, and float numbering`
  3. `17f8b79 fix: replace stale grid-search hyperparameter table with Bayesian-opt bounds`
- **[FACT]** All three are on `main` directly, per the repository owner's
  standing instruction for this project.

## What Codex changed (context for the numbers below)

- **[FACT]** Fund returns are compounded from daily returns
  (`bases_fundos_diarios.xlsx`, `ibov_diarios.xlsx`, `CDI_diarios.xlsx`) to the
  ML backtest's own calendar-15th endpoints, replacing the earlier calendar-month
  convention.
- **[FACT]** The Dimson lead/lag market adjustment was removed. Fund, ML, IBOV,
  CDI, and factor returns now share one 15th-to-15th timing convention, which
  made the correction unnecessary rather than wrong.
- **[FACT]** Two new analyses were added: fund-picking robustness (Table 6,
  Figures 13-15) and two-direction mean-variance spanning (Table 7).
- **[FACT]** `latex_paper/manuscript.tex` now uses `\input{...}` for every
  generated table and figure rather than embedding large generated blocks.

## Independently verified and closed without code changes

These were open review items in the prior handoff. Verification was done by
re-deriving each number from the raw daily files or by reimplementing the
statistic independently, not by re-reading the code that produced it.

- **[RESOLVED]** CDI provenance and units. Single column `CDI Acumulado`,
  2,219 daily observations, zero negative days, mean ~0.033%/day (~8.8% p.a.),
  consistent with a Brazilian money-market daily rate correctly parsed as
  percent. No further action needed.
- **[RESOLVED]** Partial-month fund-return handling. Across all 82,617
  observed fund-months, median daily coverage is 95.7% of trading days; only
  0.5% of fund-months fall below 50% coverage and 0.13% have 3 or fewer days.
  Low-coverage months are not concentrated in the top quartile (0.54% vs 0.72%
  fundwide). A minimum-coverage rule would exclude a negligible, non-biasing
  slice of the data; none was added.
- **[RESOLVED]** IBOV reconciliation. Daily-compounded IBOV matches the ML
  backtest's stored benchmark to within 0.1 basis point (max abs diff
  0.001 percentage points across 102 periods). The 15th-to-15th rewrite is
  correct, not merely internally consistent.
- **[RESOLVED]** Spanning Wald statistics. Independently reimplemented via
  per-equation OLS plus the SUR covariance kronecker(Sigma, (X'X)^-1); matches
  the code's `spanning_wald_stat()` exactly (71.6910/df 4 and 28.9039/df 6).
- **[RESOLVED]** The reported `Delta SR = 0.000` for "ML spans human" is a
  genuine corner solution (both optimizations place 100% weight on
  `FUNDS_Q75`), not a masked optimizer failure. A 200,000-draw random-simplex
  search over the same asset set found nothing better than the SLSQP result.
- **[FACT, corrected]** The prior handoff stated 1,128 eligible funds. The
  actual count under the 36-observation screen is **1,127**; the 282
  top-quartile count (25% of 1,127, rounded up) was already correct and is
  unaffected.

## Fixes applied and committed

### `7ed54ef` — four correctness/interpretation fixes

1. **Fund-picker Panel C threshold.** The caption defined "Min. likely" as
   `P(Delta IR > 0) >= 75%`; the code used `>= 0.5`. Worse, because
   `Delta IR >= 0` by construction (the weight grid contains both endpoints),
   `median(Delta) > 0` and `P(Delta > 0) >= 0.5` are the same test, so the two
   table columns were reporting one number twice ("100% / 100%" in every row).
   Fixed the code to `>= 0.75`, so the two columns now diverge. New Table 6
   values: baskets of 4 and 8 no longer clear the bar ("--"); baskets of 20 and
   40 do.
   - **[OPEN, minor]** Basket size 20 clears the new bar at P = 0.754 against a
     0.750 threshold, which is inside Monte Carlo noise at 500 draws/cell
     (SE approx 0.019). The qualitative conclusion (small baskets fail even at
     perfect precision) is solid; that one cell's classification could flip
     under a different seed. Raising `precision_draws_per_cell` (currently 500)
     to ~5,000 would settle it cheaply if the exact cutoff matters.
2. **Spanning-test interpretation.** Added per-test-asset intercepts and their
   t-statistics beneath Table 7, computed from the same OLS fit
   (`resid_vcov <- crossprod(residuals) / (n - k)`, intercept SE from
   `sqrt(diag(resid_vcov) * xtx_inv[1,1])`); cross-checked against `lm()` and
   matches exactly. This surfaced that "ML spans human" is driven by
   `FUNDS_MEAN`'s significantly *negative* intercept (alpha = -0.31%/month,
   t = -2.46), not by human managers adding value; and that in "Human spans
   ML" no individual ML portfolio has a significant intercept (t in
   [-0.30, 1.02]), so that rejection is driven by the `B*1 = 1` scaling
   restriction rather than abnormal return. Caption reframed to lead with
   Delta SR / Delta IR as the economically relevant, long-only-consistent
   quantities.
3. **IR-convention reconciliation.** Table 3 (`\ref{actret}`) reports IR as
   geometric annualized active return / TE; the combination (Table 5) and
   fund-picker (Table 6) tables use the arithmetic-mean convention. Both are
   defensible, but the same portfolio can carry two different IRs across
   tables (e.g. RP: 0.58 in Table 3, 0.49 in Table 6). Added a caption sentence
   to each of the three tables stating its convention and cross-referencing
   the others; no numbers changed.
4. **Frontier figure/table consistency.** `combo_metrics()` reported `ir` as
   the median across funds of each fund's own IR, but plotted
   `(active_risk, active_ret)` as separate medians of the components. For the
   two `FUNDS_MEAN`/`FUNDS_Q75` panels (one manager series) these coincide; for
   the two `MED_FUND`/`MED_FUND_Q75` panels (282 funds) they diverge by up to
   0.04 IR, so the plotted maximum-IR diamond did not always sit on the drawn
   curve. Added `ir_plot = median(active_ret) / median(active_risk)` and moved
   the diamond to `slice_max(ir_plot)`; `ir` (used in Table 5) is untouched.
   Also stopped collapsing the `w=0` "pure manager" point across ML schemes:
   because a fund's non-reporting months are reallocated to that scheme's own
   ML return, the `w=0` point genuinely differs by scheme (6 distinct values
   across 8 schemes in the `MED_FUND_Q75` panel). The point now carries the
   scheme colour instead of being forced black, and the caption explains the
   multiplicity.

### `ee987ff` — three presentation fixes

5. **Ranking-statistic caption.** Figure 13 claimed funds were ranked by
   "full-window geometric return"; `rank_funds()` actually uses the arithmetic
   mean of monthly returns. Rather than just correcting the wording, added two
   diagnostics computed in `plots.R` and injected into the caption: 94% of
   top-quartile membership is unchanged under a geometric ranking, and 50% of
   top-quartile funds (142 of 282) are ranked on a shorter history than the
   full window because the 36-observation eligibility screen admits partial
   histories. Table 3's caption gained a pointer to this, since `FUNDS_Q75` is
   first defined there.
   - **[OPEN]** The underlying issue (unequal history lengths competing on the
     same arithmetic-mean rank, with the top quartile also running higher
     volatility: mean vol 7.36 vs 6.96 fundwide) is now disclosed but not
     corrected. A length-adjusted or geometric ranking is a defensible
     alternative; changing it would move `FUNDS_Q75` throughout the paper and
     is an owner decision, not something to change unilaterally.
6. **Delta-IR interval label.** Panel A's column was headed "95% CI", which
   invites a significance reading the statistic cannot support (`Delta IR`
   is non-negative by construction and further inflated by maximizing over the
   weight grid, so a lower bound of 0.00 tests no null). Renamed to "95% band"
   and the caption now states this explicitly.
7. **Hard-coded float numbers.** Every generated caption
   (`\caption[Table 3: ...]`, `\caption[Figure 7: ...]`, etc.) carried a
   hand-maintained number in its optional argument, separate from LaTeX's own
   auto-numbering. Four of six table numbers were already stale (a float had
   been inserted ahead of them) and two different tables both claimed
   "Table 6". This was invisible only because the manuscript has no list of
   tables/figures to typeset the optional argument; adding one would have
   surfaced six wrong numbers at once. Removed the numbers; the short caption
   is now the optional argument. `latex_figure()` lost its `fig_num`
   parameter; all 15 call sites updated. LaTeX-assigned numbers are unchanged
   (tables 2-7, figures 1-15).
   - Found and fixed two related bugs while verifying this: Figure 13's panels
     rendered alphabetically instead of RP-first, because the ribbon layer
     supplied `scheme` as a character while the main layer supplied a factor
     (`facet_wrap()` falls back to alphabetical when a facet variable
     disagrees in type across layers); and Figure 15's cell labels were at two
     decimals while the underlying values span 0-0.012, collapsing every
     100%-precision cell to "0.01" regardless of basket size. Both fixed;
     Figure 15 now shows its actual gradient (0.005/0.008/0.011/0.012 across
     basket sizes 4/8/20/40).

### `17f8b79` — stale hyperparameter table replaced

8. **Table 13 (`\label{hyperchoice}`), "Candidate values for hyperparameters",
   still described grid-search values from before the pipeline switched to
   Bayesian optimization** (e.g. RF trees fixed at 500; XGB depth in
   `{2,4,8,10}`), and 9 of its 17 symbols were never defined anywhere in the
   prose. Replaced with a table **generated from
   `tuning_strategy@hyper_grid_domain`**, read directly off each fitted base
   learner's stored config, so the bounds cannot drift from what was actually
   run. Reports min/max per hyperparameter, the search budget (initial design
   points, acquisition iterations), the acquisition function (EI), and the
   objective (36-month validation RMSE).
   - Two guards make future drift loud instead of silent: an unmapped
     `(model, hyperparameter)` pair aborts the build rather than being
     dropped, and the NN1-NN3 "shared search space" claim is checked against
     the stored bounds rather than assumed.
   - Naming collisions are called out in the caption: `glmnet`'s `alpha` maps
     to the paper's $\lambda^{\rho}$ but is its complement (ridge share =
     1 - alpha), and `lambda.min.ratio` maps to the *floor* of the
     $\lambda^{\psi}$ path, not $\lambda^{\psi}$ itself.
   - Table body font changed to `\scriptsize` (matching every other generated
     table) and the NN1-NN3 column given proportionally more width via
     tabularx's `\hsize` ratio trick, since its bracketed intervals are the
     longest in the table; both were needed to stop lines wrapping mid-cell.

## Compilation status

- **[FACT]** Pipeline re-run from a clean R session after every code change in
  this review; exits 0 each time.
- **[FACT]** Manuscript recompiled (2-3 `pdflatex` passes) after every change;
  currently 58 pages, 0 LaTeX errors, 0 undefined references, 0 undefined
  citations.
- **[FACT]** `bibtex` exits 1 on `references.bib` (duplicate entries / a
  malformed author list) — this is **pre-existing**, was already an open item
  in the prior handoff, and is unrelated to this review, since no citations
  were touched. A `bibtex` pass was skipped for most of this review's
  recompiles because no citation changed; a full `pdflatex/bibtex/pdflatex
  x2` sequence was run once (after the fund-picker/spanning patches) and
  produced the same 58-page, error-free PDF modulo that pre-existing `bibtex`
  exit.
- **[FACT]** Overfull-hbox count unchanged at 18 throughout (all pre-existing,
  in the appendix `mostcitedarticles` tables and prose, none newly introduced).

## What is still open (owner decisions or follow-up work)

1. **Section 5 prose rewrite** — explicitly deferred throughout this review
   per the repository owner's instruction ("do not rewrite text yet"). Current
   prose still asserts claims the corrected numbers no longer support:
   - t-statistics "above 3.0 or 4.0" (ML alphas are now 1.88-2.70 in Table 3,
     1.77-2.77 in Table 5's style-adjusted specification).
   - "Best-in-Class achieved a t-stat of 2.68" (now 3.75 CAPM-alpha t-stat in
     Table 3, but only 0.72 once style-adjusted in Table 5 — the load-bearing
     asymmetry the paper should lead with).
   - The spanning-test intercepts (see fix #2 above) are new material with no
     home in the prose yet.
2. **`references.bib` duplicate entries** — pre-existing, causes `bibtex` to
   exit 1; needs the repository owner to say which duplicate is the intended
   one before de-duplicating.
3. **Fund-ranking length/volatility disclosure vs. correction** — see item 5
   above. Disclosed in the caption; whether to additionally correct the
   ranking (geometric and/or length-adjusted) and re-run everything downstream
   of `FUNDS_Q75` is an owner call given how much depends on it.
4. **Basket-size-20 threshold noise** in Table 6 Panel C (see item 1 above) —
   cosmetic unless the exact cutoff is going to be quoted in text.
5. **Eight pre-existing dangling `\ref`s** carried over from before this
   review, never addressed: `absret`, `activecumret`, `relperf`, `downret`,
   `capture`, `box`, `cor1`, `scatter`, `results`.

## Build artifact: `manuscript.pdf` always shows as modified

- **[FACT]** `pdflatex` output is not byte-reproducible here. Three consecutive
  compiles of an unchanged `manuscript.tex` produced three different MD5
  hashes (`642bfea5...`, `fa4fda06...`, `3c29fdb1...`), with the file size
  drifting by a few kilobytes each time.
- **[INFERENCE]** This is the usual pdfTeX behaviour: the PDF embeds a
  `/CreationDate`, `/ModDate` and document `/ID` derived from the wall clock.
- **[FACT, practical consequence]** `git status` will report
  `latex_paper/manuscript.pdf` as modified after *any* recompile, whether or
  not the content changed. A non-empty diff on this file is therefore not
  evidence that the manuscript changed, and it cannot be used as one.
- **[OPEN]** Owner decision, if the noise is bothersome. Either (a) accept it
  and commit the PDF alongside real source changes, which is the current
  practice; (b) set `SOURCE_DATE_EPOCH` (pdfTeX honours it, together with
  `FORCE_SOURCE_DATE=1`) to make builds reproducible; or (c) stop tracking the
  compiled PDF and treat it as a build output. No change made either way —
  this is a workflow preference, not a defect.

## Reproduction

```powershell
& 'C:\Users\paulo.fonteles\AppData\Local\Programs\R\R-4.3.0\bin\Rscript.exe' `
  --vanilla -e ".libPaths(c('renv/library/R-4.3/x86_64-w64-mingw32', .libPaths())); source('plots.R')"
```

```powershell
cd latex_paper
pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex
pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex
```

Run `bibtex manuscript` between the two `pdflatex` passes only if a citation
changed; it will exit 1 regardless (see "Compilation status" above), but the
PDF still compiles correctly.

If table or figure *content* needs changing, edit `plots.R` and re-source —
never edit the generated blocks inside `manuscript.tex`, they will be
overwritten on the next pipeline run.

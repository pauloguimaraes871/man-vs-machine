# HANDOFF — "Man vs Machine" figures/tables pipeline

## Context

Paper: *Man vs Machine — can ML-based stock-picking outperform human skill?* (Brazilian
equities). The manuscript lives in `latex_paper/manuscript.tex` (elsarticle class). All
tables and figures in the results section are **generated programmatically** by
`plots.R` at the project root — do not hand-edit them in the manuscript, regenerate.

`plots.R` is sourced interactively in an R session that must already hold two objects
which are **not** loaded by the script:

- `signal_ports` — cap-scaled long-only single-signal portfolio backtests (used to build
  the factor-mimicking portfolios for Table 5).
- `restore_meta_xts_dates()` — a user-defined helper that repairs the date index on
  `meta_xts` objects. **This is not defined in `plots.R` and not in factoRverse.** Sourcing
  the script in a clean session fails at the first call. Folding its definition into
  `plots.R` is a known TODO (matters for the planned Zenodo/reproducibility release).

Everything else loads from `data/` (six `.qs` bundles auto-loaded by folder name, plus
`bases_fundos.xlsx`, `ibov.xlsx`, `CDI.xlsx`).

Outputs: `tables/*.tex` (Tables 3–6), `figures/*.png` + `figures/*_figure*.tex`
(Figures 1–12). The `.tex` snippets are paste-ready `table`/`figure` environments.
`latex_paper/figures/` is a **separate copy** — PNGs must be copied there after
regenerating, or the manuscript builds against stale images.

## Non-obvious methodological decisions (do not "simplify" these away)

These were each arrived at after finding a concrete bug or bias. Reverting any of them
reintroduces a known error.

1. **Benchmark timing conventions must never be mixed.** The ML backtest measures returns
   15th-to-15th (`selected_bench_return`); the Economatica fund panel and `ibov.xlsx` are
   calendar-month. Differencing a calendar-month fund return against the mid-month IBOV
   injects ~5%/month of pure timing noise — it inflated fund tracking error roughly 3x
   (19% vs the true ~6%) and biased ML-vs-fund correlations toward zero. Each series is
   now differenced against IBOV **on its own convention** (`bench_vec` for ML,
   `bench_fund_vec` for funds), and the man+machine combination is built from *active*
   returns (valid because the weights sum to one), never by subtracting one benchmark
   from a blended total.

2. **Table 5 uses the Dimson (1979) correction on MKT.** The style factors are mid-month
   while funds are calendar-month, and that non-synchronicity attenuated fund market betas
   to ~0.5 with R² ~0.40. The regression now includes lead/contemporaneous/lag market
   terms and reports their **sum**, with the t-stat of that linear combination computed
   from the full covariance block (`sum(V[dimson_terms, dimson_terms])`) — not by treating
   the three as independent. Applied uniformly to all rows so the specification is
   consistent; ML rows are unaffected (validation check: their betas/R² should not move).
   `references.bib` still needs a `dimson1979` entry; the caption currently says plain
   "Dimson (1979)".

3. **Cross-fund aggregation uses the MEDIAN, not the mean.** Ratios with near-zero
   denominators (Martin ratio especially — a fund with tiny active drawdown produces a
   huge value) dominate a cross-fund mean; `AVG_FUND_Q75` read 3.04 before the switch.
   Rows are labelled `MED_FUND` / `MED_FUND_Q75` accordingly.

4. **"Metric of the average" vs "average of the metrics" are different benchmarks and both
   are reported.** `FUNDS_MEAN`/`FUNDS_Q75` compute metrics on an aggregated series (an
   investor holding all / all top-quartile funds, banking cross-fund diversification).
   `MED_FUND`/`MED_FUND_Q75` compute metrics per fund then take the median (an investor
   who picks one fund). For the combination grid, averaging combined *returns* across
   funds is algebraically degenerate — it collapses exactly to the `FUNDS_MEAN`
   combination — so metrics are computed per fund and aggregated afterwards.

5. **Top-quartile funds are ranked within the evaluation window.** The fund panel runs
   Nov-2015..Jul-2026 but the backtest ends May-2024; ranking over the whole file let 27
   months of post-window returns decide Best-in-Class membership. `fund_avg` now uses
   `fund_ret[keep, ]`. Selection is still ex-post (disclose this — it is not identifiable
   in advance), but no longer uses data outside the evaluated period.

6. **Combination grid, not random weights.** Two assets with a budget constraint give a
   one-dimensional weight space, so the frontier is traced on `w ∈ {0, 0.05, …, 1}`.
   Random draws would add Monte Carlo noise for no benefit.

7. **Dead/unborn funds reallocate to ML.** In `blend()`, months where a fund does not
   report are held at 100% ML rather than dropped, so every combination is a continuously
   invested path. Side effect: `w = 1` endpoints now coincide exactly with the standalone
   ML portfolios. Note the consequence — at `w = 0` the "pure manager" square is really
   "manager when available, ML otherwise", so those squares scatter slightly by ML scheme
   in the single-fund panels.

8. **CDI covers 2015-11..2024-04, one month short of the window.** Only the Sharpe ratio
   uses it; the code warns and drops the affected month rather than halting. Re-exporting
   CDI through May-2024 clears the warning. Note Table 5 silently loses that month too.

## Current state

Manuscript blocks (lines ~714 onward) were replaced wholesale with the regenerated
`.tex` snippets — 4 tables (3–6) and 12 figures (1–12), in that order. Backup at
`latex_paper/manuscript.tex.bak`.

**The manuscript compiles.** `latex_paper/manuscript.pdf`, 53 pages, no fatal LaTeX
errors, zero undefined citations. Build sequence: `pdflatex` → `bibtex` → `pdflatex`
→ `pdflatex`.

Verified after the splice: `table`/`figure`/`tabularx` environments balance (11/11,
12/12, 11/11), every `\includegraphics` target exists, and the edit introduced **no new**
dangling `\ref`s.

Two `references.bib` entries were fixed because they hard-blocked compilation — both
pre-existing, unrelated to the results work, and the only edits made outside `plots.R`
and the generated blocks:

- line 770 (`masters1993`): `C\++` → `{C}{+}{+}`. The stray backslash produced an
  undefined control sequence in the `.bbl`, which is a *generated* file, so the error
  surfaced at a line number that does not correspond to anything in `manuscript.tex`.
- line 697 (`ranger`): `{C++}` → `{C}{+}{+}`, changed pre-emptively; harmless either way.

**No prose has been rewritten.**

## Open items, in priority order

1. **Narrative text contradicts the new numbers.** §5 still says alpha "t-stats exceeding
   2.0 and remarkably above 3.0 or 4.0" (now 1.88–2.70, none above 3.0) and "Best-in-Class
   achieved a t-stat of 2.68" (now 3.75 in Table 3, 0.72 style-adjusted in Table 5). This
   is the main outstanding task.
2. **The headline result to write up**: FUNDS_Q75 has the *highest* CAPM alpha t-stat in
   Table 3 (3.75, above every ML portfolio) but an insignificant style-adjusted alpha in
   Table 5 (0.72), while ML alphas are essentially unchanged between the two specifications
   (1.88–2.70 → 1.77–2.77). I.e. the Best-in-Class human's alpha is style beta; the
   machine's survives style adjustment. On the combination frontier, only the FUNDS_Q75
   panel shows a clear interior optimum (active risk ~6.4% → ~5.3% at similar return);
   the other three panels optimise at or near `w = 1`.
   Note for the write-up: ML alpha t-stats no longer clear the Harvey-Liu-Zhu 3.0 bar the
   stale draft invoked — the honest framing is significance at conventional levels plus
   survival of style adjustment, not a 3.0 threshold claim.
3. **Eight pre-existing dangling `\ref`s** from the stale draft's removed tables:
   `absret`, `activecumret`, `relperf`, `downret`, `capture`, `box`, `cor1`, `scatter`,
   `results`. They currently render as bold `??` in the PDF and should clear when §5 is
   rewritten against the new tables.
4. **`bibtex` exits 1 on pre-existing data-quality problems** in `references.bib`: five
   duplicate entries (`aghassiasnessfattouchemoskowitz2023`, `houxuezhang2017`,
   `coqueretguida2020`, `lewellen2015`, `hsuetal2022`) and a malformed author list on
   `orimoloyeetal2019` ("Too many commas in name 2"). bibtex skips the duplicates and
   continues and every citation resolves, so the PDF is fine — but whichever duplicate
   bibtex kept may not be the intended one, so these are worth reconciling before
   submission.
5. Fold `restore_meta_xts_dates()` into `plots.R`; add a `dimson1979` entry to
   `references.bib` and switch the Table 5 caption from plain "Dimson (1979)" to
   `\citet{dimson1979}`; consider replacing hard-coded figure/table numbers with LaTeX
   auto-numbering, since the current numbers desync silently if the order changes.
6. Re-export `CDI.xlsx` through May-2024 to clear the missing-month warning (see item 8
   above).

## Regenerating everything

```r
# in an R session that already holds `signal_ports` and `restore_meta_xts_dates()`
source("plots.R")
```

then

```bash
cp figures/*.png latex_paper/figures/     # figures/ and latex_paper/figures/ are separate
cd latex_paper && pdflatex manuscript && bibtex manuscript && pdflatex manuscript && pdflatex manuscript
```

If table or figure *content* needs changing, edit `plots.R` and re-source — never edit the
generated blocks inside `manuscript.tex`, they will be overwritten.

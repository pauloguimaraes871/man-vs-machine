# Handoff: refined sample + conditional (per-mandate) gap panel

Last updated 2026-08-18. Supersedes the previous handoff, which described the
`codex/add-gap-panel-scaffold` branch and the 59-period build.

## Branch and scope

- Branch: `main`. Nothing committed this session — the working tree carries all
  the changes below and needs the owner's review before any commit.
- Changed by this session: `build_gap_panel.R`, `analysis_gap.R`,
  `DESIGN_NOTE.md`, `tests/testthat/test-build-gap-panel.R`,
  `tests/testthat/test-panel-integrity.R`, this file.
- Also modified in the tree but **not by this session** (pre-existing, untouched):
  `latex_paper/manuscript.tex`, `latex_paper/references.bib`, `renv.lock`,
  `run_paper_machine.R`, and the untracked `factoRverse/v.0.7.0/`.
- Regenerated outputs: `data/derived/gap_panel.qs`, `industry_panel.qs`, plus
  `figures/` and `tables/` from `analysis_gap.R`.

## What changed

**1. Refined sample.** Every input moved to the `_refined` vintage:
`bases_fundos_diarios`, `aum_diarios`, `flow_diarios`, `n_investors_diarios`,
`CDI_diarios`, `dados_cadastrais`, `top_{1..15}_w`. 703 funds, 701 usable across
all four daily panels, 2010-01-01..2026-07-31.

The refined exports removed feeder/mirror vehicles at source and are **not a
subset** of the original files — 171 of the 703 codes are absent from them. The
two vintages must never be mixed within a build. The refined cadastral also puts
its header on row 3 rather than row 1, so `build_static()` now detects the header
row instead of assuming it.

**2. Machine.** `data/derived/machine_run/meta_grid.qs` (local paper run, grid
arm) replaces the pinned 2024-05 bundle. Structure differs — four mandate
cohorts under `$cohorts`, not `$backtest_meta_portfolios_results$
ibov_port_backtest_cohort` — so `read_ml_index()` / `read_ml_returns()` were
rewritten rather than repointed. All cohorts share one 128-period index,
2015-12-15..2026-07-15.

**3. Conditional benchmark.** `gap(i,t) = ra_m(i)(t) - ra_i(t)`, where `m(i)` is
the fund's mandate: Ações Dividendos → `idiv`, Ações Small Caps → `smll`,
everything else → `ibov`. Full rationale in `DESIGN_NOTE.md` §7.1.

`isee` was built (ANBIMA class ∪ an ESG/sustainability/governance name rule,
19 funds) and then **dropped on the owner's decision** — too thin to carry its
own benchmark. ESG funds now fall back to `ibov`. The isee cohort still exists in
`meta_grid.qs`, and the name rule (including the word-boundary lookarounds that
keep "Itaú Asgard" out) is recoverable from this session's git history if the
sample ever grows.

**4. Factor basis.** Now the local run's `screen.qs` signal ports, because the
pinned 2024-05 basis stops before the panel does. **One deviation from
`plots.R`:** the new gold features retired `assets_gr1y_tr`, so the CMA leg uses
`assets_gr1y` (owner's decision). The other five legs are unchanged.

## Current state

Panel: **89,728 fund-periods, 701 funds, 27,975 scored rows over 92 periods**
(2018-12-15..2026-07-15 — 128 machine periods less the 36-period beta burn-in).
Up from 59 periods in the previous build.

Estimation sample (`analysis_gap.R`): 14,172 rows, 239 funds, 84 periods.

Unconditional gap, two-way clustered (pp/month, positive = machine ahead):

| Arm | n | Funds | Mean gap | se | t | p |
|---|---|---|---|---|---|---|
| pooled | 14,172 | 239 | 0.282 | 0.155 | 1.82 | 0.069 |
| ibov | 12,345 | 212 | 0.261 | 0.161 | 1.62 | 0.104 |
| smll | 832 | 11 | 0.613 | 0.208 | 2.95 | 0.003 |
| idiv | 995 | 16 | 0.266 | 0.133 | 2.00 | 0.046 |

**This is a change in the headline result and the owner has not yet ruled on how
to read it.** The previous build reported the gap as not distinguishable from
zero (0.34pp, se 0.47, t = 0.71, 59 periods). The standard error has fallen by
roughly two thirds, largely because the window is half again as long, and the two
mandated arms are now individually significant. Do not write this up as a settled
finding without the owner.

H7 ladder still reads flat across all eight construction schemes (0.17 to 0.40
pp/month), consistent with the systematisation story rather than optimiser
sophistication. Note its standard errors are the crude `sd/sqrt(n_periods)`, not
clustered — that is pre-existing and worth upgrading before the ladder is cited.

Tests: **123 pass**, 58 of them integration assertions on the built artefact
(`Rscript -e 'options(gap_panel.run=FALSE); testthat::test_dir("tests/testthat")'`).

## OPEN items

1. **Two dead cadastral columns in the refined export.** `perf_fee` is FALSE for
   all 21,662 candidate rows (the export carries no "Sim" for *Cobra taxa
   performance*; the original had 18 of 1,943), and `Alavancado` is "N" for all
   703 funds against 578 "S" of 1,943 before. A 30% → 0% shift on `Alavancado`
   does not look like a subsetting artefact — **check whether the refined pull
   filtered on it or the column failed to populate.** `analysis_gap.R` now drops
   zero-variance regressors with a warning rather than dying inside `chol()`, so
   this degrades loudly rather than silently.

2. **Curated quant-validation set is stale.** 10 of the 20
   `QUANT_VALIDATION_CODES` are absent from the refined snapshot; the assertion
   warns and lists them rather than aborting (owner's decision). Several are
   clearly feeder/cotas vehicles the `Multigestor` flag missed (Warren Fatores
   **Feeder**, V8 Veyron **Fc**, Nu Smart **Classe Única de Cotas**, Mint
   **Bdr**), but Simpaul Momentum/Value/Multifatores, Murano Multifatores, Itaú
   Smart Low Vol and BB Seleção Fatorial simply are not in the revised universe.
   Re-curate before the label is used for anything beyond a diagnostic.

3. **Duplicate screen is reduced but not inert.** 653 clusters across 701 funds,
   48 funds flagged non-representative. Expected to be near-useless on a
   feeder-free sample; the residual 48 are worth a look.

4. **Published attribution tables need re-running** before they are reported
   alongside these gaps — the CMA leg changed.

5. **`plots.R` is untouched** and still reads the pinned 2024-05 artefacts. It
   and `build_gap_panel.R` are now on different sample vintages and different
   factor bases. Nothing shares state between them, but any figure that puts
   their numbers side by side is currently comparing two different studies.

6. **Carried over from the previous handoff, still unresolved:** fund-periods
   with `w_top1 == 100`. Worth re-checking on the refined sample — feeders were
   the leading hypothesis and they are largely gone now, so the surviving cases
   should be more informative than before.

7. **Comdinheiro holdings probe remains shelved**, unchanged. See
   `COMDINHEIRO_NOTES.md`; nothing in this session touched it.

## Standing constraints (from `~/.claude/CLAUDE.md`, unchanged)

- Never edit/create/delete files or run shell commands without presenting a plan
  and getting explicit approval first. Routine diagnostic reads and greps are
  fine; anything that changes `build_gap_panel.R`'s output is not.
- Never commit/push without explicit authorization each time.
- Keep `latex_paper/manuscript.pdf` and `provenance/` out of any commit unless
  the owner deliberately expands scope.

## Operational note

`data/dados_cadastrais_refined.xlsx` was open in Excel during this session and
file-locked; `readxl` fails with a bare "zip file cannot be opened". Close the
workbook before any build.

R lives at `~/AppData/Local/Programs/R/R-4.3.0/bin/Rscript.exe` — not on `PATH`.

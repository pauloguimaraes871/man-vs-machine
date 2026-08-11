# Handoff: gap panel + regression + Comdinheiro holdings probe

## Branch and scope

- Branch: `codex/add-gap-panel-scaffold`
- Latest commits:
  - `fdbe70a` docs: record Comdinheiro FundScreener holdings-API probe
  - `c0ba3b0` feat: wire DV, screens, and regression analysis into gap panel
  - `a3c18d4` fix: support signed top-x holdings (prior session)
- Working tree is otherwise clean except `latex_paper/manuscript.pdf`
  (modified) and `provenance/` (untracked) — **deliberately excluded** from
  both commits above; do not add them without the owner's explicit go-ahead.

## State of the gap-panel study (commit `c0ba3b0`)

Panel builds end-to-end: `source("build_gap_panel.R")` writes
`data/derived/gap_panel.qs` and `industry_panel.qs`. Both the unit suite
(`tests/testthat/test-unit*.R`, if present) and the 54-assertion integration
suite in `tests/testthat/test-panel-integrity.R` pass.

`analysis_gap.R` runs the regression and diagnostics. Headline result,
already reported to the owner: the fund-vs-machine gap is **not
statistically distinguishable from zero** (mean 0.34pp/month, se 0.47,
t = 0.71, two-way clustered on 59 periods), and is flat across all 8
meta-portfolio construction schemes (H7 ladder) — read as support for a
systematisation story rather than optimiser sophistication. Nothing here is
pending; this is closed unless the owner reopens it.

Sample waterfall: 197,676 → 42,015 (DV) → 26,274 (drop feeders/FoF) → 19,003
(dedupe) → 16,981 (reporting/post-inception) → **10,252 rows, 237 funds, 59
periods** in the covariate-complete regression sample.

## State of the Comdinheiro holdings probe (commit `fdbe70a`)

Explored pulling fund-level holdings from `FundScreener001` to build a richer
diversification/turnover measure. Fully written up in
[`COMDINHEIRO_NOTES.md`](COMDINHEIRO_NOTES.md); working code in
[`comdinheiro_fundscreener.R`](comdinheiro_fundscreener.R) (inert on
`source()`, nothing fires without an explicit call).

**Shelved, not abandoned.** The API hard-caps every response at 1,000 rows
(confirmed: `linppag`/`truncar`/`pageviews`/`limit_01` are all inert), and the
cut is non-random — it silently drops the largest funds. A working escape
exists (`filtro` accepts chained `patrimonio>=lo AND patrimonio<hi`, enabling
a self-verifying adaptive bisection over AUM bands — proven on one class: 9
requests recovered 234 funds vs. 54 from a naive call), but a full pull
across all ANBIMA equity classes and ~66 months lands around 3,000–4,600
requests against a ~1,000/month budget. Not worth resuming until quota
allows. If resumed, resolve the `data_carteira` as-of/disclosure-lag
semantics first (cheap, one fund, a few months) before trusting any join —
see "Finding 3" in the notes file.

## OPEN — the immediate next task: `hhi_top15`/`w_top1 = 100%` rows

The owner flagged that some fund-periods have `w_top1 = 100`. This can only
mean one of: (a) a **feeder** whose single "holding" is the master fund
(should already be excluded via `is_feeder`, so any surviving cases are a
bug in that flag or its timing), (b) a genuine **mono-stock fund** (plausible
for FMP-FGTS/`SUGOV`-style mandates), or (c) a data artefact. Left as-is it
silently pollutes `hhi_top15`, `eff_bets_max`, `n_pos_min`, and `resid_weight`
for those rows.

Three options the owner is weighing, not yet decided:

1. **Redo the holdings download** including master funds (currently
   excluded, since the original design intent was to study only the vehicles
   investors actually buy into) — largest change, would touch the whole
   `topX_w` ingestion path.
2. **Drop fund-periods with `w_top1 == 100`** — owner asked to **confirm how
   many rows this removes** before committing to it; not yet counted.
3. **Set `hhi_top15`/related concentration fields to `NA`** for those rows,
   same spirit as the existing short-book and >102%-exposure gates in
   `summarise_concentration()` — smallest, most consistent-with-existing-
   screens option, but silently discards a signal that might usefully
   distinguish index-hugging or mono-stock mandates.

**Next action:** before recommending an option, check the actual composition
of `w_top1 == 100` rows against `is_feeder`, `is_fof`, and `anbima_class`
(especially FMP-FGTS / `SUGOV`) in `gap_panel.qs`, and get the row/fund count
under option (2). None of this has been investigated yet this session — pick
up here.

## Standing constraints (from `~/.claude/CLAUDE.md`, unchanged)

- Never edit/create/delete files or run shell commands without presenting a
  plan and getting explicit approval first (routine diagnostic reads/greps
  are fine; anything that changes `build_gap_panel.R`'s output is not).
- Never commit/push without explicit authorization each time.
- HANDOFF.md exclusion still applies: keep `latex_paper/manuscript.pdf` and
  `provenance/` out of any commit unless the owner deliberately expands
  scope.

# Comdinheiro FundScreener001: holdings-level API probe

**Date:** 2026-08-09
**Status:** Probed, working, **shelved** — request budget makes the full pull
infeasible this month. See "Decision" below.

## Objective

Evaluate whether Comdinheiro's `FundScreener001` report can supply fund-level
equity holdings (asset, weight/value, purchases, sales) for the funds already
in `gap_panel.qs`, keyed to the same `ml_index` dates used by
`build_gap_panel.R`. The candidate use is a richer diversification/turnover
measure than the existing `topX_w` weight files, and a possible discretionary-
vs-systematic signal from holdings churn.

## API pattern

Same shape as the pipeline's
`Multifatorial 3.0/pipeline/R/load/download_comdinheiro.R`:

- `POST https://api.comdinheiro.com.br/v1/ep1/import-data`
- form body: `username`, `password`, `URL`, `format=json3`
- `URL` is the **relative** path only (strip the
  `https://www.comdinheiro.com.br/` prefix from a URL copied out of the web
  UI).
- Response JSON: `$tables$tab0`, header in `lin0`, rows in `lin1..linN`,
  columns `col0..colM`. The pipeline's `parse_json3_table()` / `br_num()`
  (BR decimal comma, parenthesised negatives) parse it unmodified.
- Credentials: `COMD_USER` / `COMD_PASS` environment variables. This project
  has no `.Renviron` of its own; the working credentials live in the
  pipeline project's `.Renviron`
  (`Renda Variável/Multifatorial 3.0/pipeline/.Renviron`). Load them with
  `readRenviron()` rather than copying the secret into this repo.

## Finding 1 — hard 1,000-row cap, and it is not a random sample

A single `FundScreener001` request returns **at most 1,000 rows**, no matter
what. Confirmed inert: `linppag` (100 / 1,000 / 50,000 all identical),
`truncar`, `pageviews`, and `limit_01` tried both as a row limit (`3000`,
`0,3000`) and as an offset (`1000,1000` — page 2 returned the *same* funds as
page 1, not the next page). There is no pagination mechanism and no
total-row-count field in `meta`.

Worse, the cut is **not random**: without a `patrimonio` (AUM) filter, the
response silently keeps the *smallest* funds and drops the largest. The
10-class query from the original URL returned 35 funds capped at R$1.4bn AUM;
adding `patrimonio>1e9` surfaced funds up to **R$18bn** that the plain query
never returned. The cap can also truncate a fund's book mid-portfolio: the
last fund in an unfiltered response (BB Top Ações Ibovespa Indexado) came
back with only 44 of its holdings, smallest at R$3.1M, when a complete
passive-index book runs to much smaller lines.

## Finding 2 — `filtro` chains with `AND`, enabling a self-verifying bisection

`filtro=desc_ativo=acoes+AND+patrimonio%3E%3D<lo>+AND+patrimonio%3C<hi>`
(`%3E%3D` = `>=`, `%3C` = `<`) works and is respected exactly — a
`[1e9, 5e9)` band returned funds with AUM in `[1.01e9, 4.71e9]`, nothing
outside it. `;`, `&&` (`%26%26`), and `,` as condition separators were all
rejected.

This makes an adaptive bisection possible and **self-verifying**: a response
under 1,000 rows is provably complete for its band (disjoint half-open PL
bands, so no double-counting is possible); a response of exactly 1,000 means
split the band and recurse. Verified end-to-end on `cl_anb=ACIE` for
2021-01: **9 requests** recovered **3,139 rows / 234 funds**, versus 1,000
rows / 54 funds from the naive single call — the naive call was seeing well
under a quarter of that one class alone.

Implementation: [`comdinheiro_fundscreener.R`](comdinheiro_fundscreener.R),
functions `comd_fundscreener_band()` (one band) and
`comd_fundscreener_bisect()` (recursive walk). Nothing executes on `source()`
— the example calls at the bottom are wrapped in `if (FALSE)`.

## Finding 3 — `data_carteira` snaps to month-start, and holdings are stable within a month

`data_carteira` is a genuine reported field, not an echo of the request:
`data_cart=01/01/2021` and `data_cart=15/01/2021` both come back stamped
`01/01/2021` with **byte-identical books** for all 35 funds tested;
`data_cart=15/02/2021` comes back stamped `01/02/2021` with **every** book
different from the January snapshot. So Comdinheiro resolves any date inside
a calendar month to that month's snapshot and labels it by month start.

Two open questions this probe cannot settle, both load-bearing for
`build_gap_panel.R`'s no-look-ahead discipline (see the AUM/investor timing
note at the top of `S5` in that file):

1. **What the monthly snapshot is an as-of measurement of.** Is
   `01/02/2021` the end-of-January book, or the true early-February book?
   CVM disclosure timing needs to be checked against this before trusting a
   join on the label alone.
2. **Disclosure lag.** If Comdinheiro backfills a fund's book once it is
   filed (funds can delay CVM carteira disclosure), joining month `M`
   holdings onto month `M`'s return would be look-ahead.

`patrimonio` in the response tracks `data_rr` (the risk/return reference
date), not `data_cart` — it is not a substitute for the panel's existing
daily AUM series and the two dates must be advanced together in any future
loop.

## Finding 4 — the join key exists but needs a new column

`codigo_anbima` **does not join**: the API returns CVM-175 class codes
(`C0000xxxxxx`-style, 438 of 438 sampled), while the panel's `code` is a mix
of 1,780 numeric ANBIMA fund codes and 109 C-prefixed class codes. Direct
intersection on that field is zero, and fund-name matching is unreliable
(uncontrolled abbreviation — 5.7% match rate even after normalising).

**Column 7 of `dados_cadastrais.xlsx` is CNPJ** (header sits in row 3:
`# | Nome | Classe | Pais Sede | Tipo de Ativo | Ativo/Cancelado | CNPJ |
Código Anbima | ...`), and it is not currently read into
`CAD_COLS` in `build_gap_panel.R` (which starts at column 2). Coverage: 1,935
of the 1,938 codes in `gap_panel.qs` have a valid 14-digit CNPJ (99.8%; 30
CNPJs are duplicated across cadastral rows, presumably multi-class funds).

Of the 438 distinct CNPJs collected across today's sampling, only 97 (22%)
appear in the cadastral file at all — expected in part, since `situacao`
pulls in discontinued funds that predate the panel's snapshot, but the size
of that gap is itself worth checking against the panel's own survivorship
assumptions before relying on this join.

**No `build_gap_panel.R` edit was made for this** — adding `cnpj = 7L` to
`CAD_COLS` is a one-line change to make whenever the holdings pull is
resurrected, not before, since there is no consumer of it yet.

## Decision: shelved

Per-class-per-month bisection cost is uneven (some ANBIMA classes return
under 1,000 rows in a single call; `ACIE` needed 9). At roughly 5–9 requests
per class-month across the URL's 10 `cl_anb` classes, a full pull across the
~66 months in `ml_index` lands in the **3,000–4,600 request** range. Against
a ~1,000-request monthly budget, that does not fit. This exploration itself
consumed on the order of 40–50 requests.

**Not pursuing the holdings pull further until the request budget allows.**
If revisited: resolve the `data_carteira` as-of/lag question first (cheap —
one fund, a few adjacent months), since it decides whether the eventual
join can key on `ml_index` 15ths directly or needs an as-of adjustment.

## Files

- [`comdinheiro_fundscreener.R`](comdinheiro_fundscreener.R) — the tested
  fetch/bisection functions, self-contained, no side effects on `source()`.
- No raw API responses are committed (proprietary licensed data); they were
  cached only in the session scratchpad during probing and are reproducible
  by rerunning the functions in this file.

# Design Note — Repositioning "Man vs Machine"

**Status:** working specification, synchronized with `build_gap_panel.R`; `plots.R` is unchanged.
**Date:** 2026-08-04
**Author:** drafted by Claude from the 2026-08-02/03 brainstorm; every claim marked
`[VERIFY]` still needs the owner's confirmation.

---

## 1. What changes

**From:** a horse race — do ML portfolios beat Brazilian discretionary fund managers?

**To:** a decomposition — *what explains the return difference between human managers
and a systematic benchmark?*

The horse race is not novel. An ML pipeline in the Gu–Kelly–Xiu family applied to a
new market is an application, and it is not even the first Brazilian one. The
decomposition is novel, and it uses the same data.

The machine stops being the paper's subject and becomes its **measuring instrument**:
a benchmark that embodies the published frontier of cross-sectional return prediction
(linear and non-linear), estimated out-of-sample, with multiple-testing control. The
question becomes what humans do that the instrument cannot capture — and what
structural frictions prevent them from capturing it.

### Consequences for the current manuscript

- The full ML methodology moves to an appendix. It is an input, not the story.
- Section 5 prose (still unwritten — see `HANDOFF.md`) should be drafted against the
  new frame, not the old one. This is a reason *not* to have written it yet.
- The literature review needs a new section on mutual-fund performance (§3 below).
- The title must change. See §2.

---

## 2. Differentiation from prior work

| Paper | What it does | How this paper differs |
|---|---|---|
| Abis, *Man vs. Machine: Quantitative and Discretionary Equity Management* | Classifies **existing US funds** as quant vs discretionary from filings; compares the two groups | Her "machine" is technology **bundled with organization** — real funds with fees, flows, career concerns. Ours is technology **without** organization: a constructed counterfactual. She cannot separate the two; we can. |
| Miguel & Chen (2025), *How active is your (nominally) actively managed quantitative fund?* | Shows 50% of US quant fund assets are closet-indexed; Active Share loads **negatively** for quants | Confirms that the *label* "quant" does not identify the *behaviour*. Supports our decision not to classify funds as systematic (see §9, rejected designs). |
| Kaniel, Lin, Pelger & van Nieuwerburgh (JFE 2023); DeMiguel et al. (JFE 2023) | Use ML to **select managers** | We use ML as the **competitor and yardstick**, not the selector. Different question. Already cited in `references.bib`. |

**Title collision is a real problem.** The current title is Abis's title. A referee
seeing it will assume derivative work before reading the abstract. Retitle around the
decomposition, e.g. *"What Explains the Gap? Decomposing Human–Machine Performance
Differences in Brazilian Equity Funds."*

**Framing caution.** Do not claim the benchmark is "state-of-the-art AI." That claim
ages badly and invites the objection that a GKX-family pipeline is not the 2026
frontier. Claim what is defensible: *a disciplined systematic benchmark representing
the published frontier of cross-sectional return prediction, estimated out-of-sample
on Brazilian data with multiple-testing control.*

---

## 3. Literature section to add

Currently `references.bib` contains essentially **no mutual-fund performance
literature** — it is Hou–Xue–Zhang, Gu–Kelly–Xiu, Kelly–Xiu, Kelly–Malamud–Zhou,
Jensen–Kelly–Pedersen, Blitz et al., Fama–French, Kaniel et al. A fund-literature
referee will reject the paper for not engaging the field.

Minimum additions, grouped by the role they play:

**Skill vs luck / how fund alpha is evaluated**
Carhart (1997); Fama & French (2010); Kosowski et al. (2006); Barras, Scaillet &
Wermers (2010).

**Why zero net alpha is not zero skill — the equilibrium argument**
Berk & Green (2004); Berk & van Binsbergen (2015). *This is the strongest theoretical
attack on the old framing and the foundation of the gross/net decomposition in §6.*

**Scale**
Pástor, Stambaugh & Taylor (2015); Pástor & Stambaugh (2012); Gârleanu & Pedersen
(JF 2018) — fixed information-acquisition costs generating scale **economies**, the
counterweight to price-impact **diseconomies**.

**Process and activeness measurement**
Cremers & Petajisto (2009); Amihud & Goyenko (2013).

**State-dependent skill**
Kacperczyk, Van Nieuwerburgh & Veldkamp (2014); Abis.

**Brazilian fund literature**
The bootstrap/Carhart studies of Brazilian equity funds; the size-effect studies; the
industry-competition and persistence work. `[VERIFY]` — these were found by web search
and the specific findings below need checking against the actual papers.

> **Citation caveat:** the reference list above was recalled from memory and partially
> confirmed by web search on 2026-08-02/03. Every entry must be verified before it
> enters `references.bib`. Note also the pre-existing `bibtex` exit-1 duplicate-entry
> problem recorded in `HANDOFF.md`.

---

## 4. Theoretical frame

### 4.1 The Fundamental Law as an accounting identity

Generalized law (Clarke, de Silva & Thorley 2002; Grinold 1989):

```
IR  =  IC  ×  TC  ×  sqrt(BR)
```

- `IC` — forecasting skill (correlation of forecast with realised return)
- `TC` — transfer coefficient (correlation of *actual* with *ideal* active weights)
- `BR` — breadth (number of independent bets)

**Two algebra points that must not be got wrong:**

1. Breadth enters as `sqrt(BR)`, not `BR`.
2. The law is **multiplicative**, so differences do **not** factor into deltas:
   `IR_h − IR_m ≠ ΔIC × ΔTC × sqrt(ΔBR)`. An additive decomposition requires logs,
   `ln IR_h − ln IR_m = (ln IC_h − ln IC_m) + (ln TC_h − ln TC_m) + ½(ln BR_h − ln BR_m)`,
   which needs all components positive — and many funds have negative IR. Use a
   Shapley or Oaxaca–Blinder sequential decomposition to handle the sign problem.

**Identification limit — this is the binding constraint.** From returns alone, `IR` is
observed but its factorisation is not: one equation, three unknowns. IC, TC and BR are
all properties of *what was held*. Measuring them properly requires CVM holdings
(CDA files), which the owner has ruled out on time grounds.

**Therefore the law is used as an identity with proxied terms and a named residual,
not as a structural estimation target.** See §5.

### 4.2 The two opposing scale forces

- **Diseconomies** — price impact, rising in *fund* size (Berk–Green; PST).
- **Economies** — fixed cost of information acquisition, falling in *firm* size
  (Gârleanu–Pedersen). Research teams, data and infrastructure are paid at the
  management-company level, not the fund level.

`[VERIFY]` Brazilian studies report *increasing* returns to scale — larger funds
outperform — which is the opposite of US and of 35-country evidence. If it replicates
in our sample, the information-cost force dominates at observed Brazilian fund sizes,
which is consistent with a weak public-information environment. **This must be
verified in our own data; the obvious confound is survivorship.** Refuting it would
itself be a contribution to the Brazilian literature.

**Design consequence:** include `PL do Fundo` and `PL da Gestora` *separately*. They
proxy the two opposing forces, and Brazilian disclosure lets us separate what US data
cannot.

---

## 5. Hypotheses

Pre-specified. Sign predictions are what the paper is committing to *before* running
the regressions; the multiple-testing discipline in `references.bib`
(Jensen–Kelly–Pedersen) is the standard we will be held to.

| # | Hypothesis | Predicted sign | Source |
|---|---|---|---|
| H1 | The human–machine gap narrows in gross terms relative to net | gap_gross < gap_net | Berk & van Binsbergen |
| H2 | Gap widens with fund size (price impact) | `PL do Fundo` ↑ ⇒ gap ↑ | Berk–Green, PST |
| H3 | Gap narrows with management-company size (information economies) | `PL da Gestora` ↑ ⇒ gap ↓ | Gârleanu–Pedersen |
| H4 | Gap widens with shorter redemption terms (forced flow-driven trading) | redemption days ↓ ⇒ gap ↑ | Structural; Selic-flow channel |
| H5 | Humans do relatively better in recessions (machine cannot market-time) | recession ⇒ gap ↓ | **Abis** |
| H6 | Humans do relatively better when cross-sectional dispersion is high | dispersion ↑ ⇒ gap ↓ | Stock-picking opportunity |
| H7 | The deficit does not widen with model complexity | gap(OLS) ≈ gap(NN3) | If true, the story is *systematisation*, not AI |

> **H5 corrects an error in the original brainstorm.** Abis finds *discretionary* funds
> outperform quantitative funds **in recessions**, and that quantitative performance is
> **pro-cyclical**. The mechanism is that discretionary managers switch between picking
> and timing while quants only pick. Our machine is a pure cross-sectional picker, so it
> is Abis's quant type. Do not build the design on the opposite sign.

> **H7 is the exhibit that answers the original "is it the machine or the factors?"
> question** — using variation already on disk (OLS / GLMNET / RF / XGB / NN1–NN3 across
> RP, CS, MVO_BOX) and requiring no new data.

---

## 6. The gross/net decomposition — core exhibit

Preferred over putting capacity on the right-hand side of a regression, because it is a
measured object rather than an estimated coefficient.

```
gap_gross  =  ML gross      −  fund gross      <- pure signal comparison
gap_net    =  ML net @ AUM  −  fund net        <- implementable comparison
gap_gross − gap_net  =  the cost & capacity wedge
```

- ML raw returns are obtainable (owner confirmed).
- Fund gross returns require adding back expense ratios.
- Sweep the AUM dial in the transaction-cost model to trace the **capacity curve**, and
  report the **crossover AUM** at which the machine ceases to beat the median manager.

**Two caveats that must be stated in the paper:**

1. The crossover's location depends entirely on the price-impact calibration, which is
   an *assumption*, not an estimate. Report it as a function of the impact parameter
   with sensitivity bands — never as a single number.
2. The comparison is asymmetric: fund costs are realised and embedded in reported net
   returns; ML costs are simulated. Say so explicitly.

---

## 7. Specification

The current code defines two complementary panel specifications. The dependent
variable is the **one-period realised risk-adjusted return gap**, not a rolling alpha
or an information ratio:

```
ra(i,t)  = r(i,t) - rf(t) - beta_hat(i,t-1)' f(t)
gap(i,t) = ra_ML(t) - ra(i,t)
```

`beta_hat(i,t-1)` is fitted on a strictly trailing 36-month window. A fund-period is
eligible for the gap only with a current return and all 36 preceding returns needed
for that fit. This is a row-level, economically available-data rule; it is not a
full-sample survival screen.

### Primary specification — pooled panel, no time fixed effects

```
gap(i,t) = b0 + b1·PL_fundo(i,t) + b2·PL_gestora(i,t)
         + b3·redemption_days(i) + b4·perf_fee(i)
         + b5·expense_ratio(i) + b6·age(i,t) + controls + e(i,t)
```

- Estimate two-way clustered standard errors by fund and period.
- Market-state main effects are identified from approximately 102 periods, not from
  the number of fund-month rows. Interpret this limited effective sample honestly.
- `rho_machine`, idiosyncratic risk, tracking error, and concentration are revealed
  strategy variables/mediators. Describe or decompose through them; do not include
  them as controls in the main gap regression.

### Robustness specification — time fixed effects

Add period fixed effects to the primary specification. This absorbs the common ML
return component and all other period shocks. State **main effects** are then not
identified, but interactions between states and fund characteristics remain
identified and are robust to arbitrary common shocks.

### Specification warnings

- **Tracking error is a bad control.** It is an outcome of the skill being measured — a
  manager with better forecasts takes bigger bets, raising TE. Controlling for it strips
  out part of the signal. It is descriptive/mediating evidence, not a main RHS control.
- **Risk-adjust before differencing.** A raw gap loads mechanically on the beta
  difference between fund and ML portfolio. The panel instead uses strictly trailing
  six-factor betas and period-*t* realised residual components.
- **Timing of levels and holdings.** A return labelled by a closing 15th covers the
  interval from the preceding 15th. Fund AUM and investor counts are therefore sampled
  at that opening date; current-window flows are outcomes and only lagged flow measures
  enter as predictors. A month-end holdings snapshot is attached to the first complete
  15th-to-15th return window beginning after the snapshot is available.
- **Partial holdings and shorts.** Top-X fields can contain signed positions. Their
  squared-weight concentration statistic remains descriptive, but NAV-based residual
  weight, minimum-position, and effective-bets bounds are undefined for a short book
  or materially levered positive exposure and are recorded as missing. Source snapshots
  with impossible absolute weights above 1,000% or broken rank ordering are excluded
  from concentration measures with a warning, never imputed as zero.
- **Persistence trap.** Market P/E and dividend yield are highly persistent; regressing
  on them over ~102 months invites Stambaugh bias and the Ferson–Sarkissian–Simin
  critique. **Drop them as primary state variables.** Keep P/E, DY, market volatility
  and universe breadth in a robustness appendix only.
- **Fund turnover is not recoverable** from returns, and Brazilian funds do not report
  it in usable standard form. Do not plan around it.

---

## 8. Variables

### Already on disk

| Variable | Source |
|---|---|
| Fund monthly returns, 15th-to-15th | `bases_fundos_diarios.xlsx`, compounded |
| Eligible universe (1,127 funds `[VERIFY]`, ≥36 months) | `plots.R:166` |
| ML portfolio returns, net of costs + 1% p.a. fee | `plots.R:289` |
| ML portfolio returns, gross | obtainable per owner |
| Six-factor loadings, R², per fund | `regress_flat`, `plots.R:1035` |
| Model complexity ladder (OLS…NN3 × RP/CS/MVO_BOX) | `sb_list` |
| Portfolio weights by liquidity class | `plots.R:838` |

### To acquire (owner: "trivial")

`PL do Fundo`; `PL Total da Gestora`; administrator / manager identity; Classificação
ANBIMA; expense ratio; performance fee (Y/N); redemption days; number of investors;
investor type (Geral / Qualificado / Profissional); fund age; feeder or mirror flag;
fund-of-funds flag; ANBIMA aggregate industry AUM; Selic series.

### To construct

| Variable | Definition | Role |
|---|---|---|
| `rho_machine(i,t)` | Correlation of fund *i*'s **residual** active returns with the ML portfolio's, on a trailing window | Returns-based TC proxy |
| `idio_vol(i,t)` | Residual volatility from the six-factor model (or `1 − R²`) | Returns-based breadth proxy |
| `dispersion(t)` | Cross-sectional return dispersion, or average pairwise correlation | Stock-picking opportunity |
| `recession(t)` | CODACE official Brazilian business-cycle dates | H5 |

**Three notes on the constructed variables:**

1. `rho_machine` must be computed on **residual** active returns (after stripping the
   six factors), otherwise it conflates weight alignment with shared factor exposure —
   two funds with no common holdings but identical value tilts would look aligned.
2. `rho_machine` must be estimated on a **trailing** window and performance measured
   *out* of it. The ML portfolio performed well in-sample, so contemporaneous
   correlation with it is mechanically related to performance. This is the same
   maximisation-bias problem already handled in the oracle/FUNDS_Q75 analysis.
3. `idio_vol` carries a live tension worth exploiting rather than hiding:
   **Amihud–Goyenko** read low R² as selectivity predicting *higher* alpha;
   **Abis** reads high R² as diversified systematic breadth. Same statistic, opposite
   stories. Our machine has known, high breadth and can serve as the reference point
   that separates them.

---

## 9. Rejected designs, and why

| Design | Why rejected |
|---|---|
| Hand-picked systematic/quant fund subgroup | Too small and judgmental for a main estimand. A curated owner-supplied **20-fund validation label set**, resolved to ANBIMA codes and checked against the cadastral source, is retained for diagnostics only. |
| Classifying funds as systematic by **closeness to the ML portfolios** | Circular. Sorting on correlation with a high-performing portfolio and then measuring performance assumes the conclusion, and re-imports maximisation bias. Closeness to ML is a valid **outcome**, never a classifier. |
| Classifying by non-market factor R² | Contaminated: the style factors are built from the **same feature library the ML trades** (`plots.R:1012`), so high non-market R² partly *is* ML-closeness. |
| Measured TC from CVM holdings | Correct but ruled out on time. ~1,127 funds × ~102 months of CDA files is a multi-month data-engineering project. **This is the single highest-value future extension** — the ML portfolio supplies the "ideal portfolio" that TC is normally undefined against, making Active Share *against the machine* computable. |
| Selic as an instrument for industry scale | Fails the exclusion restriction — Selic hits equity returns through many channels, not only flows. Present as **regime variation**, not IV. |

---

## 10. Threats, and how each is handled

| Threat | Handling |
|---|---|
| Residual interpreted as "skill" | **Report as *unexplained*, not skill.** It absorbs measurement error and every misspecification. Show its stability across specifications — a residual stable at ~40% under varied controls is a defensible statement; one swinging 20–70% means the controls are doing the work. |
| Unfair fight (paper portfolio vs real funds) | Partly pre-empted: transaction costs and a 1% p.a. fee are already deducted, microcaps excluded. Extend via the AUM sweep in §6. |
| Multiple testing across ~20 candidate variables | Pre-specify the H1–H7 set; everything else goes to a robustness appendix. |
| Survivorship in fund characteristics | Standard; disclose. Particularly acute for the scale anomaly in §4.2. |
| ~102 months is thin for state-dependence | Treat state main effects as period-level evidence; use two-way clustered panel inference in the primary specification, limit the state set, and report power honestly. |
| Fund-of-funds benchmark unfairness | FoF returns carry two fee layers. Frame explicitly as "the allocator's realised experience," not as a skill benchmark, or gross up. |

---

## 11. Exhibit list (proposed)

1. Gross vs net gap, and the cost/capacity wedge — **core exhibit** (§6)
2. Capacity curve and crossover AUM, with impact-parameter sensitivity bands
3. Model-complexity ladder: gap vs OLS … NN3 (**H7**)
4. Primary pooled regression and time-fixed-effect robustness table
5. State-main-effect and state-by-trait interaction table (recession, dispersion)
6. Scale decomposition: `PL do Fundo` vs `PL da Gestora` (**H2/H3**)
7. Explained vs unexplained share of the gap, across specifications
8. *(Appendix)* Full ML methodology, currently in the body
9. *(Appendix)* Robustness: P/E, DY, volatility, universe breadth

---

## 12. Owner decisions required

1. **Retitle** — the current title is Abis's. (§2)
2. **Confirm the repositioning** before the panel scaffold is completed and any regression is estimated.
3. **Verify the Brazilian increasing-returns-to-scale finding** in our sample. The
   mechanism in §4.2 depends on it.
4. **Confirm gross ML returns and fund expense ratios are both obtainable** — §6 is the
   core exhibit and fails without them.
5. **Holdings/TC extension: in or out?** Currently out on time grounds; it is the
   version with a genuine methodological contribution.
6. Pre-existing open items from `HANDOFF.md` remain unresolved: `references.bib`
   duplicates (bibtex exit 1), eight dangling `\ref`s, `manuscript.pdf` non-reproducible
   builds.

---

## 13. Implementation status

- `build_gap_panel.R` now reads the daily and cadastral panels; constructs opening-AUM,
  flow, manager, industry, and partial-holdings covariates; and provides rolling
  risk-adjustment and residual-gap helpers. It does **not yet** assemble the dependent
  variable or mediators into the final panel, nor estimate any regression. `plots.R`
  remains untouched.
- No data acquired.
- No citation verified against the actual papers.
- §5 prose in `manuscript.tex` remains unwritten — deliberately, and it should now be
  drafted against this frame rather than the old one.

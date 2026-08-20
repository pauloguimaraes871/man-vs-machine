# Hyperparameter grid comparison

Sources, read directly rather than recalled from memory:

- **GKX** — Gu, Kelly & Xiu (2020/2019 wp), *Empirical Asset Pricing via Machine
  Learning*, Table A.5.
- **H&K** — Hanauer & Kalsbach (2023), *Machine Learning and the Cross-Section of
  Emerging Market Stock Returns*, Appendix C.4–C.5.
- **Rubesam** — Rubesam (2021), *Machine Learning Portfolios with Equal Risk
  Contributions: Evidence from the Brazilian Market*, Appendix Table A.1 (the
  explicit numeric grid; §3.2.5–3.2.7 gives the prose).
- **BayesCM** — the linked page
  (`bayescm.com/aplicacao-de-modelos-alternativos-para-selecao-de-acoes-no-mercado-brasileiro/`).
  **This is a different underlying study from Rubesam's** — its model set is
  OLS/Ridge/LASSO/PCR/PLS/IPCA + RF + 2 NN architectures + a "Comb4" ensemble, with
  a 24m-train / 12m-validate / 12m-test window expanding every 6 months. No GBRT/XGB
  is described. The page does not disclose numeric hyperparameter grids, batch
  sizes, epochs, ensemble sizes, or λ ranges — confirmed by direct fetch, not
  assumed. Included as a column for completeness; almost every cell is "not
  disclosed."
- **`run_paper_machine.R`** — this script's realigned grid arm (`GRID_OVERRIDES`).
- **pipeline (unmodified)** — the pipeline's own `*_grid`/`*_bayes` configs in
  `backtest_base_signal_blending.R`, before this script's patch.

Ranges are quoted as given; where a source fixes a value rather than tuning it,
that is stated.

---

## Random Forest

| | GKX | H&K | Rubesam | BayesCM | pipeline (unmodified) | **this script** |
|---|---|---|---|---|---|---|
| trees | 1–1000 (tuned) | 100–600 (tuned) | **300, fixed** | not disclosed | 200–800 (bayes) / 500 fixed (grid) | **500, fixed** (matches Rubesam) |
| depth | 1–6 | 1–8 | max splits 1–50 | not disclosed | 2–10 (bayes) / 2,4,8,10 (grid) | **2,4,6,8** (H&K-bounded) |
| features/split | — (uses √P dropout, not tuned) | R ∈ [0.01, 1] (share) | 10-value equally-spaced grid (unspecified upper bound) | not disclosed | mtry 0.1–1 (bayes) / 0.25,0.5,0.75,1 (grid) | **0.25,0.5,0.75,1.00** — floor is high; see note |
| min.bucket / node size | not a separate tuned param | not tuned | not tuned | not disclosed | 1–10 (bayes) / 1,5,10 (grid) | **5, fixed** (neither paper tunes it) |

**Note on `mtry`.** GKX use fixed √P dropout (not tuned); Rubesam's grid runs from
a low share up to 1 across 10 points; H&K allow down to 0.01. This script's floor of
0.25 — a quarter of all 108 signals per split — never explores the low-mtry regime
that produces RF's decorrelation benefit in either reference. **This is the one
deviation worth actually fixing**, not just disclosing: extending down to ~0.05–0.10
costs nothing extra (RF is already the cheapest tuned model in the arm).

---

## Boosting (GBRT / XGBoost)

| | GKX | H&K | Rubesam | BayesCM | pipeline (unmodified `xgb_grid`) | **this script** |
|---|---|---|---|---|---|---|
| algorithm | GBRT (Bühlmann–Hothorn) | GBRT | Friedman (2001) LS_Boost | not applicable (no boosting model) | XGBoost | XGBoost |
| trees / iterations | 1–1000 | 50–500 (tuned) | 1–500 (tuned) | — | fixed at 500 | **100, 250, 500** (tuned, per H&K) |
| learning rate | 0.01–0.1 (tuned) | 0.01–0.1 (tuned) | **fixed at 0.0025** | — | 0.001, 0.01, 0.05 (grid) / 0.005–0.10 (bayes) | **0.01, 0.05, 0.10** (H&K-bounded) |
| depth | 1–2 | 1–8 | max splits 1–50 | — | 3,5,8 (grid) / 2–8 (bayes) | **1,2,4,6** (shallow, small panel) |
| min_child_weight | not used (tree algo differs) | not used | not used | — | 1,5 (tuned) | **1, fixed** — tuned by neither paper |
| subsample | not used | not used | not used | — | 0.8, fixed | **0.8, fixed** |
| colsample_bytree | not used | not used | not used | — | 0.50, 0.75 (tuned) | **0.8, fixed** — tuned by neither paper |
| alpha / gamma | not used | not used | not used | — | 0,5 / 0, fixed (tuned/fixed) | **0, fixed** (both) |

**The pipeline's unmodified `xgb_grid` was the worst-aligned config in the whole
file**: it fixed `nrounds` — the one hyperparameter every boosting reference tunes
— and instead tuned `min_child_weight`, `colsample_bytree`, and `alpha`, none of
which appear in GKX, H&K, or Rubesam's boosting specification. This script's
realignment restores the three-parameter design common to GKX and H&K.

**Rubesam's fixed LR of 0.0025 is a genuine outlier** — an order of magnitude below
both GKX's and H&K's floor of 0.01. Worth a footnote if your results are compared
against his specifically, since a much smaller learning rate at a comparable
iteration cap implies a much shallower effective fit.

**Checkpoint evidence, not a citation.** In the actual `xgb_grid` run,
`best_iteration` ranged 3–130 (mean 30.5) against nominal caps of 100/250/500 —
early stopping bound every single fit before `nrounds` did. Practically, `nrounds`
is non-binding in this setup regardless of which of the three values is nominally
selected; report boosting length as early-stopping-governed, not grid-selected.

---

## Neural Networks

| | GKX | H&K | Rubesam | BayesCM | pipeline (unmodified) | **this script** |
|---|---|---|---|---|---|---|
| architectures | NN1–NN5, pyramid 32→16→8→4→2 | NN1–NN5, same pyramid | NN1–NN5, same pyramid | "2 neural network architectures," unspecified | NN1 (32), NN2 (32,16,**8**\*), NN3 (32,16,8) | NN1 (32), NN2 **(32,16)**\*\*, NN3 (32,16,8) |
| activation | ReLU | ReLU (implied, follows GKX) | **sigmoid** | not disclosed | ReLU | ReLU |
| l1 penalty λ₁ | 1e-5 – 1e-3 (tuned) | 1e-5 – 1e-3 (tuned) | **not tuned** (absent from Table A.1) | not disclosed | 1e-2, 1e-3, 1e-5 (grid) / 1e-5–1e-2 (bayes) | **1e-5, 1e-4, 1e-3** (H&K-bounded) |
| learning rate | {0.001, 0.01} (tuned) | 0.001 – 0.1 (tuned) | **not tuned** | not disclosed | 1e-2,1e-3,1e-4 (grid) / 1e-4–1e-2 (bayes) | **0.001, 0.01, 0.10** (H&K-bounded) |
| l2 / dropout | not stated as tuned | not stated as tuned | not stated | not disclosed | l2 0,1e-4; drop 0.25,0.50 (grid) / drop 0.25–0.75 (bayes) | **l2 = 0, drop = 0.25, fixed** |
| batch size | **10,000, fixed** | **10,000, fixed** | not stated | not disclosed | 256 (grid, fixed) / 128–512 (bayes) | **256, fixed** — see note |
| epochs | **100, fixed**; early stopping governs actual length | **100, fixed** | early stopping only (sole listed hyperparameter) | not disclosed | 100 (grid, fixed) / 60–110 (bayes) | **100, fixed** |
| ensemble size | **10, fixed** (not tuned; see appendix note below) | **10, fixed** ("as in De Nard et al. 2022") | **50, fixed** | not disclosed | **1** (no ensembling implemented) | **10, fixed**; independent initialization draws are averaged after hyperparameter selection |

\* Unmodified pipeline `nn2_bayes` used `c(32,16,8)` — identical to `nn3_bayes`,
almost certainly a copy-paste defect (now fixed on `fix/sb-nn-config-defects`).
`nn2_grid`'s hardcoded `training_sample_size = 120` / `validation_sample_size = 60`
(vs. 82/36 everywhere else) is the companion defect, also fixed on that branch.

\*\* This script's / the fixed pipeline's `nn2` architecture, `c(32,16)`, matching
GKX/H&K/Rubesam's shared pyramid rule for a two-layer network.

**Batch size, 256 vs. 10,000.** Both GKX and H&K train on tens of millions of
firm-months; 10,000 is a small fraction of one training window for them. This
panel's training window is roughly 16k rows. 256 keeps the batch a genuine
subsample of the data (true SGD) rather than approaching full-batch; state this
as an intentional adaptation to sample size, not an unexplained deviation.

**Ensemble size.** The current script sets `n_ensembles = 10` for the neural-network
refit. Hyperparameter selection is conducted with one network per candidate;
after selection, ten independently initialized networks are refitted and their
predictions are averaged. This follows the use of random initializations and
averaging in Gu, Kelly, and Xiu (2020), while avoiding a tenfold increase in the
search budget.

---

## Elastic net / GLMNET

| | GKX | H&K | Rubesam | BayesCM | pipeline `glmnet_grid` (unmodified, **unmodified in this script too**) |
|---|---|---|---|---|---|
| λ | tuned, grid unspecified in main text | 1e-5 – 1e-2 | default `glmnet` grid, 100 values | not disclosed | `lambda.min.ratio` 0.1–0.7 (7 values) |
| α | tuned | 0 – 1 | not applicable (LASSO/Ridge run separately, not elastic net) | not disclosed | 0.25, 0.50, 1.00 |

`lambda.min.ratio` parameterises the *ratio* defining a lambda path, not λ itself,
so this row is not a like-for-like comparison — flagged in the script's own
comments and left untouched for that reason (cheap at 21 fits/date regardless).

---

## Bayesian-optimisation bounds (the `_bayes` arm)

None of the four literature sources use Bayesian optimisation — GKX, H&K and
Rubesam all validate over a grid; BayesCM's page does not name a tuning method.
So there is no literature column to compare against here; this table instead
documents what changed between the pipeline's original bounds and this script's
patch, for the record.

| model | parameter | pipeline (unmodified) | this script (`balanced` preset) |
|---|---|---|---|
| glmnet_bayes | α, λ.min.ratio bounds | (0,1), (0.0001, 0.5) | unchanged |
| glmnet_bayes | init/n_iter/k_iter | 8 / 25 / 8 | 8 / 16 / 8 |
| rf_bayes | mtry, trees, depth, min.bucket bounds | (0.1,1), (200,800), (2,10), (1,10) | unchanged |
| rf_bayes | init/n_iter/k_iter | 12 / 50 / 4 | 10 / 30 / 10 |
| xgb_bayes | 8 hyperparameter bounds | see pipeline source | unchanged except `nrounds` |
| xgb_bayes | `nrounds` bound | (200, 1000) | **(200, 300)** — see caveat below |
| xgb_bayes | init/n_iter/k_iter | 15 / 60 / 3 | 12 / 36 / 12 |
| nn1/nn2/nn3_bayes | l1,l2,droprate,lr,batch,epochs bounds | (1e-5,1e-2),(1e-5,1e-3),(0.25,0.75),(1e-4,1e-2),(128,512),(60,110) | unchanged |
| nn1/nn2/nn3_bayes | init/n_iter/k_iter | 12 / 65–75 / 1 | 10 / 30 / 10 |

**Caveat on the `xgb_bayes` nrounds cap, now that the grid-arm checkpoint has been
read.** The same evidence that showed `nrounds` non-binding in the grid arm
(`best_iteration` 3–130 against caps of 100–500) applies here too: capping the
*bound* at 300 rather than letting early stopping govern a wider bound is
defensible for cost, but it does narrow the model class at the low-`eta` corner of
the search space (see the earlier discussion of `eta × nrounds` coupling). Not
contradicted by anything above, but restated here since this is the reference
document for exactly this kind of grid/bound decision.

---

## Summary of what a referee would see

1. **RF, boosting and NN grids are now within the union of GKX's and H&K's ranges**,
   with Rubesam's fixed-LR boosting and untuned-NN specification noted as the more
   conservative alternative reading.
2. **One real gap remains**: RF's `mtry` floor (0.25) doesn't reach the low-mtry
   region both GKX (implicitly, via √P) and Rubesam's 10-point grid explore. Free
   to fix.
3. **Ensemble size (ensembling at all) is the largest disclosed simplification** —
   three sources fix 10, 10, and 50; this script currently runs 1, with a proposal
   pending to add 10.
4. **BayesCM cannot be used as a grid reference** — it's a different study with no
   published hyperparameter table, included here only because it was supplied as a
   source.

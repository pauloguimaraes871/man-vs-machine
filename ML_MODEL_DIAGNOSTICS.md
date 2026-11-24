# ML Model Diagnostics — Findings and Decisions

Evidence base for three research-design questions:

- **(a)** Is it worth training more models?
- **(b)** Is it worth adjusting the tuning grid, or increasing the tuning budget?
- **(c)** Is it worth changing the target and `chosen_eval_metric`?

All numbers come from the pinned paper run (`manifest_2026_07.qs`, individual-signals
specification, `ibov` mandate). Both tuning arms were inspected: `mb_grid.qs`
(grid search) and `mb_bayes.qs` (Bayesian optimization). 12 annual refits (June),
36-month validation window, 131–133 OOS months, 7 base learners.

---

## 0. Headline

| Question | Answer | Confidence |
|---|---|---|
| (a) More models / deeper NNs? | **No.** Depth is exhausted at 1 layer; NN1–NN2–NN3 rank-correlate 0.918. | High |
| (b) Adjust the grid? | **Marginally.** Three floors bind, but only the NN learning rate could plausibly matter. | Medium |
| (b) Increase tuning budget? | **No.** BO with a larger budget does not beat grid, and overfits the validation set 4–6× more. | High |
| (c) Change target / eval metric? | **Yes — the highest-value change of the three.** But region-restriction beats asymmetric loss ~2:1. | High |

The single most important number in this document: the metric currently used for
model selection (`rmse`) has a rank correlation with realized portfolio Sharpe of
**+0.04 to +0.18** — near zero, and the wrong sign. The same metric restricted to
the predicted top tercile has a rank correlation of **−0.79 to −0.86**.

---

## 1. Are the models healthy?

**Yes.** Nothing here requires retraining or repair.

### 1.1 Generalization gap (OOS minus validation RMSE)

| Model | Grid gap | Grid % | Bayes gap | Bayes % |
|---|---|---|---|---|
| NN3 | +0.070 | 0.32% | +0.188 | 0.85% |
| NN2 | +0.031 | 0.14% | +0.159 | 0.72% |
| NN1 | +0.027 | 0.12% | +0.138 | 0.63% |
| XGB | +0.065 | 0.29% | +0.066 | 0.30% |
| GLMNET | +0.031 | 0.14% | +0.031 | 0.14% |
| RF | −0.040 | −0.18% | −0.021 | −0.09% |

Every gap is under 1% of the metric level and RF's is negative. There is no
validation-set overfitting anywhere.

**Note the arm asymmetry.** The Bayes arm's neural-network gaps are 4–6× the grid
arm's. This is the expected signature of a finer search: BO exploits the
validation set harder, so it overfits the *tuning* set more. It is the one clean
respect in which grid search is better, and it supports keeping grid as the
headline arm.

### 1.2 No degradation over time

Linear trend of OOS RMSE on time (grid arm):

| Model | slope/yr | t | p |
|---|---|---|---|
| XGB | +0.111 | 0.69 | 0.49 |
| GLMNET | +0.108 | 0.67 | 0.51 |
| RF | +0.105 | 0.64 | 0.52 |
| NN2 | +0.092 | 0.56 | 0.58 |
| NN1 | +0.087 | 0.53 | 0.60 |
| NN3 | +0.072 | 0.44 | 0.66 |
| OLS | +0.069 | 0.42 | 0.68 |

All p > 0.49. Hit-rate trends all p > 0.24. Year-to-year swings (2017 RMSE 17.5,
2024 RMSE 25.4) are volatility regimes common to all seven models, not staleness.

**No model needs retraining.** With annual refits there is no action to take.

### 1.3 The OLS feature-importance collapse is not overfitting

Post-2018, OLS surrogate coefficients blow up (max |β| 3.4 → 108.4, Σ|β| 201.4)
while net importance collapses to near zero. This is multicollinearity /
variance inflation, not overfitting:

- OLS RMSE degrades **least** across the 2018 boundary (+0.480 vs GLMNET +0.848, XGB +0.861)
- Its cross-model rank *improves* (5.81 → 4.84)
- Hit rate improves (+0.0234)
- Cancellation ratio |Σβ| / Σ|β| falls 0.45 → 0.057
- Post-2018 gross loading is concentrated almost entirely in `momentum`

OLS is a weak model throughout (worst full-sample RMSE, 23.208) but it is not
*newly* broken at 2018.

Related: `momentum` has the widest gross envelope in **every** model, so
within-theme offsetting there is a property of the signal set, not an OLS artifact.

---

## 2. (a) Is it worth training more models?

**No.** Two independent lines of evidence.

### 2.1 The models already agree

Mean per-date cross-sectional rank correlation of predictions, 131 dates:

|        | OLS | GLMNET | RF | XGB | NN1 | NN2 | NN3 |
|--------|-----|--------|----|-----|-----|-----|-----|
| OLS    | 1.000 | 0.754 | 0.710 | 0.631 | 0.808 | 0.773 | 0.779 |
| GLMNET | 0.754 | 1.000 | 0.797 | 0.634 | 0.849 | 0.785 | 0.772 |
| RF     | 0.710 | 0.797 | 1.000 | 0.760 | 0.851 | 0.826 | 0.810 |
| XGB    | 0.631 | 0.634 | 0.760 | 1.000 | 0.680 | 0.662 | 0.671 |
| NN1    | 0.808 | 0.849 | 0.851 | 0.680 | 1.000 | 0.930 | 0.899 |
| NN2    | 0.773 | 0.785 | 0.826 | 0.662 | 0.930 | 1.000 | 0.925 |
| NN3    | 0.779 | 0.772 | 0.810 | 0.671 | 0.899 | 0.925 | 1.000 |

- Mean off-diagonal: **0.776**
- **NN1–NN2–NN3 mean pairwise: 0.918** — effectively the same model three times
- XGB is the most differentiated learner (0.631–0.760), and is therefore the one
  worth *keeping* for ensemble diversity even though it has the worst Sharpe

### 2.2 Depth buys nothing

Paired monthly OOS differences, 131 months:

| Comparison | Grid ΔRMSE | p | Bayes ΔRMSE | p |
|---|---|---|---|---|
| NN2 − NN1 | −0.016 | 0.53 | +0.002 | 0.93 |
| NN3 − NN2 | **+0.053** | **0.020** | +0.029 | 0.42 |
| NN3 − NN1 | +0.037 | 0.27 | +0.030 | 0.38 |

The third layer is **significantly worse** in the grid arm and insignificantly
worse under BO. The second layer buys nothing. Hit rate agrees (NN2 − NN1 =
−0.005, p = 0.095, i.e. NN2 slightly *worse*).

`FIX_NN2_ARCHITECTURE <- TRUE` is set in `run_paper_machine.R:135`, so the Bayes
ladder is three genuinely distinct architectures. The null is real, not the
`c(32,16,8)` collision.

### 2.3 Decision

**Do not add models or layers.** The complexity ladder is exhausted at one hidden
layer. This matches Gu–Kelly–Xiu (shallow beats deep in asset pricing) and is
*stronger* evidence than theirs, because it replicates under two independent
tuning methods.

**Pipeline-project implication:** keep NN1, drop NN2 and NN3. Retain XGB for
diversity despite its weak standalone Sharpe. A defensible minimal roster is
**GLMNET + RF + XGB + NN1** (plus OLS as the linear baseline) — a straight ~30%
cut in learner-fitting time with no measurable loss.

---

## 3. (b) Tuning grid and tuning budget

### 3.1 Where the search space actually binds

Ignore frozen (single-valued) hyperparameters — `num.trees`, `min.bucket`,
`subsample`, `colsample_bytree`, `alpha`, `gamma`, `regularizer_l2`, `droprate`,
`size_of_batch`, `number_of_epochs`. They show as 12/12 at both edges by
construction and carry no information.

Among genuinely tuned parameters:

| Model | Parameter | Grid edge-hits | Bayes edge-hits | Reading |
|---|---|---|---|---|
| NN1/2/3 | `lr` | LOW 8, 5, 6 of 12 | LOW 4, 4, 3 of 12 | **wants lr below 0.001** — replicates across arms |
| RF | `mtry` | LOW 9/12 (0.25) | LOW 4/12 (0.1) | wants less feature sampling than the floor |
| GLMNET | `lambda.min.ratio` | LOW 5/12; last 5 refits all at 0.1 | LOW 4/12 | wants a lower floor |
| GLMNET | `alpha` | LOW 7 + HIGH 5 = **12/12** | 2 + 4 of 12 | never picks the interior value |
| XGB | `eta` | HIGH 8/12 (0.1) | HIGH 1/12 | grid wants a higher learning rate |
| XGB | `max_depth` | HIGH 4/12 | HIGH 5/12 (8) | wants more depth |

Two things worth reporting in the paper:

1. **The NN learning-rate floor binds in both arms.** This is the only edge-hit
   that replicates across tuning methods and is therefore the only one likely to
   move results.
2. **GLMNET `alpha` lands on an edge in all 12 grid refits and never on 0.5.**
   With a domain of {0.25, 0.5, 1} the elastic-net mixing parameter is genuinely
   bimodal, bouncing between ridge-like and lasso-like. That is a substantive
   statement about the signal set, not a tuning failure.

### 3.2 Is a bigger budget worth it?

**No.** The Bayes arm searches a continuous space with a larger budget and:

- does not beat grid on OOS RMSE (differences are in the third decimal)
- overfits the validation set 4–6× more on the neural networks (§1.1)
- collapses the NN3-worse-than-NN2 signal into noise, i.e. buys *less* clarity

### 3.3 Decision

**Extending the NN learning-rate floor below 0.001 is the only change worth
making, and even that is a robustness check rather than an expected improvement.**
Do not increase the tuning budget.

**Pipeline-project implication:** the tuning budget can be cut substantially.
Grid search over a small, well-chosen domain matches BO here. Combined with the
NN1-only decision from §2, this is a large speed gain with no measured cost.
Before cutting, widen the three binding floors so the smaller grid is not also a
truncated one.

---

## 4. (c) Target and `chosen_eval_metric` — the real opportunity

### 4.1 Current configuration

Every model trains and tunes identically:

```
objective = squared_error   tau = 0.5   huber_delta = 1   chosen_eval_metric = rmse
```

Target: `comdinheiro_target_fwd_act_cum_ret_3m` (3-month forward cumulative return).

### 4.2 `rmse` does not predict portfolio performance

| Model | OOS RMSE | rank | Hit rate | Sharpe (RP) | rank |
|---|---|---|---|---|---|
| NN2 | 23.020 | **1** | 0.5170 | 0.796 | 6 |
| NN1 | 23.048 | 2 | 0.5225 | 0.803 | 4 |
| NN3 | 23.061 | 3 | 0.5208 | 0.809 | 3 |
| RF | 23.069 | 4 | 0.5110 | 0.841 | 2 |
| XGB | 23.114 | 5 | 0.5089 | 0.764 | 7 |
| GLMNET | 23.126 | 6 | **0.4956** | **0.857** | **1** |
| OLS | 23.208 | 7 | 0.5130 | 0.797 | 5 |

**Spearman(RMSE, Sharpe) = +0.107 (CS), +0.179 (RP)** — near zero and the *wrong*
sign. GLMNET has the worst hit rate of all seven (below a coin flip) and the best
Sharpe in both construction schemes.

Why a level metric fails here:

1. **RMSE scores the level; the portfolio only uses the cross-sectional ranking.**
   A miscalibrated model can order stocks correctly and never be rewarded.
2. **RMSE weights all stocks equally; the portfolio only touches the tails.**
   ~99% of squared-error mass comes from stocks that are never held. Accuracy can
   be bought in the middle of the distribution, where it is easy and irrelevant.
3. **Portfolio return is a covariance, not an error** — roughly
   `cov(pred, realized) / dispersion`. This can rise while RMSE also rises.

### 4.3 The decisive test: restrict the metric to the traded region

Per-model accuracy from raw OOS predictions. `RMSE_top3` = RMSE restricted to
each model's own predicted top tercile:

| Model | IC | top3_ret | LS spread | **RMSE_top3** | MAE_top3 | RMSE_all |
|---|---|---|---|---|---|---|
| NN1 | 0.1265 | 1.409 | 3.942 | 17.447 | 13.100 | 22.247 |
| RF | 0.1246 | 1.430 | 3.854 | 18.003 | 13.454 | 22.265 |
| NN3 | 0.1226 | 1.396 | 3.966 | 18.284 | 13.768 | 22.284 |
| GLMNET | 0.1223 | 1.335 | 3.495 | **17.064** | 12.812 | 22.328 |
| NN2 | 0.1215 | 1.503 | 3.962 | 18.384 | 13.671 | 22.230 |
| XGB | 0.1073 | 1.372 | 3.582 | 19.012 | 14.036 | 22.315 |
| OLS | 0.1051 | 1.255 | 3.500 | 18.679 | 13.988 | 22.431 |

Rank correlation of each candidate metric with realized portfolio Sharpe (n = 7):

| Metric | CS | RP |
|---|---|---|
| **RMSE_top3** | **−0.786** | **−0.857** |
| MAE_top3 | −0.679 | −0.750 |
| IC | +0.643 | +0.571 |
| BIAS_top3 | +0.571 | +0.500 |
| mb (mean bias) | −0.500 | −0.464 |
| IC_IR | +0.321 | +0.179 |
| mape | +0.321 | +0.250 |
| rmse | +0.107 | +0.179 |
| hr | −0.107 | −0.179 |
| **RMSE_all** | **+0.036** | **+0.143** |
| mae / mphe / mpe | +0.071 | +0.143 |

For error metrics, strongly negative is what we want.

**The same loss function, restricted to the traded region, goes from useless
(+0.04) to the best available predictor of realized Sharpe (−0.86).** GLMNET
ranks 1st on `RMSE_top3` and 1st on Sharpe; XGB ranks 7th and 7th. Under
`RMSE_all` the ordering is scrambled: NN2 is 1st on error and 6th on Sharpe; RF
is 6th on error and 2nd on Sharpe.

### 4.4 Important negative result: `mpe` as configured is redundant

`mae / mpe = 2.000000` for all eight models. `mpe` is pinball (quantile) loss and
`quantile_tau = 0.5`, at which pinball loss is exactly MAE/2. **As currently
configured, `mpe` is a monotone transform of MAE and produces an identical model
ranking.** It cannot add information.

This does *not* kill the idea — it locates it precisely. Pinball loss becomes
asymmetric and upper-tail-focused only when `tau` moves away from 0.5. Setting
`tau = 0.7–0.9` makes the loss penalize *under*-prediction of high outcomes more
than over-prediction, which is exactly the long-only top-tercile objective.
**The lever is `quantile_tau`, not the choice of `mpe` over `rmse`.**

Similarly, `mphe` (pseudo-Huber, delta = 1) is a symmetric robust loss. It
downweights outliers relative to RMSE but is still a whole-distribution,
symmetric criterion, so it should not be expected to fix the ranking problem.

### 4.4b Scoring existing predictions under pinball — what this does and does NOT test

**Scope warning.** Everything in this subsection re-scores the **existing**
predictions, which were trained *and tuned* on `rmse`. It asks: across the seven
learners, does metric X rank them the way realized Sharpe does?

It does **not** test what happens if `chosen_eval_metric = "mpe"` is set and the
backtests are re-run — that changes which hyperparameters win at each refit and
therefore changes the models themselves. See §4.4c for that experiment, which
these numbers can only motivate, not substitute for. If anything they are an
optimistic bound: swapping model families moves predictions far more than
re-tuning within one family does.

Pinball loss recomputed on the existing OOS predictions at several `tau`, and on
the predicted top tercile, scored by Spearman with realized Sharpe (n = 7):

| Metric | CS | RP | mean |
|---|---|---|---|
| **RMSE_top3** (region-restricted, symmetric) | −0.786 | −0.857 | **−0.821** |
| top-tercile pinball, tau = 0.60 / 0.67 / 0.75 | −0.750 | −0.821 | −0.786 |
| top-tercile pinball, tau = 0.50 | −0.679 | −0.750 | −0.714 |
| **whole-sample pinball, tau = 0.67** | **−0.464** | **−0.429** | **−0.446** |
| whole-sample pinball, tau = 0.75 | −0.464 | −0.429 | −0.446 |
| whole-sample pinball, tau = 0.90 | −0.429 | −0.393 | −0.411 |
| whole-sample pinball, tau = 0.60 | −0.250 | −0.071 | −0.161 |
| **whole-sample pinball, tau = 0.50 (= current `mpe`)** | **+0.071** | **+0.143** | **+0.107** |

Three conclusions:

1. **`tau = 0.67` is a genuine improvement**: +0.107 → −0.446. Moving off the
   median is doing real work, not cosmetic.
2. **Region restriction is roughly twice as effective**: −0.821 vs −0.446. If
   only one change is made, restrict the region, not the asymmetry.
3. **Combining them adds nothing** over region alone (−0.786 vs −0.821).

The model *ordering* under `tau = 0.5` versus `tau = 0.67` has a rank
correlation of **−0.107** — moving tau almost completely reshuffles which model
wins. So this is a substantive change to the selection rule, not a tweak.

**Why asymmetry helps less than region.** Pinball at `tau > 0.5` fits a higher
conditional quantile, which shifts predictions *up*. Because the portfolio only
uses the cross-sectional *ranking*, a uniform level shift changes nothing. The
ranking moves only where the shift is uneven — i.e. where conditional variance
differs across stocks. So `tau > 0.5` is partly a **volatility tilt**: it pushes
high-dispersion stocks up the ranking. That may be desirable, but it is a
different mechanism from "care more about the stocks I actually buy", and it
should be described as such rather than as tail-focus.

### 4.4c The actual experiment: re-tune on pinball

factoRverse supports this directly. `calculate_eval_metrics()` accepts
`chosen_eval_metric` in `{rmse, rss, cp, mae, mphe, mpe, mape, hr}` and computes

```r
validation_sample_mpe <- mean(ifelse(error >= 0,
                                     quantile_tau * error,
                                     (1 - quantile_tau) * (-error)))
```

with `error = target - pred`. So `error >= 0` is **under**-prediction and takes
weight `quantile_tau`. At `tau = 0.67` under-prediction is penalized 0.67 against
0.33 for over-prediction, the fitted object is the 67th conditional quantile, and
predictions shift up. The tuner maximizes `Score = -mpe`, so the sign is handled.

**The two arms as implemented are deliberately asymmetric:**

| Arm | `chosen_eval_metric` | `custom_objective` | Channel |
|---|---|---|---|
| `rmse` | rmse | squared_error | baseline |
| `mpe` | mpe, tau 0.67 | squared_error | **selection only** |
| `mphe` | mphe, delta 1 | **pseudo_huber_error** | **selection + training** |

`translate_metrics()` maps `pseudo_huber_error` to `reg:pseudohubererror` for
xgb and `keras::loss_huber(delta)` for the NNs, and to **NULL** for ols, glmnet
and rf — those take no custom objective. So on the `mphe` arm glmnet and rf move
through selection only, while xgb and nn1–nn3 move through both. That splits the
arm into two readable groups: a difference confined to glmnet/rf is a selection
effect; one appearing only in xgb/nn is a training-objective effect.

**Two caveats.**

1. On the `mpe` arm, `translate_metrics()` does not support `mpe` as an
   in-training metric for xgb or nn and falls back to `rmse` for the
   **early-stop** criterion, while still applying `mpe` to hyperparameter
   selection (factoRverse warns at runtime). Selection is the channel under
   test, but the arm is not a pure pinball run and should not be described as
   one.
2. **OLS is a free control across all three arms, including `mphe`.** It is
   untuned, and class validity forbids ols configs from carrying any
   `custom_objective` other than the `"squared_error"` default at all —
   confirmed by `validObject()` rejecting an attempted `ols`/`pseudo_huber_error`
   assignment outright when this was first run (see the run log below). So OLS
   is bit-identical across `rmse`, `mpe` and `mphe`; if its portfolio moves at
   all, that is a plumbing bug.

**The underlying knobs:**

| Slot | Current | Effect of changing it |
|---|---|---|
| `tuning_strategy@chosen_eval_metric` | `"rmse"` | which hyperparameters win at each refit |
| `sb_backtest_config@custom_objective` | `"squared_error"` | what the model minimizes during fitting |
| `sb_backtest_config@quantile_tau` | `0.5` | feeds `mpe` **and** the pinball objective if selected |

Setting `chosen_eval_metric = "mpe"` and `quantile_tau = 0.67` while leaving
`custom_objective = "squared_error"` is the **clean isolated experiment**:
training is unchanged, only hyperparameter selection moves. That is the version
worth running first.

**Free control:** OLS is untuned and has no `tuning_strategy`, so it is
bit-identical across arms. Any change in the OLS portfolio would indicate a
plumbing bug rather than a real effect.

**What to expect.** Modest. Hyperparameter selection within a family moves
predictions much less than swapping families. §4.4b's whole-sample tau result
(+0.107 → −0.446) is a ceiling on the effect, not a forecast. The honest prior is
that this is worth *one* run to find out, not a program of work.

**Cost.** A full re-run of the sb + mb + meta stages for the grid arm. Do not run
both tuning arms — §3.2 already establishes BO adds nothing.

**How to run it** (implemented in `run_paper_machine.R`):

```bash
PAPER_EVAL_METRIC=mpe Rscript run_paper_machine.R
```

`PAPER_EVAL_METRIC` is `rmse` (default), `mpe` (pinball, `quantile_tau = 0.67`)
or `mphe` (pseudo-Huber, `delta = 1`). It is orthogonal to `PAPER_ARM`.
Artefacts are tagged — `mb_grid_mpe.qs`, `meta_grid_mpe.qs`,
`mb_grid_mphe.qs` — and the `rmse` tag is empty, so the existing
`mb_grid.qs` / `meta_grid.qs` keep their names and stay valid.

**Before the first metric run, move the SB checkpoint aside:**

```bash
mv data/checkpoints/backtest_base_signal_blending_checkpoint/backtest_base_signal_blending_checkpoint_2026-07-15.qs /tmp/
```

The checkpoint path is keyed on the gold vintage **alone** — not the arm, not
the metric — and `backtest_base_signal_blending()` skips any model already in
it. Config names are identical across metric arms, so without this the mpe run
would skip every model and hand back rmse-tuned learners wearing an mpe label.
The run now aborts with an explicit message rather than doing that silently, but
the checkpoint still has to be moved. The baseline does not need it: `sb_grid.qs`
already exists, so the checkpoint only matters for resuming an interrupted run.
This applies going into **every** arm switch, not just the first: after the
`mpe` run finished, its checkpoint was still sitting at the same path and had to
be moved aside again before `mphe` could start — the guard caught it and
aborted rather than silently reusing mpe-tuned models.

**Two bugs the first live runs surfaced, both now fixed in `run_paper_machine.R`:**

- The tuning-metric block originally applied to *every* config
  `create_sb_configs()` builds, not just `SB_CONFIGS`. The pipeline's full
  base-learner pool also includes heuristic configs (`sw_lt_perf`, `ew`, …)
  carrying a heuristic `custom_objective` like `"max_lt_perf_agg_z_score"`;
  overwriting that with `EVAL_OBJECTIVE` failed class validity. Fixed by
  gating the whole block on `nm %in% SB_CONFIGS`.
- Even within `SB_CONFIGS`, assigning `custom_objective = "pseudo_huber_error"`
  to `ols` (on the `mphe` arm) failed class validity: `"Custom objectives are
  only allowed for 'sw', 'rp', 'hrp', 'mvo', 'mmaf', 'xgb' or 'nn'
  algorithms."` This is stricter than assumed — it isn't just that
  `translate_metrics()` returns `NULL` for ols/glmnet/rf and the assignment is
  inert; the config object cannot hold a non-default value there at all. Fixed
  by gating the `custom_objective` assignment on `cfg@sb_algorithm %in%
  c("xgb", "nn")`. The net effect is unchanged — glmnet/rf/ols still move
  through selection only on `mphe` — but by never touching the slot, not by
  the translation being a no-op.

**What to compare.** Realized net Sharpe/IR of the resulting portfolios, not the
training metric — that is the whole point of §4.2. OLS is the control and must
not move.

### 4.5 Decision and recommended experiment

**Yes, this is worth pursuing, and it is the highest-value of the three
questions.** But the finding is sharper than "pick a different metric":

> The problem is not the *loss family* (squared vs absolute vs Huber). It is that
> the metric is evaluated over the *whole cross-section* when the strategy only
> trades one tercile of it.

Two distinct levers, in priority order:

1. **`chosen_eval_metric` (selection).** Cheapest and best-evidenced. Selecting
   hyperparameters that minimize error *in the predicted top tercile*, or
   maximize rank IC, would have chosen better portfolios. Requires a
   region-restricted or rank-based metric in the tuning loop.
2. **`custom_objective` (training).** More invasive. XGB and the NNs both accept
   `pinball` and `pseudo_huber`. An asymmetric pinball objective with
   `tau = 0.7–0.9` aligns the *training* loss with the long-only mandate.

Suggested minimal experiment, framed as a challenger branch against the current
production baseline:

- Baseline: `squared_error` / `rmse` (current)
- C1: `squared_error` training, **top-tercile RMSE** selection metric
- C2: `squared_error` training, **rank IC** selection metric
- C3: `pinball` training at `tau = 0.8`, `rmse` selection
- C4: `pinball` training at `tau = 0.8`, top-tercile selection

Judge on realized net Sharpe/IR of the resulting portfolios, not on the training
metric. With 7 learners × 2 schemes there are enough portfolios to compare
rankings rather than single points.

### 4.6 Caveats

- **n = 7 models.** Spearman at n = 7 needs |ρ| > 0.75 for p < 0.05. The
  `RMSE_top3` result clears that bar in RP (−0.857) and sits just under it in CS
  (−0.786). Strong, but not conclusive.
- **Selection effect.** `RMSE_top3` is evaluated on each model's *own* predicted
  tercile, so it partly measures how well a model calibrates the region it
  chooses to be confident about. That is a feature for this purpose, but it means
  the metric is not strictly comparable across models with very different
  prediction dispersion.
- The comparison is in-sample *with respect to the choice of metric*: a menu of
  metrics was searched and the best reported. A genuine out-of-sample update run
  is needed before claiming this improves anything.

---

## 5. Implications for the paper

1. **Do not present OOS RMSE as evidence for or against the machine.** It is a
   model-fitting diagnostic with near-zero rank correlation to the economic
   outcome. The IR/Sharpe table is the economic result. If RMSE is reported at
   all, report it alongside §4.2 as a *methodological finding*.
2. **The complexity-ladder null (§2.2) is publishable.** Shallow beats deep,
   replicated under two tuning methods.
3. **The GLMNET `alpha` bimodality (§3.1) is worth a sentence.**
4. **The net-vs-gross feature-importance distinction (§1.3)** is already in the
   figures and captions.

---

## 6. Regime analysis of the gap — implemented

`mkt_ret` and `mkt_vol` (the fund's own mandate's benchmark index return and its
trailing 12-month volatility) are now carried out of
`build_gap_panel.R` and a **Spec 3** was added to `analysis_gap.R`. Specs 1 and 2
are untouched; Spec 3's centered intercepts reproduce Spec 1's exactly
(gap 0.281, gap_raw 0.503), confirming both run on the same 14,307-row sample.

Market state is Group C (environment) in the panel taxonomy: constant within
mandate × period, therefore absorbed completely by Spec 2's time FE. That is why
it needs its own no-time-FE specification rather than extra columns on Spec 2.

### 6.1 Sample limitation — two crisis windows are unreachable

**The estimation sample spans 2019-01-15 to 2026-07-15.** The 36-month trailing
beta burn-in plus the ML backtest start push the sample well past the earlier
episodes:

| Window | Dates | In sample? |
|---|---|---|
| Impeachment / 2015-16 recession | 2015-04 to 2016-08 | **No — zero rows** |
| Greve dos Caminhoneiros | 2018-05 to 2018-06 | **No — zero rows** |
| COVID crash and recovery | 2020-02 to 2020-12 | Yes (11 periods) |
| Selic tightening cycle | 2021-03 to 2022-08 | Yes (15 periods) |

The windows are still defined in `CRISIS_WINDOWS` and `regime_label()` so they
activate automatically if the sample is ever extended backwards. `droplevels()`
removes the empty levels; without it the all-zero dummy columns make the two-way
clustered vcov singular.

### 6.2 Result: the gap is NOT state-dependent

Spec 3, two-way clustered, n = 14,307:

| Term | gap | t | gap_raw | t |
|---|---|---|---|---|
| (Intercept) | 0.281 | 1.88 | 0.503 | **3.33** |
| `mkt_ret` | −0.008 | −0.19 | +0.014 | 0.39 |
| `bear` | −0.218 | −0.54 | +0.240 | 0.61 |
| `crisis_f` covid | **−0.985** | **−2.18** | **−1.086** | **−2.59** |
| `crisis_f` ratehike | +0.235 | 0.51 | −0.136 | −0.28 |

Cell means:

| Cut | Regime | n | Periods | Mean mkt | gap | t | gap_raw | t |
|---|---|---|---|---|---|---|---|---|
| direction | UP | 7690 | 53 | +5.30 | 0.366 | 1.83 | 0.482 | 2.18 |
| direction | DOWN | 6618 | 49 | −3.58 | 0.182 | 0.78 | 0.528 | 2.23 |
| tercile | T1 | 4771 | 37 | −4.62 | 0.018 | 0.06 | 0.414 | 1.39 |
| tercile | T2 | 4929 | 40 | +0.65 | 0.410 | 1.75 | 0.608 | 2.63 |
| tercile | T3 | 4608 | 35 | +7.79 | 0.415 | 1.55 | 0.482 | 1.63 |
| crisis | normal | 10576 | 58 | +1.37 | 0.342 | 1.99 | 0.611 | 3.49 |
| crisis | covid | 1471 | 11 | +0.72 | −0.632 | −1.41 | −0.372 | −0.96 |
| crisis | ratehike | 2261 | 15 | +0.64 | 0.589 | 1.32 | 0.566 | 1.21 |

Up/down capture, raw returns:

| Direction | Periods | Avg fund | Machine | Diff |
|---|---|---|---|---|
| DOWN | 49 | −3.36 | −2.84 | **+0.528** |
| UP | 53 | +4.91 | +5.39 | **+0.482** |

**Three findings:**

1. **No ordinary state dependence.** Both `mkt_ret` and `bear` are
   insignificant for both DVs. The raw gap is statistically significant in *both*
   directions (UP t = 2.18, DOWN t = 2.23) and of nearly equal size. **The
   machine's advantage is not a disguised beta bet** — it is not bought by
   winning big in rallies.

2. **The machine wins marginally more on the way down** (+0.528 vs +0.482 pp/mo),
   though the asymmetry is far from significant. Note this *reverses* the sign of
   the preliminary estimate made with the cross-sectional mean fund return as a
   market proxy (which suggested UP 0.577 / DOWN 0.470). The proxy was
   misleading; the mandate-specific benchmark is the right measure.

3. **COVID is a real and significant exception.** The machine underperformed its
   normal edge by about 1 pp/month during 2020 (gap −0.985, t = −2.18; gap_raw
   −1.086, t = −2.59), and the level gap turned negative in that window. This is
   the one episode where the discretionary-manager claim — that human judgment
   earns its keep in a regime break — has support in this sample. It should be
   reported, not buried: it is a bounded, honest limitation that strengthens the
   rest of the result. The rate-hike cycle shows nothing.

The risk-adjusted gap is weakest in the bottom market tercile (0.018) while the
raw gap is stable (0.414), which says the factor-exposure wedge does more work in
falling markets — consistent with the machine carrying a different factor profile
rather than different timing skill.

### 6.3 Volatility regime (Spec 4)

Regimes defined by **trailing 12-month volatility of the mandate's own benchmark**,
computed strictly through t−1, normalized by that mandate's full-sample mean
volatility (`vol_rel`, where 1.0 = typical for that universe), then cut into
within-mandate terciles with the middle tercile as reference.

The volatility is computed in `build_gap_panel.R`, not in `analysis_gap.R`, and
that placement is load-bearing: inside `build_dv()` the market series still spans
the full ML index from 2015-06, whereas the gap does not exist until the 36-month
beta window fills. Computing it downstream burned a second 12-month window off
the front and cost 13% of the estimation sample — including the entire pre-COVID
baseline, which is exactly what the COVID dummy is measured against. Moved
upstream, **252 of 252 mandate-periods are defined and Spec 4 runs on the same
14,307 rows as Spec 3.**

Cell means:

| Regime | Periods | Mean vol_rel | Mean mkt | gap | t | gap_raw | t |
|---|---|---|---|---|---|---|---|
| vol_low | 38 | 0.67 | +1.03 | 0.479 | **2.06** | 0.560 | **2.48** |
| vol_mid | 47 | 0.89 | +0.01 | 0.233 | 0.80 | 0.590 | **2.15** |
| vol_high | 34 | 1.43 | +2.69 | 0.075 | 0.29 | 0.333 | 1.09 |
| (split at mean) LOW | 59 | 0.74 | +0.68 | 0.277 | 1.40 | 0.521 | **2.76** |
| (split at mean) HIGH | 33 | 1.38 | +2.16 | 0.288 | 1.17 | 0.468 | 1.70 |

Spec 4 coefficients (vol_mid reference, alongside direction and crisis terms):

| Term | gap | t | gap_raw | t |
|---|---|---|---|---|
| vol_low | +0.391 | 1.08 | +0.045 | 0.13 |
| vol_high | +0.213 | 0.56 | +0.015 | 0.03 |
| crisis covid | **−1.011** | **−2.18** | **−1.084** | **−2.35** |

**Finding: volatility does not define a regime the gap responds to.** Both
volatility terms are insignificant for both DVs, and adding them leaves the COVID
coefficient essentially unchanged (−0.985 → −1.011 on `gap`).

The one pattern worth noting is descriptive: the **risk-adjusted** gap declines
monotonically across volatility terciles (0.479 → 0.233 → 0.075) while the raw
gap is much flatter (0.560 → 0.590 → 0.333). Read with §6.2's tercile result,
this says the factor-exposure wedge widens when markets are volatile — the
machine's raw advantage survives, but more of it is attributable to factor tilts
rather than residual selection.

**Why the volatility terms come out flat: they are collinear with COVID.** The
COVID window has mean `vol_rel` = 1.54, the highest of any cut in the sample. The
high-volatility regime and the COVID episode are largely the same 11–34 periods.
With both in the model the named-event dummy wins, which is the right outcome —
it says the machine's bad patch was *COVID specifically*, not high volatility in
general. Note also that high-volatility periods here were on average **rising**
markets (mean mkt +2.69), because the window is dominated by the COVID recovery
rather than the crash.

### 6.4 Caveats

- 11 COVID periods and 15 rate-hike periods. These are small windows; the COVID
  coefficient rests on one episode and should not be generalized to "the machine
  fails in crises".
- Crisis windows are fixed **ex ante** from the historical record, never chosen
  by inspecting the gap. A drawdown-threshold rule fitted on this sample would
  select on the DV and manufacture its own result.
- `bear` and `mkt_ret` are both included deliberately: the former captures an
  asymmetry at zero that a linear term cannot represent. They are correlated but
  not collinear.

---

## 7. Reproduction notes

| Analysis | Source objects |
|---|---|
| Generalization gap, OOS trends, boundary hits, NN ladder | `mb_grid.qs`, `mb_bayes.qs` |
| Accuracy vs portfolio, model agreement, metric ranking | `mb_grid.qs`, `learners_grid.qs` |
| Regime split | `data/derived/gap_panel.qs` |

Relevant slots on `sb_backtest_results`: `best_hyperparameters_m_xts`,
`validation_eval_metrics_hyper_choice_m_xts`, `chosen_eval_metric_validation`,
`oos_sb_outputs_m_df` (carries `target` / `pred` / `error` in raw return units),
`sb_backtest_config@tuning_strategy@hyper_grid_domain@hyperparameter_list`.

Gotcha: `base_learners_oos_predictions_m_df` holds **rank-normalized** predictions
in [0, 1] and has **no `target` column**. Use `oos_sb_outputs_m_df` for any
error-based calculation.

# Pipeline code used by the paper

The 26 functions from the Multifatorial 3.0 pipeline that `run_paper_machine.R`
uses, copied unmodified from commit
`8739bb0bce966dad8f7c701f0c7413a6fa25f61f`, the code behind the submitted results.
The pipeline repository is not needed to run the paper.

This is now the paper's own code and may be edited here. List every edit below.

- `tools.R`, `save_checkpoint()`: the checkpoint folder can be redirected with
  `options(mvm.checkpoint_root = )`, so named reruns keep their own checkpoints.

`run_paper_machine.R` wraps `create_sb_configs()` (to apply the paper's settings)
and replaces `update_workflow_with_versions()` (provenance stamps). Both are
visible in sections 2b and 2c of that script.

The Bayesian signal-selection branch, which the paper does not use, needs cmdstanr,
pryr and a machine-specific CmdStan path. Neither package is in renv.lock, so
`renv::status()` lists both. That is expected.

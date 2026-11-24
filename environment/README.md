# Computational environment

## Pins

| Component | Version |
|---|---|
| Pipeline helpers | `8739bb0bce966dad8f7c701f0c7413a6fa25f61f` |
| R | 4.3.0 (Windows x86_64) |
| renv | 1.1.4 |
| factoRverse | 0.7.0, bundled tarball |
| Python | 3.11.14 |
| Python packages | `environment/python-requirements.txt` |

The pipeline pin was chosen from fit timestamps, diffs and fingerprints of the
cached objects. Uncommitted edits at the time of estimation cannot be ruled out.

## Restore

Run from the project root. `renv.lock` refers to the factoRverse tarball by a
path relative to that directory.

1. In R 4.3.0:

       renv::restore()

2. In PowerShell with conda. Turn crash reports off first: with `--yes`, conda
   uploads them without asking, and they contain local paths.

       $env:CONDA_REPORT_ERRORS = "false"

   Create the environment, where conda-forge is reachable:

       conda create --name mvm-keras215 --override-channels -c conda-forge python=3.11.14 pip

   On the Sicoob network conda-forge is blocked by policy (seen 2026-11-22).
   Either request access from IT or, if the package files are already in the
   local conda cache (`pkgs`), create the environment offline by passing the
   path of every required `.conda` file, dependencies included:

       conda create --offline --name mvm-keras215 <path-to-each-.conda-file>

   Then install the pins from PyPI:

       conda activate mvm-keras215
       python -m pip install -r environment/python-requirements.txt

3. Create a `.Renviron` in the project root (not tracked) containing

       RETICULATE_PYTHON=<path to that environment's python.exe>

Re-estimating also needs `data/inputs/machine_2026_07/`, which is not tracked:
copy `data/` along with the repository. The pipeline code is in `R/pipeline/`.

## Limits on the Python pins

The versions were observed on 2026-11-22. They are not a verified copy of the
environment used for the August 2026 runs. File timestamps in that environment show:

- 2026-01-12: initial installation of TensorFlow, Keras and their dependencies.
  No non-bytecode file of TensorFlow, Keras, h5py, ml_dtypes or protobuf has a
  modification time after 1 August.
- 2026-09-10, after the runs: `tf_keras` 2.15.1 was installed or reinstalled.
  Whether it was present in August is not established by the file times.
- 2026-11-21, after the runs: `pandas`, `python-dateutil`, `pytz` and `tzdata`
  were installed. They are omitted from the requirements. NumPy was reinstalled.
  The 14 binaries left from the previous copy, dated January, are byte-identical
  to those of the installed NumPy 1.26.4.

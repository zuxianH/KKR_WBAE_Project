# BAE_Codex Runtime Bundle

This folder contains the current files needed to run `RunSUSYWBE.wl` on another machine.

## Included files

- `RunSUSYWBE.wl`
- `SUSYWBEWorkflow.wl`
- `ValidateSYT221.wl`
- `Project.toml`
- `Manifest.toml`
- `solver/`
- `utils/`
- `julia/`

## What must be installed

- Wolfram Mathematica or Wolfram Engine with `WolframKernel`
- Julia

Tested Julia version from the bundled `Manifest.toml` is `1.12.5`.

The Wolfram side uses `Combinatorica\`` in a few places. That package is part of standard Wolfram installations, so there is nothing extra to install for it.

## First-time setup

Open a terminal in this folder and run:

```bash
julia --project=. -e 'using Pkg; Pkg.instantiate(); Pkg.precompile()'
```

For faster repeated runs, you can optionally build the custom Julia sysimage:

```bash
julia --project=. julia/build_sysimage.jl
```

This creates `.sysimages/susywbe_worker.so` on Linux, `.dylib` on macOS, or `.dll` on Windows.

## Run the workflow

From a terminal:

```bash
WolframKernel -noprompt -run 'Get["/absolute/path/to/BAE_Codex_runtime/RunSUSYWBE.wl"]'
```

Inside a Wolfram notebook:

```wl
SetDirectory["/absolute/path/to/BAE_Codex_runtime"];
Get["/absolute/path/to/BAE_Codex_runtime/RunSUSYWBE.wl"];
```

## Optional validation

To run the validation case:

```bash
WolframKernel -noprompt -run 'Get["/absolute/path/to/BAE_Codex_runtime/ValidateSYT221.wl"]'
```

To check the Julia project and persistent worker:

```bash
./check_julia_setup.sh
```

## Cluster Migration Validation

After copying the folder to a cluster node, run the full migration check:

```bash
./check_cluster_env.sh
```

If Julia or Wolfram are not on `PATH`, set them explicitly first:

```bash
export JULIA=/full/path/to/julia
export WOLFRAM_KERNEL=/full/path/to/WolframKernel
./check_cluster_env.sh
```

This creates a timestamped directory under `.runs/cluster-validation-*/` with:

- `check_cluster_env.summary.txt`: top-level pass/fail summary
- `path_assumption_scan.txt`: suspicious absolute/local-machine paths still present in code
- `julia/cluster_julia_stages.csv`: Julia environment, instantiate, precompile, and import checks
- `julia/cluster_julia_packages.csv`: per-package import results
- `wolfram/ClusterValidation.stages.csv`: Mathematica-side launcher, worker, and smoke-test results
- per-stage stdout/stderr logs for direct Julia and Wolfram checks

You can also run the two main diagnostics individually:

```bash
julia --project=. julia/cluster_julia_diagnostic.jl --project-root . --output-dir ./.runs/cluster-julia-diagnostic
WolframKernel -noprompt -script ClusterValidation.wl "$(pwd)" "$(pwd)/.runs/cluster-wolfram-diagnostic"
```

The Wolfram smoke test uses a tiny tableau `{{1, 3}, {2}}` so it still exercises the real Mathematica -> Julia continuation handoff while staying lightweight.

Typical failure patterns:

- Julia not found or wrong Julia on the cluster:
  Check `check_cluster_env.summary.txt`, `julia/cluster_julia_environment.txt`, and the `JuliaExecutable` fields in `wolfram/ClusterValidation.summary.wl`.
- Broken or stale Julia environment:
  Look at `julia/cluster_julia_stages.csv`, especially `project_activation`, `instantiate`, `precompile`, and `required_package_imports`.
- Persistent worker/sysimage problems:
  Compare `direct_worker_probe.stdout.log` with the Wolfram `PersistentWorkerProbe` and `SmokeTestPreferredBackend` stages.
- Mathematica can launch Julia trivially but the real continuation fails:
  Inspect the smoke-test run folders in `wolfram/smoke-preferred/` or `wolfram/smoke-process-per-run/`, especially `julia.stderr.log`, `julia.timings.csv`, `workflow.timings.csv`, and `saved_data/output.csv`.
- Folder move/path issues:
  Review `path_assumption_scan.txt`. Active workflow code is relative-path aware, but legacy/sample files may still mention old machine-specific locations.

## Runtime output folders

These are created automatically when needed and do not need to be shipped:

- `.runs/`
- `Result_SYT/`
- `.sysimages/`

## Notes

- If `julia` is not on `PATH`, set `JULIA=/full/path/to/julia` before running the commands above.
- From Wolfram, you can also pass `JuliaExecutable -> "/full/path/to/julia"` to `RunSingleSYT`, `RunAllSYTSerial`, or `RunAllSYTParallel` if the kernel cannot resolve Julia from its environment.
- The sysimage must be built on the target machine. Do not copy a compiled sysimage from a different OS or CPU and expect it to work.
- If an incompatible copied sysimage is present, the runtime now falls back to the plain persistent Julia session automatically.
- `utils/functions.m` no longer hardcodes a local distance-data path; `LoadDistanceData` now uses `SUSYWBE_DISTANCE_DATA_DIR` or a project-local `distance_data/` folder instead.
- This bundle intentionally excludes legacy files and generated output data.

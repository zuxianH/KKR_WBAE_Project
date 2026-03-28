#!/usr/bin/env bash
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMESTAMP="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="${CLUSTER_VALIDATION_RUN_DIR:-$ROOT_DIR/.runs/cluster-validation-$TIMESTAMP}"
JULIA_BIN="${JULIA:-}"
WOLFRAM_BIN="${WOLFRAM_KERNEL:-}"

mkdir -p "$RUN_DIR"

SUMMARY_FILE="$RUN_DIR/check_cluster_env.summary.txt"
COMMANDS_FILE="$RUN_DIR/check_cluster_env.commands.txt"
ENV_FILE="$RUN_DIR/check_cluster_env.environment.txt"
PATH_SCAN_FILE="$RUN_DIR/path_assumption_scan.txt"

FAILURES=0
WARNINGS=0

if [[ -z "$JULIA_BIN" ]]; then
  JULIA_BIN="$(command -v julia || true)"
fi

if [[ -n "$JULIA_BIN" && -x "$(command -v readlink 2>/dev/null || true)" ]]; then
  JULIA_BIN="$(readlink -f "$JULIA_BIN" 2>/dev/null || printf '%s' "$JULIA_BIN")"
fi

if [[ -z "$WOLFRAM_BIN" ]]; then
  WOLFRAM_BIN="$(command -v WolframKernel || true)"
fi

export JULIA="$JULIA_BIN"
export BAE_CLUSTER_PROJECT_ROOT="$ROOT_DIR"
export BAE_CLUSTER_VALIDATION_DIR="$RUN_DIR/wolfram"

{
  echo "timestamp=$TIMESTAMP"
  echo "root_dir=$ROOT_DIR"
  echo "run_dir=$RUN_DIR"
  echo "hostname=$(hostname 2>/dev/null || true)"
  echo "pwd=$(pwd)"
  echo "julia_bin=$JULIA_BIN"
  echo "wolfram_bin=$WOLFRAM_BIN"
  echo "slurm_job_id=${SLURM_JOB_ID:-}"
  echo "slurm_cluster_name=${SLURM_CLUSTER_NAME:-}"
  echo "pbs_jobid=${PBS_JOBID:-}"
  echo "lsb_jobid=${LSB_JOBID:-}"
  echo "julia_project=${JULIA_PROJECT:-}"
  echo "julia_load_path=${JULIA_LOAD_PATH:-}"
  echo "julia_depot_path=${JULIA_DEPOT_PATH:-}"
  echo "path=${PATH:-}"
} > "$ENV_FILE"

{
  echo "Cluster migration validation"
  echo "Root directory: $ROOT_DIR"
  echo "Run directory:  $RUN_DIR"
  echo
} > "$SUMMARY_FILE"

log_summary() {
  printf '%s\n' "$*" | tee -a "$SUMMARY_FILE"
}

log_command() {
  printf '%s\n' "$*" >> "$COMMANDS_FILE"
}

run_stage() {
  local stage_name="$1"
  shift
  local stdout_file="$RUN_DIR/${stage_name}.stdout.log"
  local stderr_file="$RUN_DIR/${stage_name}.stderr.log"
  local started
  local finished
  local elapsed

  started="$(date +%s)"
  log_command "[$stage_name] $*"

  if "$@" >"$stdout_file" 2>"$stderr_file"; then
    finished="$(date +%s)"
    elapsed="$((finished - started))"
    log_summary "[OK]    $stage_name (${elapsed}s)"
    log_summary "        stdout: $stdout_file"
    log_summary "        stderr: $stderr_file"
    return 0
  fi

  finished="$(date +%s)"
  elapsed="$((finished - started))"
  log_summary "[FAIL]  $stage_name (${elapsed}s)"
  log_summary "        stdout: $stdout_file"
  log_summary "        stderr: $stderr_file"
  FAILURES=$((FAILURES + 1))
  return 1
}

log_summary "Executable discovery"
if [[ -n "$JULIA_BIN" ]]; then
  log_summary "  Julia:         $JULIA_BIN"
else
  log_summary "  Julia:         not found"
  FAILURES=$((FAILURES + 1))
fi

if [[ -n "$WOLFRAM_BIN" ]]; then
  log_summary "  WolframKernel: $WOLFRAM_BIN"
else
  log_summary "  WolframKernel: not found"
  FAILURES=$((FAILURES + 1))
fi
log_summary

log_summary "Path scan"
if command -v rg >/dev/null 2>&1; then
  if rg -n "/home/|/Users/|/cfs/|BAE_code_new_new|BAE_Project|Documents/BAE_" \
      --glob '*.wl' \
      --glob '*.m' \
      --glob '*.jl' \
      --glob '*.sh' \
      --glob 'README.md' \
      --glob 'Project.toml' \
      --glob 'Manifest.toml' \
      "$ROOT_DIR" >"$PATH_SCAN_FILE"; then
    log_summary "  Warning: suspicious absolute/local-machine paths were found."
    log_summary "  Review: $PATH_SCAN_FILE"
    WARNINGS=$((WARNINGS + 1))
  else
    log_summary "  No suspicious absolute/local-machine paths found in tracked code files."
  fi
else
  log_summary "  ripgrep not available; skipped path scan."
  WARNINGS=$((WARNINGS + 1))
fi
log_summary

if [[ -n "$JULIA_BIN" ]]; then
  run_stage \
    "cluster_julia_diagnostic" \
    "$JULIA_BIN" \
    --project="$ROOT_DIR" \
    "$ROOT_DIR/julia/cluster_julia_diagnostic.jl" \
    --project-root "$ROOT_DIR" \
    --output-dir "$RUN_DIR/julia"

  WORKER_STDOUT="$RUN_DIR/direct_worker_probe.stdout.log"
  WORKER_STDERR="$RUN_DIR/direct_worker_probe.stderr.log"
  log_command "[direct_worker_probe] printf 'PING\\nQUIT\\n' | $JULIA_BIN --project=$ROOT_DIR $ROOT_DIR/julia/SUSYWBEPersistentWorker.jl"
  if printf 'PING\nQUIT\n' | "$JULIA_BIN" --project="$ROOT_DIR" "$ROOT_DIR/julia/SUSYWBEPersistentWorker.jl" >"$WORKER_STDOUT" 2>"$WORKER_STDERR"; then
    if grep -q $'^READY\t' "$WORKER_STDOUT" && grep -q '^PONG$' "$WORKER_STDOUT" && grep -q '^BYE$' "$WORKER_STDOUT"; then
      log_summary "[OK]    direct_worker_probe"
      log_summary "        stdout: $WORKER_STDOUT"
      log_summary "        stderr: $WORKER_STDERR"
    else
      log_summary "[FAIL]  direct_worker_probe"
      log_summary "        stdout: $WORKER_STDOUT"
      log_summary "        stderr: $WORKER_STDERR"
      FAILURES=$((FAILURES + 1))
    fi
  else
    log_summary "[FAIL]  direct_worker_probe"
    log_summary "        stdout: $WORKER_STDOUT"
    log_summary "        stderr: $WORKER_STDERR"
    FAILURES=$((FAILURES + 1))
  fi
  log_summary
fi

if [[ -n "$WOLFRAM_BIN" ]]; then
  run_stage \
    "cluster_wolfram_validation" \
    "$WOLFRAM_BIN" \
    -noprompt \
    -script "$ROOT_DIR/ClusterValidation.wl" \
    "$ROOT_DIR" \
    "$RUN_DIR/wolfram"
  log_summary
fi

log_summary "Artifacts"
log_summary "  Summary:      $SUMMARY_FILE"
log_summary "  Commands:     $COMMANDS_FILE"
log_summary "  Environment:  $ENV_FILE"
log_summary "  Path scan:    $PATH_SCAN_FILE"
log_summary
log_summary "Totals"
log_summary "  Failures: $FAILURES"
log_summary "  Warnings: $WARNINGS"

if [[ "$FAILURES" -eq 0 ]]; then
  log_summary
  log_summary "Cluster migration validation passed."
  exit 0
fi

log_summary
log_summary "Cluster migration validation failed."
exit 1

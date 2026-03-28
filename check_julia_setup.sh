#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ -n "${JULIA:-}" ]]; then
  JULIA_BIN="$JULIA"
else
  JULIA_BIN="$(command -v julia || true)"
fi

if [[ -z "$JULIA_BIN" ]]; then
  echo "Julia not found. Put julia on PATH or set JULIA=/full/path/to/julia." >&2
  exit 1
fi

echo "Runtime root: $ROOT_DIR"
echo "Julia: $JULIA_BIN"
echo

echo "[1/3] Instantiating Julia project..."
"$JULIA_BIN" --project="$ROOT_DIR" -e 'using Pkg; Pkg.instantiate(); Pkg.precompile()'

echo "[2/3] Warming the Julia worker path..."
"$JULIA_BIN" --project="$ROOT_DIR" "$ROOT_DIR/julia/precompile_susywbe_worker.jl"

echo "[3/3] Probing the persistent worker..."
WORKER_LOG="$ROOT_DIR/julia_setup_probe.log"
printf 'PING\nQUIT\n' | "$JULIA_BIN" --project="$ROOT_DIR" "$ROOT_DIR/julia/SUSYWBEPersistentWorker.jl" > "$WORKER_LOG"

grep -q $'^READY\t' "$WORKER_LOG"
grep -q '^PONG$' "$WORKER_LOG"
grep -q '^BYE$' "$WORKER_LOG"

echo
echo "Julia setup check passed."
echo "Worker probe log: $WORKER_LOG"

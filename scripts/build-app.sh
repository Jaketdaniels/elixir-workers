#!/usr/bin/env bash
#
# Compile the Elixir application + minimal AtomVM stdlib to .beam files
# and package into an .avm archive.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
APP_DIR="${PROJECT_DIR}/elixir-app"
ATOMVM_DIR="${PROJECT_DIR}/vendor/AtomVM"
BUILD_DIR="${PROJECT_DIR}/build/beams"
OUTPUT="${PROJECT_DIR}/build/app.avm"
WORKER_OUTPUT="${PROJECT_DIR}/worker/app.avm"

echo "=== Building Elixir application ==="

# Clean previous beams to avoid stale modules inflating the .avm
rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"

# --- Step 1: Compile AtomVM estdlib (Erlang) ---
# Only modules actually used at runtime. Most erlang:* / lists:* / binary:*
# functions are NIFs/BIFs built into the WASM binary and don't need .beam files.
# maps.beam is needed for maps:to_list/1, maps:put/3, maps:iterator/1.
echo "Compiling AtomVM Erlang stdlib..."
ESTDLIB_DIR="${ATOMVM_DIR}/libs/estdlib/src"
ESTDLIB_MODULES=(
    maps
)
for mod in "${ESTDLIB_MODULES[@]}"; do
    if [ -f "${ESTDLIB_DIR}/${mod}.erl" ]; then
        erlc -o "${BUILD_DIR}" "${ESTDLIB_DIR}/${mod}.erl" 2>/dev/null
    fi
done
echo "  Compiled ${#ESTDLIB_MODULES[@]} Erlang stdlib modules"

# --- Step 2: Compile application modules ---
echo "Compiling Elixir application..."
elixirc -o "${BUILD_DIR}" \
    "${APP_DIR}/lib/atomvm/wasi.ex" \
    "${APP_DIR}/lib/elixir_workers/json.ex" \
    "${APP_DIR}/lib/elixir_workers/router.ex" \
    "${APP_DIR}/lib/elixir_workers.ex"
echo "  Compiled 4 application modules"

# --- Step 3: Package into .avm ---
echo "Packaging .avm archive..."
python3 "${SCRIPT_DIR}/pack_avm.py" "${OUTPUT}" \
    "Elixir.ElixirWorkers.beam" "${BUILD_DIR}"

# Copy to worker directory
cp "${OUTPUT}" "${WORKER_OUTPUT}"
echo "Copied to ${WORKER_OUTPUT}"

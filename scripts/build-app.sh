#!/usr/bin/env bash
#
# Compile the Elixir application + AtomVM stdlib to .beam files
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

mkdir -p "${BUILD_DIR}"

# --- Step 1: Compile AtomVM estdlib (Erlang) ---
echo "Compiling AtomVM Erlang stdlib..."
ESTDLIB_DIR="${ATOMVM_DIR}/libs/estdlib/src"
ESTDLIB_MODULES=(
    init kernel erlang maps lists binary io io_lib proplists unicode
    code code_server application gen gen_server gen_event gen_statem
    supervisor proc_lib sys timer math string os queue sets calendar
    base64 erts_debug logger
)
for mod in "${ESTDLIB_MODULES[@]}"; do
    if [ -f "${ESTDLIB_DIR}/${mod}.erl" ]; then
        erlc -o "${BUILD_DIR}" "${ESTDLIB_DIR}/${mod}.erl" 2>/dev/null
    fi
done
echo "  Compiled ${#ESTDLIB_MODULES[@]} Erlang modules"

# --- Step 2: Compile eavmlib (Erlang) ---
echo "Compiling AtomVM eavmlib..."
EAVMLIB_DIR="${ATOMVM_DIR}/libs/eavmlib/src"
erlc -o "${BUILD_DIR}" "${EAVMLIB_DIR}/atomvm.erl" 2>/dev/null
echo "  Compiled atomvm.erl"

# --- Step 3: Compile AtomVM exavmlib (Elixir stdlib) ---
echo "Compiling AtomVM Elixir stdlib..."
EXAVMLIB_DIR="${ATOMVM_DIR}/libs/exavmlib/lib"
elixirc --no-docs --ignore-module-conflict -o "${BUILD_DIR}" \
    "${EXAVMLIB_DIR}/Map.ex" 2>/dev/null
echo "  Compiled Map.ex"

# --- Step 4: Compile application modules ---
echo "Compiling Elixir application..."
elixirc -o "${BUILD_DIR}" \
    "${APP_DIR}/lib/atomvm/wasi.ex" \
    "${APP_DIR}/lib/elixir_workers/json.ex" \
    "${APP_DIR}/lib/elixir_workers/router.ex" \
    "${APP_DIR}/lib/elixir_workers.ex"
echo "  Compiled 4 application modules"

# --- Step 5: Package into .avm ---
echo "Packaging .avm archive..."
python3 "${SCRIPT_DIR}/pack_avm.py" "${OUTPUT}" \
    "Elixir.ElixirWorkers.beam" "${BUILD_DIR}"

# Copy to worker directory
cp "${OUTPUT}" "${WORKER_OUTPUT}"
echo "Copied to ${WORKER_OUTPUT}"

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

ESTDLIB_DIR="${ATOMVM_DIR}/libs/estdlib/src"
EXAVMLIB_DIR="${ATOMVM_DIR}/libs/exavmlib/lib"

echo "=== Building Elixir application ==="

rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"

# --- Step 1: Compile AtomVM estdlib (Erlang) ---
# Networking/dist/ets/crypto excluded — no WASI support.
echo "Compiling Erlang stdlib..."
ESTDLIB_MODULES=(
    gen gen_server gen_statem gen_event supervisor proc_lib sys
    maps lists proplists queue sets
    io io_lib string unicode
    timer math base64 calendar
)
for mod in "${ESTDLIB_MODULES[@]}"; do
    if [ -f "${ESTDLIB_DIR}/${mod}.erl" ]; then
        erlc -o "${BUILD_DIR}" "${ESTDLIB_DIR}/${mod}.erl" 2>/dev/null || true
    fi
done
echo "  Compiled Erlang stdlib"

# --- Step 2: Compile AtomVM exavmlib (Elixir stdlib) ---
# Exclude hardware-specific modules (GPIO, I2C, LEDC, AVMPort).
echo "Compiling Elixir stdlib..."
EXAVMLIB_FILES=()
while IFS= read -r f; do
    base="$(basename "$f" .ex)"
    case "$base" in
        GPIO|I2C|LEDC|AVMPort|Console) continue ;;
        *) EXAVMLIB_FILES+=("$f") ;;
    esac
done < <(find "${EXAVMLIB_DIR}" -name '*.ex' | sort)

elixirc -o "${BUILD_DIR}" "${EXAVMLIB_FILES[@]}"
echo "  Compiled Elixir stdlib"

# --- Step 3: Compile application modules ---
echo "Compiling application..."
elixirc -o "${BUILD_DIR}" \
    "${APP_DIR}/lib/atomvm/wasi.ex" \
    "${APP_DIR}/lib/elixir_workers/json.ex" \
    "${APP_DIR}/lib/elixir_workers/router.ex" \
    "${APP_DIR}/lib/elixir_workers.ex"
echo "  Compiled application"

# --- Step 4: Package into .avm ---
echo "Packaging .avm archive..."
python3 "${SCRIPT_DIR}/pack_avm.py" "${OUTPUT}" \
    "Elixir.ElixirWorkers.beam" "${BUILD_DIR}"

cp "${OUTPUT}" "${WORKER_OUTPUT}"
echo ""
ls -lh "${WORKER_OUTPUT}"

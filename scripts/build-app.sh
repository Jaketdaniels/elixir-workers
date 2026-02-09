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
echo "Compiling Erlang stdlib..."
ESTDLIB_MODULES=(
    gen gen_server gen_statem gen_event supervisor proc_lib sys
    maps lists proplists queue sets
    io io_lib string unicode
    timer math base64 calendar
)
ESTDLIB_ERRORS=0
for mod in "${ESTDLIB_MODULES[@]}"; do
    if [ -f "${ESTDLIB_DIR}/${mod}.erl" ]; then
        if ! erlc -o "${BUILD_DIR}" "${ESTDLIB_DIR}/${mod}.erl" 2>&1; then
            echo "  Warning: failed to compile ${mod}.erl (may be optional)"
            ESTDLIB_ERRORS=$(( ESTDLIB_ERRORS + 1 ))
        fi
    fi
done
if [ "$ESTDLIB_ERRORS" -gt 0 ]; then
    echo "  Warning: ${ESTDLIB_ERRORS} stdlib module(s) failed to compile"
fi
echo "  Compiled Erlang stdlib"

# --- Step 2: Compile AtomVM exavmlib (Elixir stdlib) ---
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
APP_FILES=()
while IFS= read -r f; do
    APP_FILES+=("$f")
done < <(find "${APP_DIR}/lib" -name '*.ex' | sort)

elixirc -o "${BUILD_DIR}" "${APP_FILES[@]}"
echo "  Compiled ${#APP_FILES[@]} app modules"

# --- Step 4: Detect startup module ---
STARTUP=$(grep -rl 'def start' "${APP_DIR}/lib" --include='*.ex' | head -1)
if [ -z "$STARTUP" ]; then
    echo "Error: No module with start/0 found in elixir-app/lib/"
    exit 1
fi
STARTUP_MOD=$(grep -m1 'defmodule' "$STARTUP" | sed 's/.*defmodule \([^ ]*\).*/\1/' | tr '.' '.')
STARTUP_BEAM="Elixir.${STARTUP_MOD}.beam"
echo "  Startup module: ${STARTUP_MOD}"

# --- Step 5: Package into .avm ---
echo "Packaging .avm archive..."
python3 "${SCRIPT_DIR}/pack_avm.py" "${OUTPUT}" \
    "${STARTUP_BEAM}" "${BUILD_DIR}"

cp "${OUTPUT}" "${WORKER_OUTPUT}"
echo ""
ls -lh "${WORKER_OUTPUT}"

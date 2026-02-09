#!/usr/bin/env bash
#
# Build AtomVM for WASI target using wasi-sdk.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Ensure erl is in PATH (Homebrew doesn't always symlink it)
for erldir in /opt/homebrew/opt/erlang/bin /usr/local/opt/erlang/bin; do
  if [[ -x "${erldir}/erl" ]] && ! echo "$PATH" | grep -q "$erldir"; then
    export PATH="${erldir}:${PATH}"
  fi
done
BUILD_DIR="${PROJECT_DIR}/build/atomvm-wasi"
ATOMVM_WASI_DIR="${PROJECT_DIR}/atomvm-wasi"

mkdir -p "${PROJECT_DIR}/build"
BUILD_LOG="${PROJECT_DIR}/build/atomvm-build.log"
: >"$BUILD_LOG"
BUILD_START="$(date +%s)"

# ─── Terminal ────────────────────────────────────────────────────────────────

IS_TTY=false
if [[ -t 1 ]] && command -v tput >/dev/null 2>&1; then
  IS_TTY=true
  bold="$(tput bold)"       || bold=""
  dim="$(tput dim)"         || dim=""
  reset="$(tput sgr0)"      || reset=""
  red="$(tput setaf 1)"     || red=""
  green="$(tput setaf 2)"   || green=""
  magenta="$(tput setaf 5)" || magenta=""
else
  bold="" dim="" reset="" red="" green="" magenta=""
fi

CLEAR_EOL=$'\033[K'
TOTAL_STEPS=5
CURRENT_STEP=0
STEP_LABEL=""

hide_cursor() { if $IS_TTY; then tput civis 2>/dev/null || true; fi; }
show_cursor() { if $IS_TTY; then tput cnorm 2>/dev/null || true; fi; }
trap show_cursor EXIT INT TERM HUP

step_begin() {
  CURRENT_STEP=$(( CURRENT_STEP + 1 ))
  STEP_LABEL="$1"
  if $IS_TTY; then
    printf " %s[%d/%d]%s %-22s %s…%s" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$dim" "$reset"
  fi
}

step_ok() {
  if $IS_TTY; then
    printf "\r%s %s[%d/%d]%s %-22s %s✓%s %s\n" \
      "$CLEAR_EOL" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$green" "$reset" "$1"
  else
    printf " [%d/%d] %-22s ✓ %s\n" \
      "$CURRENT_STEP" "$TOTAL_STEPS" "$STEP_LABEL" "$1"
  fi
}

step_fail() {
  if $IS_TTY; then
    printf "\r%s %s[%d/%d]%s %-22s %s✗%s %s\n" \
      "$CLEAR_EOL" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$red" "$reset" "$1" >&2
  else
    printf " [%d/%d] %-22s ✗ %s\n" \
      "$CURRENT_STEP" "$TOTAL_STEPS" "$STEP_LABEL" "$1" >&2
  fi
}

die() {
  step_fail "$1"
  if [[ -f "$BUILD_LOG" ]]; then
    if $IS_TTY; then printf " %s" "$dim" >&2; fi
    tail -15 "$BUILD_LOG" | while IFS= read -r line; do
      printf " %s\n" "$line" >&2
    done
    if $IS_TTY; then printf "%s" "$reset" >&2; fi
  fi
  printf " Log: %s\n" "$BUILD_LOG" >&2
  exit 1
}

spin() {
  local desc="$1"; shift
  local spin_chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
  local i=0 rc=0

  echo "=== $desc ===" >>"$BUILD_LOG"
  hide_cursor
  "$@" >>"$BUILD_LOG" 2>&1 &
  local pid=$!

  if $IS_TTY; then
    while kill -0 "$pid" 2>/dev/null; do
      printf "\r%s %s[%d/%d]%s %-22s %s%s%s %s" \
        "$CLEAR_EOL" \
        "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
        "$STEP_LABEL" \
        "$magenta" "${spin_chars:i%${#spin_chars}:1}" "$reset" "$desc"
      i=$(( i + 1 ))
      sleep 0.08
    done
  fi

  wait "$pid" || rc=$?
  show_cursor
  return "$rc"
}

# Like spin() but parses [XX%] progress from cmake output in the log file
spin_progress() {
  local desc="$1"; shift
  local spin_chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
  local i=0 rc=0 pct=""
  local log_pos

  echo "=== $desc ===" >>"$BUILD_LOG"
  log_pos=$(wc -c < "$BUILD_LOG" | tr -d ' ')
  hide_cursor
  "$@" >>"$BUILD_LOG" 2>&1 &
  local pid=$!

  if $IS_TTY; then
    while kill -0 "$pid" 2>/dev/null; do
      # Parse latest [XX%] from new log output
      pct=$(tail -c +"$log_pos" "$BUILD_LOG" 2>/dev/null \
        | grep -oE '\[ *[0-9]+%\]' | tail -1 \
        | grep -oE '[0-9]+' || true)
      local status="$desc"
      if [[ -n "$pct" ]]; then
        status="${desc} ${pct}%"
      fi
      printf "\r%s %s[%d/%d]%s %-22s %s%s%s %s" \
        "$CLEAR_EOL" \
        "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
        "$STEP_LABEL" \
        "$magenta" "${spin_chars:i%${#spin_chars}:1}" "$reset" "$status"
      i=$(( i + 1 ))
      sleep 0.15
    done
  fi

  wait "$pid" || rc=$?
  show_cursor
  return "$rc"
}

elapsed() { echo "$(( "$(date +%s)" - "$1" ))s"; }

# ─── 1. Detect wasi-sdk ────────────────────────────────────────────────────

step_begin "wasi-sdk"

WASI_SDK=""
for dir in "${WASI_SDK_PATH:-}" /opt/wasi-sdk "$HOME/.wasi-sdk" /usr/local/wasi-sdk; do
  if [[ -n "$dir" && -x "${dir}/bin/clang" ]]; then
    WASI_SDK="$dir"
    break
  fi
done
[[ -n "$WASI_SDK" ]] || die "not found — run 'make setup'"

step_ok "$WASI_SDK"

# ─── 2. Check AtomVM source ───────────────────────────────────────────────

step_begin "AtomVM source"

[[ -f "${PROJECT_DIR}/vendor/AtomVM/src/libAtomVM/globalcontext.c" ]] || \
  die "not found — run 'make setup'"

step_ok "vendor/AtomVM"

# ─── 3. CMake configure ───────────────────────────────────────────────────

step_begin "Configure"

mkdir -p "${BUILD_DIR}"

spin "Configuring cmake" cmake -S "${ATOMVM_WASI_DIR}" -B "${BUILD_DIR}" \
  -DCMAKE_TOOLCHAIN_FILE="${WASI_SDK}/share/cmake/wasi-sdk.cmake" \
  -DWASI_SDK_PREFIX="${WASI_SDK}" \
  -DCMAKE_SYSROOT="${WASI_SDK}/share/wasi-sysroot" \
  -DCMAKE_C_COMPILER="${WASI_SDK}/bin/clang" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_FLAGS="--sysroot=${WASI_SDK}/share/wasi-sysroot -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS" \
  -DCMAKE_EXE_LINKER_FLAGS="-lwasi-emulated-signal -lwasi-emulated-process-clocks" \
  -DATOMVM_SOURCE_DIR="${PROJECT_DIR}/vendor/AtomVM" \
  || die "cmake configure failed"

step_ok "done"

# ─── 4. Build ─────────────────────────────────────────────────────────────

step_begin "Compile"

spin_progress "Compiling WASM" cmake --build "${BUILD_DIR}" --parallel \
  || die "cmake build failed"

WASM_OUT="${BUILD_DIR}/atomvm.wasm"
[[ -f "$WASM_OUT" ]] || die "atomvm.wasm not produced"

RAW_SIZE="$(ls -lh "$WASM_OUT" | awk '{print $5}')"
step_ok "$RAW_SIZE"

# ─── 5. Optimize ──────────────────────────────────────────────────────────

step_begin "Optimize"

# Strip debug symbols
if command -v wasm-strip >/dev/null 2>&1; then
  wasm-strip "$WASM_OUT" >>"$BUILD_LOG" 2>&1
elif [[ -x "${WASI_SDK}/bin/llvm-strip" ]]; then
  "${WASI_SDK}/bin/llvm-strip" "$WASM_OUT" >>"$BUILD_LOG" 2>&1
fi

# Optimize with wasm-opt
if command -v wasm-opt >/dev/null 2>&1; then
  spin "Running wasm-opt" wasm-opt -Oz -c --low-memory-unused --zero-filled-memory \
    --strip-debug --strip-producers \
    -o "${WASM_OUT}.opt" "$WASM_OUT" \
    || die "wasm-opt failed"
  mv "${WASM_OUT}.opt" "$WASM_OUT"
fi

cp "$WASM_OUT" "${PROJECT_DIR}/worker/atomvm.wasm"
FINAL_SIZE="$(ls -lh "${PROJECT_DIR}/worker/atomvm.wasm" | awk '{print $5}')"

step_ok "$FINAL_SIZE → worker/atomvm.wasm"

# ─── Done ──────────────────────────────────────────────────────────────────

TOTAL_ELAPSED="$(elapsed "$BUILD_START")"
echo ""
printf " %s%s✓ AtomVM build complete%s %s(%s)%s\n" "$green" "$bold" "$reset" "$dim" "$TOTAL_ELAPSED" "$reset"
echo ""
rm -f "$BUILD_LOG"

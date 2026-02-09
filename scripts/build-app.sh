#!/usr/bin/env bash
#
# Compile the Elixir application + AtomVM stdlib to .beam files
# and package into an .avm archive.
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
APP_DIR="${PROJECT_DIR}/_build/starter"
FRAMEWORK_DIR="${PROJECT_DIR}/packages/elixir_workers/lib"
ATOMVM_DIR="${PROJECT_DIR}/vendor/AtomVM"
BUILD_DIR="${PROJECT_DIR}/build/beams"
OUTPUT="${PROJECT_DIR}/build/app.avm"
WORKER_OUTPUT="${PROJECT_DIR}/worker/app.avm"

ESTDLIB_DIR="${ATOMVM_DIR}/libs/estdlib/src"
EXAVMLIB_DIR="${ATOMVM_DIR}/libs/exavmlib/lib"

mkdir -p "${PROJECT_DIR}/build"
BUILD_LOG="${PROJECT_DIR}/build/app-build.log"
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

elapsed() { echo "$(( "$(date +%s)" - "$1" ))s"; }

# ─── 1. Erlang stdlib ────────────────────────────────────────────────────

step_begin "Erlang stdlib"

rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"

ESTDLIB_MODULES=(
  gen gen_server gen_statem gen_event supervisor proc_lib sys
  maps lists proplists queue sets
  io io_lib string unicode
  timer math base64 calendar
)

ESTDLIB_OK=0
ESTDLIB_ERRORS=0
for mod in "${ESTDLIB_MODULES[@]}"; do
  if [[ -f "${ESTDLIB_DIR}/${mod}.erl" ]]; then
    if erlc -o "${BUILD_DIR}" "${ESTDLIB_DIR}/${mod}.erl" >>"$BUILD_LOG" 2>&1; then
      ESTDLIB_OK=$(( ESTDLIB_OK + 1 ))
    else
      ESTDLIB_ERRORS=$(( ESTDLIB_ERRORS + 1 ))
    fi
  fi
done

if [[ "$ESTDLIB_ERRORS" -gt 0 ]]; then
  step_ok "${ESTDLIB_OK} modules (${ESTDLIB_ERRORS} skipped)"
else
  step_ok "${ESTDLIB_OK} modules"
fi

# ─── 2. Elixir stdlib ───────────────────────────────────────────────────

step_begin "Elixir stdlib"

EXAVMLIB_FILES=()
while IFS= read -r f; do
  base="$(basename "$f" .ex)"
  case "$base" in
    GPIO|I2C|LEDC|AVMPort|Console) continue ;;
    *) EXAVMLIB_FILES+=("$f") ;;
  esac
done < <(find "${EXAVMLIB_DIR}" -name '*.ex' | sort)

spin "Compiling Elixir stdlib" elixirc -o "${BUILD_DIR}" "${EXAVMLIB_FILES[@]}" \
  || die "elixirc failed"

step_ok "${#EXAVMLIB_FILES[@]} modules"

# ─── 3. Framework + application ─────────────────────────────────────────

step_begin "Framework + app"

SRC_FILES=()
while IFS= read -r f; do
  SRC_FILES+=("$f")
done < <(find "${FRAMEWORK_DIR}" "${APP_DIR}/lib" -name '*.ex' | sort)

spin "Compiling application" elixirc -o "${BUILD_DIR}" "${SRC_FILES[@]}" \
  || die "elixirc failed"

step_ok "${#SRC_FILES[@]} modules"

# ─── 4. Detect startup module ───────────────────────────────────────────

step_begin "Startup module"

STARTUP=$(grep -rl 'use ElixirWorkers.App' "${APP_DIR}/lib" --include='*.ex' | head -1 || true)
[[ -n "$STARTUP" ]] || die "no module with 'use ElixirWorkers.App' found in ${APP_DIR}/lib/"

STARTUP_MOD=$(grep -m1 'defmodule' "$STARTUP" | sed 's/.*defmodule \([^ ]*\).*/\1/' | tr '.' '.')
STARTUP_BEAM="Elixir.${STARTUP_MOD}.beam"

step_ok "$STARTUP_MOD"

# ─── 5. Package .avm ────────────────────────────────────────────────────

step_begin "Package .avm"

spin "Packing archive" python3 "${SCRIPT_DIR}/pack_avm.py" "${OUTPUT}" \
  "${STARTUP_BEAM}" "${BUILD_DIR}" \
  || die "pack_avm.py failed"

cp "${OUTPUT}" "${WORKER_OUTPUT}"
AVM_SIZE="$(ls -lh "${WORKER_OUTPUT}" | awk '{print $5}')"

step_ok "${AVM_SIZE} → worker/app.avm"

# ─── Done ────────────────────────────────────────────────────────────────

TOTAL_ELAPSED="$(elapsed "$BUILD_START")"
echo ""
printf " %s%s✓ App build complete%s %s(%s)%s\n" "$green" "$bold" "$reset" "$dim" "$TOTAL_ELAPSED" "$reset"
echo ""
rm -f "$BUILD_LOG"

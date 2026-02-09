#!/usr/bin/env bash
# Setup script for elixir-workers development.
# Installs all dependencies, resolves PATH issues, and verifies the
# full toolchain works end-to-end. Safe to re-run (idempotent).
#
# Handles: stale symlinks, partial installs, version conflicts,
# network failures, missing tools, and non-tty (CI) environments.
#
# Usage:
#   make setup                  # interactive
#   make setup NONINTERACTIVE=1 # auto-yes (CI)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

WASI_SDK_VERSION=25
SETUP_LOG="${PROJECT_DIR}/.setup.log"
SETUP_START="$(date +%s)"

MIN_CMAKE=3
MIN_ELIXIR=1
MIN_OTP=26
MIN_NODE=18

TOTAL_STEPS=11
CURRENT_STEP=0
STEP_LABEL=""
STEP_AUTO=false

export HOMEBREW_NO_ENV_HINTS=1

# ─── Terminal ────────────────────────────────────────────────────────────────

IS_TTY=false
if [[ -t 1 ]] && command -v tput >/dev/null 2>&1; then
  IS_TTY=true
  bold="$(tput bold)"       || bold=""
  dim="$(tput dim)"         || dim=""
  reset="$(tput sgr0)"      || reset=""
  red="$(tput setaf 1)"     || red=""
  green="$(tput setaf 2)"   || green=""
  yellow="$(tput setaf 3)"  || yellow=""
  cyan="$(tput setaf 6)"    || cyan=""
  magenta="$(tput setaf 5)" || magenta=""
else
  bold="" dim="" reset="" red="" green="" yellow="" cyan="" magenta=""
fi

CLEAR_EOL=$'\033[K'

hide_cursor() {
  if $IS_TTY && command -v tput >/dev/null 2>&1; then
    tput civis || true
  fi
}
show_cursor() {
  if $IS_TTY && command -v tput >/dev/null 2>&1; then
    tput cnorm || true
  fi
}

# ─── Single-line step UI ─────────────────────────────────────────────────────

# Each step occupies exactly one terminal line. Spinners, prompts, and status
# updates replace the line in place. Only step_ok/step_fail emit a newline.

step_begin() {
  CURRENT_STEP=$(( CURRENT_STEP + 1 ))
  STEP_LABEL="$1"

  if $IS_TTY; then
    printf " %s[%d/%d]%s %-22s %s…%s" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$dim" "$reset"
  fi
}

step_status() {
  if $IS_TTY; then
    printf "\r%s %s[%d/%d]%s %-22s %s%s%s" \
      "$CLEAR_EOL" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$dim" "$1" "$reset"
  fi
}

step_ok() {
  local detail="$1"
  if $STEP_AUTO; then
    detail="$detail (auto)"
    STEP_AUTO=false
  fi
  if $IS_TTY; then
    printf "\r%s %s[%d/%d]%s %-22s %s✓%s %s\n" \
      "$CLEAR_EOL" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$green" "$reset" "$detail"
  else
    printf " [%d/%d] %-22s ✓ %s\n" \
      "$CURRENT_STEP" "$TOTAL_STEPS" "$STEP_LABEL" "$detail"
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

step_skip() {
  CURRENT_STEP=$(( CURRENT_STEP + 1 ))
}

# ─── Helpers ─────────────────────────────────────────────────────────────────

die() {
  step_fail "$1"
  if [[ -f "$SETUP_LOG" ]]; then
    if $IS_TTY; then printf " %s" "$dim" >&2; fi
    tail -15 "$SETUP_LOG" | while IFS= read -r line; do
      printf " %s\n" "$line" >&2
    done
    if $IS_TTY; then printf "%s" "$reset" >&2; fi
  fi
  printf " Log: %s\n" "$SETUP_LOG" >&2
  exit 1
}

elapsed() {
  echo "$(( "$(date +%s)" - "$1" ))s"
}

major_version() {
  echo "$1" | sed 's/^[vV]//' | cut -d. -f1
}

confirm() {
  local msg="$1"

  if [[ "${NONINTERACTIVE:-0}" == "1" ]] || [[ ! -t 0 ]]; then
    STEP_AUTO=true
    return 0
  fi

  if $IS_TTY; then
    printf "\r%s %s[%d/%d]%s %-22s %s %s[Y/n]%s " \
      "$CLEAR_EOL" \
      "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
      "$STEP_LABEL" "$msg" "$dim" "$reset"
  else
    printf " %s [Y/n] " "$msg"
  fi

  read -r answer
  if $IS_TTY; then printf "\033[A"; fi

  case "$answer" in
    [nN]*) return 1 ;;
    *)     return 0 ;;
  esac
}

# ─── Collected tool versions (for summary table) ─────────────────────────────

declare -a TOOL_NAMES=()
declare -a TOOL_VERSIONS=()

record_tool() {
  TOOL_NAMES+=("$1")
  TOOL_VERSIONS+=("$2")
}

# ─── Spinner wrapper ─────────────────────────────────────────────────────────

spin() {
  local desc="$1"; shift
  local spin_chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
  local i=0 rc=0

  echo "=== $desc ===" >>"$SETUP_LOG"

  hide_cursor
  "$@" >>"$SETUP_LOG" 2>&1 &
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

# ─── Brew install with recovery ──────────────────────────────────────────────

brew_install() {
  local formula="$1" cmd="$2"
  local spin_chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
  local i=0

  echo "=== brew install $formula ===" >>"$SETUP_LOG"

  hide_cursor
  brew install "$formula" >>"$SETUP_LOG" 2>&1 &
  local pid=$!

  if $IS_TTY; then
    while kill -0 "$pid" 2>/dev/null; do
      printf "\r%s %s[%d/%d]%s %-22s %s%s%s Installing %s…" \
        "$CLEAR_EOL" \
        "${bold}${magenta}" "$CURRENT_STEP" "$TOTAL_STEPS" "$reset" \
        "$STEP_LABEL" \
        "$magenta" "${spin_chars:i%${#spin_chars}:1}" "$reset" "$formula"
      i=$(( i + 1 ))
      sleep 0.08
    done
  fi

  local brew_rc=0
  wait "$pid" || brew_rc=$?
  show_cursor

  if command -v "$cmd" >/dev/null 2>&1; then
    return 0
  fi

  step_status "Fixing symlinks…"
  brew link --overwrite "$formula" >>"$SETUP_LOG" 2>&1 || true
  if command -v "$cmd" >/dev/null 2>&1; then
    return 0
  fi

  step_status "Reinstalling…"
  brew reinstall "$formula" >>"$SETUP_LOG" 2>&1 || true
  brew link --overwrite "$formula" >>"$SETUP_LOG" 2>&1 || true

  if command -v "$cmd" >/dev/null 2>&1; then
    return 0
  fi

  die "Could not install $formula"
}

# ─── Download with retry ─────────────────────────────────────────────────────

download() {
  local url="$1" output="$2" desc="${3:-Downloading}"
  local spin_chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
  local i=0 attempt=1 max_attempts=3

  while (( attempt <= max_attempts )); do
    echo "=== curl $url ===" >>"$SETUP_LOG"
    hide_cursor
    curl -fSL -o "$output" "$url" >>"$SETUP_LOG" 2>&1 &
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

    local rc=0
    wait "$pid" || rc=$?
    show_cursor

    if [[ "$rc" -eq 0 && -s "$output" ]]; then
      return 0
    fi

    if (( attempt < max_attempts )); then
      step_status "Retry $attempt/$max_attempts…"
      sleep 3
    fi
    attempt=$(( attempt + 1 ))
  done

  die "Failed to download $desc"
}

# ─── Cleanup ─────────────────────────────────────────────────────────────────

cleanup() {
  show_cursor

  if [[ -n "${WORK_DIR:-}" && -d "${WORK_DIR:-}" ]]; then
    rm -rf "$WORK_DIR"
  fi

  if [[ "${WASI_INSTALLING:-}" == "1" && -d "$HOME/.wasi-sdk" ]]; then
    rm -rf "$HOME/.wasi-sdk"
    printf " %sCleaned up partial wasi-sdk install%s\n" "$red" "$reset" >&2
  fi
}

trap cleanup EXIT INT TERM HUP

# ═════════════════════════════════════════════════════════════════════════════

: >"$SETUP_LOG"

OS="$(uname -s)"
ARCH="$(uname -m)"

echo ""
printf " %s%s    ▄█▄%s\n" "$magenta" "$bold" "$reset"
printf " %s%s   █████%s  elixir-workers %ssetup%s\n" "$magenta" "$bold" "$reset" "$dim" "$reset"
printf " %s%s    ███%s   %s%s %s%s\n" "$magenta" "$bold" "$reset" "$dim" "$OS" "$ARCH" "$reset"
printf " %s%s     ▀%s\n" "$magenta" "$bold" "$reset"
echo ""

# ─── 1. Homebrew ─────────────────────────────────────────────────────────────

if [[ "$OS" == "Darwin" ]]; then
  step_begin "Homebrew"
  command -v brew >/dev/null 2>&1 || die "Required — https://brew.sh"
  BREW_V="$(brew --version | head -1)"
  step_ok "$BREW_V"
  record_tool "Homebrew" "$BREW_V"
elif [[ "$OS" == "Linux" ]]; then
  step_skip
else
  step_begin "Platform"
  die "Unsupported OS: $OS"
fi

# ─── 2. cmake ────────────────────────────────────────────────────────────────

step_begin "cmake"
if command -v cmake >/dev/null 2>&1; then
  CMAKE_V="$(cmake --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1 || true)"
  [[ -n "$CMAKE_V" ]] || die "Found but not working"
  [[ "$(major_version "$CMAKE_V")" -ge "$MIN_CMAKE" ]] || die "cmake $CMAKE_V, need >= $MIN_CMAKE"
  step_ok "$CMAKE_V"
  record_tool "cmake" "$CMAKE_V"
elif [[ "$OS" == "Darwin" ]]; then
  confirm "Install cmake via Homebrew?" || die "cmake is required"
  brew_install cmake cmake
  CMAKE_V="$(cmake --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1 || true)"
  step_ok "${CMAKE_V:-installed}"
  record_tool "cmake" "${CMAKE_V:-installed}"
else
  die "Required — install cmake and re-run"
fi

# ─── 3. Erlang & Elixir ─────────────────────────────────────────────────────

step_begin "Erlang & Elixir"

if ! command -v elixir >/dev/null 2>&1; then
  if [[ "$OS" == "Darwin" ]]; then
    confirm "Install elixir via Homebrew?" || die "elixir is required"
    brew_install elixir elixir
  else
    die "Required — install erlang + elixir and re-run"
  fi
fi

EXTRA_PATH=""
for erldir in /opt/homebrew/opt/erlang/bin /usr/local/opt/erlang/bin; do
  if [[ -x "${erldir}/erl" ]] && ! echo "$PATH" | grep -q "$erldir"; then
    export PATH="${erldir}:${PATH}"
    EXTRA_PATH="${erldir}"
  fi
done

ERL_V="$(erl -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().' -noshell 2>/dev/null || true)"
[[ -n "$ERL_V" ]] || die "erl not found in PATH — if using asdf/mise/kerl, activate it first"
[[ "$(major_version "$ERL_V")" -ge "$MIN_OTP" ]] || die "OTP $ERL_V, need >= $MIN_OTP"

ELIXIR_V="$(elixir --version 2>/dev/null | grep -oE 'Elixir [0-9]+\.[0-9]+' | grep -oE '[0-9]+\.[0-9]+' || true)"
[[ -n "$ELIXIR_V" ]] || die "elixir not working"
[[ "$(major_version "$ELIXIR_V")" -ge "$MIN_ELIXIR" ]] || die "Elixir $ELIXIR_V, need >= $MIN_ELIXIR"

mix --version >/dev/null 2>&1 || die "mix is not working"

step_ok "OTP $ERL_V, Elixir $ELIXIR_V"
record_tool "Erlang/OTP" "$ERL_V"
record_tool "Elixir" "$ELIXIR_V"

# ─── 4. Node.js ─────────────────────────────────────────────────────────────

step_begin "Node.js"

if command -v node >/dev/null 2>&1; then
  NODE_V="$(node --version 2>/dev/null || true)"
  [[ -n "$NODE_V" ]] || die "Found but not working"
  [[ "$(major_version "$NODE_V")" -ge "$MIN_NODE" ]] || die "Node.js $NODE_V, need >= $MIN_NODE"
  step_ok "$NODE_V"
  record_tool "Node.js" "$NODE_V"
elif [[ "$OS" == "Darwin" ]]; then
  confirm "Install node via Homebrew?" || die "node is required"
  brew_install node node
  NODE_V="$(node --version 2>/dev/null || true)"
  step_ok "${NODE_V:-installed}"
  record_tool "Node.js" "${NODE_V:-installed}"
else
  die "Required — install node and re-run"
fi

# ─── 5. Python 3 ────────────────────────────────────────────────────────────

step_begin "Python 3"

PYTHON_V="$(python3 --version 2>/dev/null | grep -oE '[0-9]+\.[0-9]+' || true)"
[[ -n "$PYTHON_V" ]] || die "python3 is required but not found"

step_ok "$PYTHON_V"
record_tool "Python" "$PYTHON_V"

# ─── 6. Binaryen ────────────────────────────────────────────────────────────

step_begin "Binaryen"

if command -v wasm-opt >/dev/null 2>&1; then
  WASMOPT_V="$(wasm-opt --version 2>/dev/null | grep -oE '[0-9]+' | head -1 || true)"
  [[ -n "$WASMOPT_V" ]] || die "Found but not working"
  step_ok "wasm-opt $WASMOPT_V"
  record_tool "Binaryen" "$WASMOPT_V"
elif [[ "$OS" == "Darwin" ]]; then
  confirm "Install binaryen via Homebrew?" || die "binaryen is required"
  brew_install binaryen wasm-opt
  WASMOPT_V="$(wasm-opt --version 2>/dev/null | grep -oE '[0-9]+' | head -1 || true)"
  step_ok "wasm-opt ${WASMOPT_V:-installed}"
  record_tool "Binaryen" "${WASMOPT_V:-installed}"
else
  die "Required — install binaryen and re-run"
fi

# ─── 7. wasi-sdk ────────────────────────────────────────────────────────────

step_begin "wasi-sdk"

if [[ -d "$HOME/.wasi-sdk" && -x "$HOME/.wasi-sdk/bin/clang" ]]; then
  WASI_V="$("$HOME/.wasi-sdk/bin/clang" --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1 || echo "unknown")"
  step_ok "v${WASI_SDK_VERSION} (clang $WASI_V)"
  record_tool "wasi-sdk" "v${WASI_SDK_VERSION}"
else
  [[ -d "$HOME/.wasi-sdk" ]] && rm -rf "$HOME/.wasi-sdk"

  case "${OS}-${ARCH}" in
    Darwin-arm64)  WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-arm64-macos.tar.gz" ;;
    Darwin-x86_64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-x86_64-macos.tar.gz" ;;
    Linux-x86_64)  WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-x86_64-linux.tar.gz" ;;
    Linux-aarch64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-arm64-linux.tar.gz" ;;
    *)             die "No wasi-sdk binary for ${OS}-${ARCH}" ;;
  esac

  confirm "Download wasi-sdk v${WASI_SDK_VERSION} (~400 MB)?" || \
    die "Required — https://github.com/WebAssembly/wasi-sdk/releases"

  WASI_URL="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_SDK_VERSION}/${WASI_ARCHIVE}"
  WORK_DIR="$(mktemp -d)"
  WASI_INSTALLING=1
  local_start="$(date +%s)"

  download "$WASI_URL" "${WORK_DIR}/${WASI_ARCHIVE}" "Downloading wasi-sdk v${WASI_SDK_VERSION}"
  spin "Extracting wasi-sdk" tar xf "${WORK_DIR}/${WASI_ARCHIVE}" -C "$WORK_DIR" || die "Failed to extract wasi-sdk"
  rm -f "${WORK_DIR}/${WASI_ARCHIVE}"

  mv "${WORK_DIR}"/wasi-sdk-* "$HOME/.wasi-sdk"
  WASI_INSTALLING=0
  rm -rf "$WORK_DIR"
  unset WORK_DIR

  if [[ -x "$HOME/.wasi-sdk/bin/clang" ]]; then
    "$HOME/.wasi-sdk/bin/clang" --version >/dev/null 2>&1 || die "Installed but clang won't execute"
    step_ok "v${WASI_SDK_VERSION} ($(elapsed "$local_start"))"
    record_tool "wasi-sdk" "v${WASI_SDK_VERSION}"
  else
    die "Install failed — ~/.wasi-sdk/bin/clang not found"
  fi
fi

# ─── 8. AtomVM submodule ────────────────────────────────────────────────────

step_begin "AtomVM submodule"

if [[ -d "${PROJECT_DIR}/vendor/AtomVM/src/libAtomVM" ]]; then
  step_ok "already initialized"
  record_tool "AtomVM" "submodule"
else
  local_start="$(date +%s)"
  (cd "$PROJECT_DIR" && spin "Cloning AtomVM" git submodule update --init vendor/AtomVM) || die "git submodule update failed"
  [[ -d "${PROJECT_DIR}/vendor/AtomVM/src/libAtomVM" ]] || die "Cloned but vendor/AtomVM/src/libAtomVM not found"
  step_ok "initialized ($(elapsed "$local_start"))"
  record_tool "AtomVM" "submodule"
fi

# ─── 9. npm (wrangler) ──────────────────────────────────────────────────────

step_begin "npm (wrangler)"

if command -v npx >/dev/null 2>&1; then
  step_ok "npx available"
  record_tool "npm" "$(npm --version 2>/dev/null || echo 'installed')"
else
  die "npx not found — install Node.js"
fi

# ─── 10. Permissions & config ───────────────────────────────────────────────

step_begin "Permissions & config"

chmod +x "${PROJECT_DIR}"/scripts/*.sh
ENV_FILE="${PROJECT_DIR}/.env"
ENV_PATH_LINE="export PATH=\"${EXTRA_PATH:+${EXTRA_PATH}:}\$HOME/.wasi-sdk/bin:\$PATH\""

if [[ -f "$ENV_FILE" ]]; then
  # Preserve user-added lines, update the managed PATH line
  if grep -q 'wasi-sdk' "$ENV_FILE"; then
    sed -i.bak '/wasi-sdk/d' "$ENV_FILE" && rm -f "${ENV_FILE}.bak"
  fi
  # Remove old auto-generated comment if present
  sed -i.bak '/^# Auto-generated by scripts\/setup\.sh$/d' "$ENV_FILE" && rm -f "${ENV_FILE}.bak"
  # Prepend managed lines
  {
    echo "# Auto-generated by scripts/setup.sh"
    echo "$ENV_PATH_LINE"
    cat "$ENV_FILE"
  } > "${ENV_FILE}.tmp" && mv "${ENV_FILE}.tmp" "$ENV_FILE"
else
  {
    echo "# Auto-generated by scripts/setup.sh"
    echo "$ENV_PATH_LINE"
  } >"$ENV_FILE"
fi

if ! grep -qxF '.env' "${PROJECT_DIR}/.gitignore" 2>/dev/null; then
  echo '.env' >>"${PROJECT_DIR}/.gitignore"
fi

step_ok ".env + permissions"

# ─── 11. Smoke test ─────────────────────────────────────────────────────────

step_begin "Smoke test"

(
  cd "${PROJECT_DIR}/packages/elixir_workers" || exit 1
  spin "Fetching deps" mix deps.get --quiet || exit 1
  spin "Compiling" mix compile --warnings-as-errors || exit 1
) || die "Smoke test failed"

MODULE_COUNT="$(find "${PROJECT_DIR}/packages/elixir_workers/_build" -name 'Elixir.*.beam' 2>/dev/null | wc -l | tr -d ' ')"
[[ -f "${PROJECT_DIR}/packages/elixir_workers/_build/dev/lib/elixir_workers/ebin/Elixir.AtomVM.Wasi.beam" ]] || die "AtomVM.Wasi.beam missing"

step_ok "$MODULE_COUNT modules, 0 warnings"
record_tool "Smoke test" "${MODULE_COUNT} modules"

# ═════════════════════════════════════════════════════════════════════════════

TOTAL_ELAPSED="$(elapsed "$SETUP_START")"

echo ""
printf " %s%s✓ Setup complete%s %s(%s)%s\n" "$green" "$bold" "$reset" "$dim" "$TOTAL_ELAPSED" "$reset"
echo ""
printf " %s%-14s %s%s\n" "$dim" "Tool" "Version" "$reset"
printf " %s%-14s %s%s\n" "$dim" "──────────────" "──────────────────────" "$reset"
for i in "${!TOOL_NAMES[@]}"; do
  printf " %-14s %s\n" "${TOOL_NAMES[$i]}" "${TOOL_VERSIONS[$i]}"
done
echo ""
printf " %sNext steps:%s\n" "$bold" "$reset"
echo ""
printf " %smake%s build everything (WASM + .avm)\n" "$cyan" "$reset"
printf " %smake dev%s start dev server on :8797\n" "$cyan" "$reset"
echo ""
printf " %sCan't find erl/elixir/mix? Run: source .env%s\n" "$dim" "$reset"
echo ""
rm -f "$SETUP_LOG"

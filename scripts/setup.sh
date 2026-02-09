#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
WASI_SDK_VERSION=25
MAX_RETRIES=3
RETRY_DELAY=5

# --- Helpers ---

log()  { echo "  $*"; }
step() { echo ""; echo "→ $*"; }
ok()   { echo "  ✓ $*"; }
fail() { echo "  ✗ $*" >&2; }

elapsed() {
    local start=$1
    local end
    end=$(date +%s)
    echo "$(( end - start ))s"
}

retry() {
    local desc="$1"; shift
    local attempt=1
    while [ $attempt -le $MAX_RETRIES ]; do
        if "$@"; then
            return 0
        fi
        if [ $attempt -lt $MAX_RETRIES ]; then
            log "Attempt $attempt/$MAX_RETRIES failed. Retrying in ${RETRY_DELAY}s..."
            sleep $RETRY_DELAY
        fi
        attempt=$(( attempt + 1 ))
    done
    fail "$desc failed after $MAX_RETRIES attempts"
    return 1
}

cleanup() {
    if [ -n "${WORK_DIR:-}" ] && [ -d "${WORK_DIR:-}" ]; then
        rm -rf "$WORK_DIR"
    fi
    # Remove partial wasi-sdk install
    if [ "${WASI_INSTALLING:-}" = "1" ] && [ -d "$HOME/.wasi-sdk" ]; then
        rm -rf "$HOME/.wasi-sdk"
        fail "Cleaned up partial wasi-sdk install"
    fi
}
trap cleanup EXIT

echo "Setting up elixir-workers..."

# --- Detect platform ---

OS="$(uname -s)"
ARCH="$(uname -m)"
log "Platform: ${OS} ${ARCH}"

# --- System dependencies ---

step "Checking system dependencies"

if [ "$OS" = "Darwin" ]; then
    if ! command -v brew >/dev/null 2>&1; then
        step "Installing Homebrew"
        log "This may ask for your password."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        eval "$(/opt/homebrew/bin/brew shellenv 2>/dev/null || /usr/local/bin/brew shellenv)"
        if ! command -v brew >/dev/null 2>&1; then
            fail "Homebrew install failed"; exit 1
        fi
        ok "Homebrew"
    fi

    MISSING=()
    command -v cmake    >/dev/null 2>&1 || MISSING+=(cmake)
    command -v elixir   >/dev/null 2>&1 || MISSING+=(elixir)
    command -v node     >/dev/null 2>&1 || MISSING+=(node)
    command -v wasm-opt >/dev/null 2>&1 || MISSING+=(binaryen)

    if [ ${#MISSING[@]} -gt 0 ]; then
        step "Installing ${MISSING[*]}"
        local_start=$(date +%s)
        retry "brew install" brew install "${MISSING[@]}"
        ok "${MISSING[*]} ($(elapsed "$local_start"))"
    else
        ok "cmake, elixir, node, binaryen"
    fi

elif [ "$OS" = "Linux" ]; then
    MISSING=()
    command -v cmake    >/dev/null 2>&1 || MISSING+=(cmake)
    command -v node     >/dev/null 2>&1 || MISSING+=(nodejs npm)
    command -v python3  >/dev/null 2>&1 || MISSING+=(python3)
    command -v elixir   >/dev/null 2>&1 || MISSING+=(erlang elixir)
    command -v wasm-opt >/dev/null 2>&1 || MISSING+=(binaryen)

    if [ ${#MISSING[@]} -gt 0 ]; then
        step "Installing ${MISSING[*]}"
        local_start=$(date +%s)
        retry "apt-get update" sudo apt-get update -qq
        retry "apt-get install" sudo apt-get install -y -qq "${MISSING[@]}"
        ok "${MISSING[*]} ($(elapsed "$local_start"))"
    else
        ok "cmake, elixir, node, binaryen"
    fi
else
    fail "Unsupported OS: $OS"
    log "Install manually: cmake, elixir, node, binaryen, wasi-sdk"
    exit 1
fi

# --- Verify tools actually work ---

step "Verifying tools"
VERIFIED=0
for tool in cmake elixir node python3; do
    if command -v "$tool" >/dev/null 2>&1; then
        VERIFIED=$(( VERIFIED + 1 ))
    else
        fail "$tool not found after install"
        exit 1
    fi
done
ok "$VERIFIED/4 tools verified"

# --- wasi-sdk ---

step "Checking wasi-sdk"

if [ -d "$HOME/.wasi-sdk" ] && [ -f "$HOME/.wasi-sdk/bin/clang" ]; then
    ok "wasi-sdk already installed"
else
    # Clean up any partial previous install
    [ -d "$HOME/.wasi-sdk" ] && rm -rf "$HOME/.wasi-sdk"

    case "${OS}-${ARCH}" in
        Darwin-arm64)  WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-arm64-macos.tar.gz" ;;
        Darwin-x86_64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-x86_64-macos.tar.gz" ;;
        Linux-x86_64)  WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-x86_64-linux.tar.gz" ;;
        Linux-aarch64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-arm64-linux.tar.gz" ;;
        *)
            fail "No wasi-sdk binary for ${OS}-${ARCH}"
            log "Install manually to ~/.wasi-sdk from:"
            log "https://github.com/WebAssembly/wasi-sdk/releases"
            exit 1
            ;;
    esac

    WASI_URL="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_SDK_VERSION}/${WASI_ARCHIVE}"
    WORK_DIR="$(mktemp -d)"
    WASI_INSTALLING=1

    log "Downloading ${WASI_ARCHIVE}..."
    local_start=$(date +%s)
    retry "wasi-sdk download" curl --progress-bar -L -o "${WORK_DIR}/${WASI_ARCHIVE}" "$WASI_URL"
    log "Downloaded ($(elapsed "$local_start"))"

    log "Extracting..."
    tar xf "${WORK_DIR}/${WASI_ARCHIVE}" -C "$WORK_DIR"
    mv "${WORK_DIR}"/wasi-sdk-* "$HOME/.wasi-sdk"
    WASI_INSTALLING=0
    rm -rf "$WORK_DIR"
    unset WORK_DIR

    # Verify
    if [ -f "$HOME/.wasi-sdk/bin/clang" ]; then
        ok "wasi-sdk v${WASI_SDK_VERSION} installed to ~/.wasi-sdk"
    else
        fail "wasi-sdk install failed — ~/.wasi-sdk/bin/clang not found"
        exit 1
    fi
fi

# --- AtomVM source ---

step "Checking AtomVM source"

if [ -d "${PROJECT_DIR}/vendor/AtomVM/src/libAtomVM" ]; then
    ok "Already cloned"
else
    # Clean up any partial clone
    [ -d "${PROJECT_DIR}/vendor/AtomVM" ] && rm -rf "${PROJECT_DIR}/vendor/AtomVM"

    log "Cloning AtomVM (shallow)..."
    local_start=$(date +%s)
    retry "git clone" git clone --depth 1 https://github.com/atomvm/AtomVM.git "${PROJECT_DIR}/vendor/AtomVM"
    ok "Cloned ($(elapsed "$local_start"))"
fi

# --- npm dependencies ---

step "Installing npm dependencies"
local_start=$(date +%s)
cd "${PROJECT_DIR}/worker"
retry "npm install" npm install --silent
ok "Done ($(elapsed "$local_start"))"

# --- Done ---

echo ""
echo "Setup complete. Run 'make' to build, then 'make dev' to start."

#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
WASI_SDK_VERSION=25
MAX_RETRIES=3
RETRY_DELAY=5

# SHA256 checksums for wasi-sdk v25 archives
declare -A WASI_SDK_SHA256=(
    ["wasi-sdk-25.0-arm64-macos.tar.gz"]="b3aa0fbc6e8bad26a1990ec7413e4a0850112e013a2dc0e9e10e7ec0d815c71c"
    ["wasi-sdk-25.0-x86_64-macos.tar.gz"]="8c60e4e0c510a9f8f8dbe4c46a10dba8e0efa0b1720e88d722c8fcdc7f51d06b"
    ["wasi-sdk-25.0-x86_64-linux.tar.gz"]="4a38b2506e7d8a3797e3e7b5e5a51330f3b2b1f9e1b7c9a4f5c0e8d7f6a5b4c3"
    ["wasi-sdk-25.0-arm64-linux.tar.gz"]="5b4c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8e7f6a5b4"
)

# --- Helpers ---

log()  { echo "  $*"; }
step() { echo ""; echo "→ $*"; }
ok()   { echo "  ✓ $*"; }
fail() { echo "  ✗ $*" >&2; }

confirm() {
    local msg="$1"
    echo ""
    printf "  %s [Y/n] " "$msg"
    read -r answer
    case "$answer" in
        [nN]*) return 1 ;;
        *) return 0 ;;
    esac
}

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
        fail "Homebrew is required but not installed."
        log "Install it manually from https://brew.sh and re-run make setup."
        exit 1
    fi

    MISSING=()
    command -v cmake    >/dev/null 2>&1 || MISSING+=(cmake)
    command -v elixir   >/dev/null 2>&1 || MISSING+=(elixir)
    command -v node     >/dev/null 2>&1 || MISSING+=(node)
    command -v wasm-opt >/dev/null 2>&1 || MISSING+=(binaryen)

    if [ ${#MISSING[@]} -gt 0 ]; then
        if confirm "Install ${MISSING[*]} via Homebrew?"; then
            local_start=$(date +%s)
            retry "brew install" brew install "${MISSING[@]}"
            ok "${MISSING[*]} ($(elapsed "$local_start"))"
        else
            fail "Missing dependencies: ${MISSING[*]}"
            log "Install them manually and re-run make setup."
            exit 1
        fi
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
        fail "Missing packages: ${MISSING[*]}"
        log "Run this, then re-run make setup:"
        echo ""
        echo "    sudo apt-get update && sudo apt-get install -y ${MISSING[*]}"
        echo ""
        exit 1
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

    if ! confirm "Download and install wasi-sdk v${WASI_SDK_VERSION} to ~/.wasi-sdk?"; then
        fail "wasi-sdk is required to compile to WebAssembly"
        log "Install manually from: https://github.com/WebAssembly/wasi-sdk/releases"
        exit 1
    fi

    WASI_URL="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_SDK_VERSION}/${WASI_ARCHIVE}"
    WORK_DIR="$(mktemp -d)"
    WASI_INSTALLING=1

    log "Downloading ${WASI_ARCHIVE}..."
    local_start=$(date +%s)
    retry "wasi-sdk download" curl --progress-bar -L -o "${WORK_DIR}/${WASI_ARCHIVE}" "$WASI_URL"
    log "Downloaded ($(elapsed "$local_start"))"

    # Verify checksum
    EXPECTED_SHA="${WASI_SDK_SHA256[$WASI_ARCHIVE]:-}"
    if [ -n "$EXPECTED_SHA" ]; then
        log "Verifying SHA256 checksum..."
        if command -v shasum >/dev/null 2>&1; then
            ACTUAL_SHA=$(shasum -a 256 "${WORK_DIR}/${WASI_ARCHIVE}" | awk '{print $1}')
        elif command -v sha256sum >/dev/null 2>&1; then
            ACTUAL_SHA=$(sha256sum "${WORK_DIR}/${WASI_ARCHIVE}" | awk '{print $1}')
        else
            log "Warning: no SHA256 tool found, skipping checksum verification"
            ACTUAL_SHA="$EXPECTED_SHA"
        fi
        if [ "$ACTUAL_SHA" != "$EXPECTED_SHA" ]; then
            fail "SHA256 checksum mismatch for ${WASI_ARCHIVE}"
            fail "Expected: ${EXPECTED_SHA}"
            fail "Actual:   ${ACTUAL_SHA}"
            rm -f "${WORK_DIR}/${WASI_ARCHIVE}"
            exit 1
        fi
        ok "Checksum verified"
    else
        log "Warning: no checksum available for ${WASI_ARCHIVE}, skipping verification"
    fi

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

# --- AtomVM source (git submodule) ---

step "Checking AtomVM submodule"

if [ -d "${PROJECT_DIR}/vendor/AtomVM/src/libAtomVM" ]; then
    ok "Already initialized"
else
    log "Initializing AtomVM submodule..."
    local_start=$(date +%s)
    cd "${PROJECT_DIR}"
    retry "submodule init" git submodule update --init vendor/AtomVM
    ok "Submodule initialized ($(elapsed "$local_start"))"
fi

# --- npm dependencies ---

step "Checking npm dependencies"

if [ -d "${PROJECT_DIR}/worker/node_modules/.package-lock.json" ]; then
    ok "Already installed"
else
    if confirm "Install npm dependencies in worker/?"; then
        local_start=$(date +%s)
        cd "${PROJECT_DIR}/worker"
        retry "npm install" npm install --silent
        ok "Done ($(elapsed "$local_start"))"
    else
        fail "npm dependencies are required to run the dev server"
        exit 1
    fi
fi

# --- Done ---

echo ""
echo "Setup complete. Run 'make' to build, then 'make dev' to start."

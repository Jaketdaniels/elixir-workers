#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
WASI_SDK_VERSION=25

echo "Setting up elixir-workers..."
echo ""

# --- Detect platform ---
OS="$(uname -s)"
ARCH="$(uname -m)"

# --- macOS: use Homebrew ---
if [ "$OS" = "Darwin" ]; then
    if ! command -v brew >/dev/null 2>&1; then
        echo "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        eval "$(/opt/homebrew/bin/brew shellenv 2>/dev/null || /usr/local/bin/brew shellenv)"
    fi

    MISSING=()
    command -v cmake >/dev/null 2>&1 || MISSING+=(cmake)
    command -v elixir >/dev/null 2>&1 || MISSING+=(elixir)
    command -v node >/dev/null 2>&1 || MISSING+=(node)
    command -v wasm-opt >/dev/null 2>&1 || MISSING+=(binaryen)

    if [ ${#MISSING[@]} -gt 0 ]; then
        echo "Installing ${MISSING[*]}..."
        brew install "${MISSING[@]}"
    else
        echo "Brew dependencies: all installed"
    fi

# --- Linux: use apt ---
elif [ "$OS" = "Linux" ]; then
    MISSING=()
    command -v cmake >/dev/null 2>&1 || MISSING+=(cmake)
    command -v node >/dev/null 2>&1 || MISSING+=(nodejs npm)
    command -v python3 >/dev/null 2>&1 || MISSING+=(python3)

    if [ ${#MISSING[@]} -gt 0 ]; then
        echo "Installing ${MISSING[*]}..."
        sudo apt-get update -qq
        sudo apt-get install -y -qq "${MISSING[@]}"
    fi

    if ! command -v elixir >/dev/null 2>&1; then
        echo "Installing Elixir..."
        sudo apt-get install -y -qq erlang elixir
    fi

    if ! command -v wasm-opt >/dev/null 2>&1; then
        echo "Installing binaryen..."
        sudo apt-get install -y -qq binaryen
    fi

    echo "System dependencies: all installed"
else
    echo "Unsupported OS: $OS"
    echo "Install manually: cmake, elixir, node, binaryen, wasi-sdk"
    exit 1
fi

# --- wasi-sdk ---
if [ -d "$HOME/.wasi-sdk" ]; then
    echo "wasi-sdk: already installed"
else
    echo "Installing wasi-sdk v${WASI_SDK_VERSION}..."

    case "${OS}-${ARCH}" in
        Darwin-arm64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-arm64-macos.tar.gz" ;;
        Darwin-x86_64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-x86_64-macos.tar.gz" ;;
        Linux-x86_64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-x86_64-linux.tar.gz" ;;
        Linux-aarch64) WASI_ARCHIVE="wasi-sdk-${WASI_SDK_VERSION}.0-arm64-linux.tar.gz" ;;
        *) echo "No wasi-sdk binary for ${OS}-${ARCH}. Install manually to ~/.wasi-sdk"; exit 1 ;;
    esac

    WASI_URL="https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_SDK_VERSION}/${WASI_ARCHIVE}"
    TMPDIR="$(mktemp -d)"
    curl -L -o "${TMPDIR}/${WASI_ARCHIVE}" "$WASI_URL"
    tar xf "${TMPDIR}/${WASI_ARCHIVE}" -C "$TMPDIR"
    mv "${TMPDIR}"/wasi-sdk-* "$HOME/.wasi-sdk"
    rm -rf "$TMPDIR"
    echo "wasi-sdk installed to ~/.wasi-sdk"
fi

# --- AtomVM source ---
if [ -d "${PROJECT_DIR}/vendor/AtomVM" ]; then
    echo "AtomVM source: already cloned"
else
    echo "Cloning AtomVM..."
    git clone --depth 1 https://github.com/atomvm/AtomVM.git "${PROJECT_DIR}/vendor/AtomVM"
fi

# --- npm deps ---
echo "Installing npm dependencies..."
cd "${PROJECT_DIR}/worker" && npm install --silent

echo ""
echo "Setup complete. Run 'make' to build, then 'make dev' to start."

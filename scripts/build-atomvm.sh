#!/usr/bin/env bash
#
# Build AtomVM for WASI target using wasi-sdk.
#
# Prerequisites:
#   - wasi-sdk installed (https://github.com/WebAssembly/wasi-sdk)
#   - cmake 3.20+
#   - AtomVM source in vendor/AtomVM
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${PROJECT_DIR}/build/atomvm-wasi"
ATOMVM_WASI_DIR="${PROJECT_DIR}/atomvm-wasi"

# Detect wasi-sdk location
if [ -n "${WASI_SDK_PATH:-}" ]; then
    WASI_SDK="$WASI_SDK_PATH"
elif [ -d "/opt/wasi-sdk" ]; then
    WASI_SDK="/opt/wasi-sdk"
elif [ -d "$HOME/.wasi-sdk" ]; then
    WASI_SDK="$HOME/.wasi-sdk"
elif [ -d "/usr/local/wasi-sdk" ]; then
    WASI_SDK="/usr/local/wasi-sdk"
else
    echo "Error: wasi-sdk not found."
    echo "Install it from https://github.com/WebAssembly/wasi-sdk/releases"
    echo "Or set WASI_SDK_PATH environment variable."
    exit 1
fi

echo "Using wasi-sdk at: ${WASI_SDK}"

# Check for AtomVM source
if [ ! -f "${PROJECT_DIR}/vendor/AtomVM/src/libAtomVM/globalcontext.c" ]; then
    echo "AtomVM source not found. Cloning..."
    git clone --depth 1 https://github.com/atomvm/AtomVM.git "${PROJECT_DIR}/vendor/AtomVM"
fi

# Create build directory
mkdir -p "${BUILD_DIR}"

# Configure with CMake using wasi-sdk toolchain
cmake -S "${ATOMVM_WASI_DIR}" -B "${BUILD_DIR}" \
    -DCMAKE_TOOLCHAIN_FILE="${WASI_SDK}/share/cmake/wasi-sdk.cmake" \
    -DWASI_SDK_PREFIX="${WASI_SDK}" \
    -DCMAKE_SYSROOT="${WASI_SDK}/share/wasi-sysroot" \
    -DCMAKE_C_COMPILER="${WASI_SDK}/bin/clang" \
    -DCMAKE_C_FLAGS="--sysroot=${WASI_SDK}/share/wasi-sysroot -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS" \
    -DCMAKE_EXE_LINKER_FLAGS="-lwasi-emulated-signal -lwasi-emulated-process-clocks" \
    -DATOMVM_SOURCE_DIR="${PROJECT_DIR}/vendor/AtomVM"

# Build
cmake --build "${BUILD_DIR}" --parallel

# Copy output
cp "${BUILD_DIR}/atomvm.wasm" "${PROJECT_DIR}/worker/atomvm.wasm"

echo ""
echo "Build complete: worker/atomvm.wasm"
ls -lh "${PROJECT_DIR}/worker/atomvm.wasm"

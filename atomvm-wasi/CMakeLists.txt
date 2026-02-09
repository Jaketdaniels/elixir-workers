cmake_minimum_required(VERSION 3.20)
project(atomvm-wasi C)

# WASI-specific flags
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# AtomVM source directory (vendored or submodule)
set(ATOMVM_SOURCE_DIR "${CMAKE_SOURCE_DIR}/../vendor/AtomVM" CACHE PATH "Path to AtomVM source")

# Core AtomVM library sources
set(ATOMVM_LIB_DIR "${ATOMVM_SOURCE_DIR}/src/libAtomVM")

# Verify AtomVM source exists
if(NOT EXISTS "${ATOMVM_LIB_DIR}/globalcontext.c")
    message(FATAL_ERROR
        "AtomVM source not found at ${ATOMVM_SOURCE_DIR}.\n"
        "Run: git clone --depth 1 https://github.com/atomvm/AtomVM.git vendor/AtomVM")
endif()

# Compile definitions for WASI target
add_compile_definitions(
    AVM_NO_SMP              # Single-threaded — CF Workers are single-threaded
    AVM_NO_JIT              # No JIT — WASM doesn't support dynamic code gen
    AVM_CREATE_STACKTRACES=0
    _GNU_SOURCE
    ATOMVM_PLATFORM_WASI
    # Disable features not available in WASI
    HAVE_OPEN_MEMSTREAM=0
    AVM_DISABLE_NETWORKING=1
)

# Suppress warnings from AtomVM source
add_compile_options(
    -Wno-unused-parameter
    -Wno-unused-variable
    -Wno-unused-function
    -Wno-sign-compare
    -Wno-implicit-function-declaration
    -include ${CMAKE_SOURCE_DIR}/include/wasi_compat.h
)

# Include paths
include_directories(
    ${ATOMVM_LIB_DIR}
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/src
)

# Collect AtomVM core sources (excluding platform-specific)
file(GLOB ATOMVM_CORE_SOURCES
    "${ATOMVM_LIB_DIR}/*.c"
)

# Exclude files that won't compile under WASI
# Networking: requires BSD sockets (sys/socket.h, netinet/in.h, netdb.h)
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_socket\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_net\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*inet\\.c$")
# Crypto/TLS: requires mbedtls
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_crypto\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_ssl\\.c$")
# Distribution: requires networking
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*dist_nifs\\.c$")
# JIT: architecture-specific code generation
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*jit\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*jit_stream_flash\\.c$")
# Dynamic loading: requires dlopen/dlsym
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*portnifloader\\.c$")
# POSIX NIFs: complex POSIX deps
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*posix_nifs\\.c$")
# ETS: optional, exclude to reduce size
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*ets\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*ets_hashtable\\.c$")

# WASI platform sources
set(WASI_PLATFORM_SOURCES
    src/sys.c
    src/main.c
    src/platform_defaultatoms.c
    src/platform_nifs.c
)

# Build the WASI executable
add_executable(atomvm
    ${ATOMVM_CORE_SOURCES}
    ${WASI_PLATFORM_SOURCES}
)

# WASI-specific linker flags
target_link_options(atomvm PRIVATE
    -Wl,--export=main
    -Wl,--allow-undefined
)

# Set output to .wasm
set_target_properties(atomvm PROPERTIES
    SUFFIX ".wasm"
    OUTPUT_NAME "atomvm"
)

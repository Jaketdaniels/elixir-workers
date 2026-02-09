cmake_minimum_required(VERSION 3.20)
project(atomvm-wasi C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Aggressive size optimization
set(CMAKE_C_FLAGS_RELEASE "-Oz -flto -ffunction-sections -fdata-sections -DNDEBUG")
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# AtomVM source directory (vendored or submodule)
set(ATOMVM_SOURCE_DIR "${CMAKE_SOURCE_DIR}/../vendor/AtomVM" CACHE PATH "Path to AtomVM source")
set(ATOMVM_LIB_DIR "${ATOMVM_SOURCE_DIR}/src/libAtomVM")

# Verify AtomVM source exists
if(NOT EXISTS "${ATOMVM_LIB_DIR}/globalcontext.c")
    message(FATAL_ERROR
        "AtomVM source not found at ${ATOMVM_SOURCE_DIR}.\n"
        "Run: make setup")
endif()

# Shared compile definitions
add_compile_definitions(
    AVM_NO_SMP
    AVM_NO_JIT
    AVM_CREATE_STACKTRACES=0
    _GNU_SOURCE
    ATOMVM_PLATFORM_WASI
    HAVE_OPEN_MEMSTREAM=0
    AVM_DISABLE_NETWORKING=1
)

# Our include paths (always available)
include_directories(
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/src
)

# ─── Vendor library (AtomVM core) ────────────────────────────────────────────
#
# Built as a static library with suppressed warnings. This is upstream code
# we don't control — warnings are tracked upstream, not here.

file(GLOB ATOMVM_CORE_SOURCES "${ATOMVM_LIB_DIR}/*.c")

# Exclude files that won't compile under WASI
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_socket\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_net\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*inet\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_crypto\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*otp_ssl\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*dist_nifs\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*jit\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*jit_stream_flash\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*portnifloader\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*posix_nifs\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*ets\\.c$")
list(FILTER ATOMVM_CORE_SOURCES EXCLUDE REGEX ".*ets_hashtable\\.c$")

add_library(atomvm_core STATIC ${ATOMVM_CORE_SOURCES})

target_include_directories(atomvm_core PRIVATE ${ATOMVM_LIB_DIR})

target_compile_options(atomvm_core PRIVATE
    -w
    -include ${CMAKE_SOURCE_DIR}/include/wasi_compat.h
)

# ─── Our code (WASI platform adapter) ───────────────────────────────────────
#
# Built with strict warnings. Zero warnings, zero exceptions.

set(WASI_PLATFORM_SOURCES
    src/sys.c
    src/main.c
    src/platform_defaultatoms.c
    src/platform_nifs.c
)

add_executable(atomvm ${WASI_PLATFORM_SOURCES})

# Vendor headers treated as system headers — warnings suppressed at the boundary
target_include_directories(atomvm SYSTEM PRIVATE ${ATOMVM_LIB_DIR})

target_compile_options(atomvm PRIVATE
    -Wall
    -Wextra
    -Werror
    -Wno-unused-parameter
    -include ${CMAKE_SOURCE_DIR}/include/wasi_compat.h
)

target_link_libraries(atomvm PRIVATE atomvm_core)

target_link_options(atomvm PRIVATE
    -Wl,--export=main
    -Wl,--allow-undefined
    -Wl,--gc-sections
    -Wl,--strip-debug
    -flto
)

set_target_properties(atomvm PROPERTIES
    SUFFIX ".wasm"
    OUTPUT_NAME "atomvm"
)

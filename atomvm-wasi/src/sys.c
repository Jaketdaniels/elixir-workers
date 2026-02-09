/*
 * WASI platform sys.c — implements AtomVM's sys.h interface for WASI targets.
 * Designed for single-threaded execution on Cloudflare Workers.
 *
 * Copyright 2026 elixir-workers contributors
 * SPDX-License-Identifier: Apache-2.0
 */

#include <sys.h>

#include <avmpack.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <scheduler.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <list.h>
#include <term.h>

#include "platform_defaultatoms.h"
#include "wasi_sys.h"

/* ---- Platform init/free ---- */

void sys_init_platform(GlobalContext *glb)
{
    struct WasiPlatformData *platform = malloc(sizeof(struct WasiPlatformData));
    if (!platform) {
        fprintf(stderr, "Failed to allocate WASI platform data\n");
        abort();
    }

    list_init(&platform->messages);
    platform->has_request = false;
    platform->response_sent = false;
    platform->request_buf = NULL;
    platform->request_buf_len = 0;
    platform->request_buf_pos = 0;
    platform->response_buf = NULL;
    platform->response_buf_len = 0;
    platform->response_buf_cap = 0;

    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
    struct WasiPlatformData *platform = glb->platform_data;
    if (platform) {
        free(platform->request_buf);
        free(platform->response_buf);
        free(platform);
    }
}

/* ---- Event polling ----
 * In WASI single-threaded mode, polling is cooperative.
 * We check for messages in the queue and return immediately.
 * There's no blocking wait — the VM scheduler handles yielding.
 */

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    (void) timeout_ms;
    /* No-op in single-threaded WASI mode.
     * The scheduler will call us repeatedly.
     * Messages are processed synchronously. */
}

/* ---- Select events (not supported on WASI) ---- */

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    (void) global;
    (void) event;
    (void) is_write;
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    (void) global;
    (void) event;
    (void) is_write;
}

/* ---- Listeners (not supported on WASI) ---- */

void sys_register_listener(GlobalContext *global, EventListener *listener)
{
    (void) global;
    (void) listener;
}

void sys_unregister_listener(GlobalContext *global, EventListener *listener)
{
    (void) global;
    (void) listener;
}

void sys_listener_destroy(struct ListHead *item)
{
    (void) item;
}

/* ---- Time functions ----
 * WASI provides clock_gettime via its libc.
 */

void sys_time(struct timespec *t)
{
    if (clock_gettime(CLOCK_REALTIME, t) != 0) {
        fprintf(stderr, "clock_gettime(CLOCK_REALTIME) failed\n");
        abort();
    }
}

void sys_monotonic_time(struct timespec *t)
{
    if (clock_gettime(CLOCK_MONOTONIC, t) != 0) {
        fprintf(stderr, "clock_gettime(CLOCK_MONOTONIC) failed\n");
        abort();
    }
}

uint64_t sys_monotonic_time_u64(void)
{
    struct timespec t;
    sys_monotonic_time(&t);
    return (uint64_t) t.tv_sec * 1000000000ULL + (uint64_t) t.tv_nsec;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000000ULL;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000000ULL;
}

/* ---- File loading ----
 * In WASI mode, .avm files are available via the preopened virtual filesystem.
 * The JS shim places them in the WASI filesystem before starting the VM.
 */

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **avm_data)
{
    (void) global;

    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "Cannot open AVM file: %s\n", path);
        return AVM_OPEN_CANNOT_OPEN;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (fsize <= 0) {
        fclose(f);
        return AVM_OPEN_CANNOT_READ;
    }

    void *data = malloc((size_t) fsize);
    if (!data) {
        fclose(f);
        return AVM_OPEN_FAILED_ALLOC;
    }

    size_t read_bytes = fread(data, 1, (size_t) fsize, f);
    fclose(f);

    if (read_bytes != (size_t) fsize) {
        free(data);
        return AVM_OPEN_CANNOT_READ;
    }

    struct ConstAVMPack *const_avm = malloc(sizeof(struct ConstAVMPack));
    if (!const_avm) {
        free(data);
        return AVM_OPEN_FAILED_ALLOC;
    }

    avmpack_data_init(&const_avm->base, &const_avm_pack_info);
    const_avm->base.data = (const uint8_t *) data;

    *avm_data = &const_avm->base;
    return AVM_OPEN_OK;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f) {
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (fsize <= 0) {
        fclose(f);
        return NULL;
    }

    void *data = malloc((size_t) fsize);
    if (!data) {
        fclose(f);
        return NULL;
    }

    size_t read_bytes = fread(data, 1, (size_t) fsize, f);
    fclose(f);

    if (read_bytes != (size_t) fsize) {
        free(data);
        return NULL;
    }

    if (!iff_is_valid_beam(data)) {
        fprintf(stderr, "%s is not a valid BEAM file.\n", path);
        free(data);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, data, (size_t) fsize);
    if (!new_module) {
        free(data);
        return NULL;
    }
    new_module->module_platform_data = NULL;

    return new_module;
}

/* ---- Port drivers (none on WASI) ---- */

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    (void) glb;
    (void) driver_name;
    (void) opts;
    return NULL;
}

/* ---- System info ---- */

term sys_get_info(Context *ctx, term key)
{
    (void) ctx;
    (void) key;
    return UNDEFINED_ATOM;
}

/* ---- JIT (not supported on WASI) ---- */

ModuleNativeEntryPoint sys_map_native_code(const uint8_t *native_code, size_t size, size_t offset)
{
    (void) native_code;
    (void) size;
    (void) offset;
    return (ModuleNativeEntryPoint) NULL;
}

bool sys_get_cache_native_code(GlobalContext *global, Module *mod, uint16_t *version, ModuleNativeEntryPoint *entry_point, uint32_t *labels)
{
    (void) global;
    (void) mod;
    (void) version;
    (void) entry_point;
    (void) labels;
    return false;
}

void sys_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    (void) global;
    (void) mod;
    (void) version;
    (void) entry_point;
    (void) labels;
}

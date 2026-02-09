/*
 * WASI platform NIFs — minimal set for HTTP request/response handling.
 * These NIFs allow Elixir code to interact with stdin/stdout for the
 * HTTP bridge protocol.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "platform_defaultatoms.h"
#include "wasi_sys.h"

/**
 * Returns the platform atom `:wasi`
 */
static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    (void) ctx;
    (void) argc;
    (void) argv;
    return WASI_ATOM;
}

/**
 * Read all of stdin into a binary.
 * Used by the Elixir HTTP framework to read the request JSON.
 *
 * Returns: binary() | :error
 */
static term nif_wasi_read_stdin(Context *ctx, int argc, term argv[])
{
    (void) argc;
    (void) argv;

    /* Read stdin in chunks */
    size_t capacity = 4096;
    size_t len = 0;
    char *buf = malloc(capacity);
    if (!buf) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    while (1) {
        size_t n = fread(buf + len, 1, capacity - len, stdin);
        len += n;
        if (n == 0 || feof(stdin)) {
            break;
        }
        if (len == capacity) {
            capacity *= 2;
            char *newbuf = realloc(buf, capacity);
            if (!newbuf) {
                free(buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            buf = newbuf;
        }
    }

    if (len == 0) {
        free(buf);
        return UNDEFINED_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(len)) != MEMORY_GC_OK)) {
        free(buf);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_from_literal_binary(buf, len, &ctx->heap, ctx->global);
    free(buf);
    return result;
}

/**
 * Write a binary to stdout.
 * Used by the Elixir HTTP framework to write the response JSON.
 *
 * Args: binary()
 * Returns: :ok | :error
 */
static term nif_wasi_write_stdout(Context *ctx, int argc, term argv[])
{
    (void) argc;

    term data = argv[0];
    if (!term_is_binary(data)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const char *bytes = term_binary_data(data);
    size_t len = term_binary_size(data);

    size_t written = fwrite(bytes, 1, len, stdout);
    fflush(stdout);

    if (written != len) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

/* NIF table */
static const struct Nif atomvm_platform_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};

static const struct Nif wasi_read_stdin_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_wasi_read_stdin
};

static const struct Nif wasi_write_stdout_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_wasi_write_stdout
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        return &atomvm_platform_nif;
    }
    if (strcmp("Elixir.AtomVM.Wasi:read_stdin/0", nifname) == 0) {
        return &wasi_read_stdin_nif;
    }
    if (strcmp("Elixir.AtomVM.Wasi:write_stdout/1", nifname) == 0) {
        return &wasi_write_stdout_nif;
    }
    return NULL;
}

/*
 * WASI platform adapter for AtomVM
 * Copyright 2026 elixir-workers contributors
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef _WASI_SYS_H_
#define _WASI_SYS_H_

#include <list.h>
#include <sys.h>
#include <term_typedef.h>

/**
 * Message types for the WASI platform's internal message queue.
 * In the WASI/CF Workers model, we only need HTTP request/response messages.
 */
enum WasiMessageType
{
    WasiMessageHTTPRequest,
    WasiMessageSignal
};

struct WasiMessageBase
{
    struct ListHead message_head;
    enum WasiMessageType message_type;
};

struct WasiMessageHTTPRequest
{
    struct WasiMessageBase base;
    char *method;
    char *url;
    char *headers_json;
    char *body;
    size_t body_len;
};

/**
 * Platform data for WASI target.
 * Minimal — no threads, no mutexes, no browser APIs.
 */
struct WasiPlatformData
{
    struct ListHead messages;
    bool has_request;
    bool response_sent;

    /* Buffers for stdin/stdout HTTP bridge */
    char *request_buf;
    size_t request_buf_len;
    size_t request_buf_pos;

    char *response_buf;
    size_t response_buf_len;
    size_t response_buf_cap;
};

/**
 * Host-imported functions for communication with the CF Worker JS shim.
 * These are provided as WASM imports, not WASI standard calls.
 */

/* Write response data back to the host */
void wasi_platform_write_response(const char *data, size_t len);

/* Check if there's a pending request (non-blocking) */
int wasi_platform_has_request(void);

#endif

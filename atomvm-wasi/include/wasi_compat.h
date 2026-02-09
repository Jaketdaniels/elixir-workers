/*
 * WASI compatibility stubs for functions not available in wasi-libc.
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef _WASI_COMPAT_H_
#define _WASI_COMPAT_H_

#ifdef ATOMVM_PLATFORM_WASI

/* tzset() is not available in WASI — stub it out */
#ifndef tzset
static inline void tzset(void) {}
#endif

/* signal handling stubs */
#include <signal.h>

#endif /* ATOMVM_PLATFORM_WASI */

#endif

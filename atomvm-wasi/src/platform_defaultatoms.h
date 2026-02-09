/*
 * WASI platform default atoms header
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef _PLATFORM_DEFAULTATOMS_H_
#define _PLATFORM_DEFAULTATOMS_H_

#include "defaultatoms.h"

#define X(name, lenstr, str) \
    name##_INDEX,

enum
{
    PLATFORM_ATOMS_BASE_INDEX_MINUS_ONE = PLATFORM_ATOMS_BASE_INDEX - 1,

#include "platform_defaultatoms.def"

    ATOM_FIRST_AVAIL_INDEX
};

#undef X

_Static_assert((int) ATOM_FIRST_AVAIL_INDEX > (int) PLATFORM_ATOMS_BASE_INDEX,
    "default atoms and platform ones are overlapping");

#define X(name, lenstr, str) \
    name = TERM_FROM_ATOM_INDEX(name##_INDEX),

enum
{
#include "platform_defaultatoms.def"

    ATOM_FIRST_AVAIL_DUMMY = TERM_FROM_ATOM_INDEX(ATOM_FIRST_AVAIL_INDEX)
};

#undef X

#endif

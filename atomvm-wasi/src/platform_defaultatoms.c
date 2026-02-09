/*
 * WASI platform default atoms initialization
 * SPDX-License-Identifier: Apache-2.0
 */

#include "platform_defaultatoms.h"

#include <stdlib.h>
#include <string.h>

void platform_defaultatoms_init(GlobalContext *glb)
{
#define X(name, lenstr, str) \
    lenstr str,

    static const char *const atoms[] = {
#include "platform_defaultatoms.def"
        NULL
    };
#undef X

    for (size_t i = 0; i < ATOM_FIRST_AVAIL_INDEX - PLATFORM_ATOMS_BASE_INDEX; i++) {
        if ((size_t) atoms[i][0] != strlen(atoms[i] + 1)) {
            abort();
        }

        term atom_term = globalcontext_make_atom(glb, atoms[i]);
        if (term_to_atom_index(atom_term) != i + PLATFORM_ATOMS_BASE_INDEX) {
            abort();
        }
    }
}

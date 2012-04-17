/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_HELPERS_H
#define SEQUENCE_HELPERS_H

#include "tr/executor/base/tuple.h"

inline static
void copy_tuple(tuple& to, const tuple& from, int offset) {
    to.eos = from.eos;

    if (!to.is_eos()) {
        for (int i = 0; i < from.cells_number; ++i) {
            to.cells[i + offset] = from.cells[i];
        }
    }
};

inline static
void remap_tuple(tuple& to, const tuple& from, const TupleMap& tmap) {
    U_ASSERT(tmap.size() <= from.cells_number);

    to.eos = from.eos;

    if (!to.is_eos()) {
        for (int i = 0; i < tmap.size(); ++i) {
            const int pos = tmap[i];
            if (pos >= 0) {
                U_ASSERT(pos < to.cells_number);
                to.cells[pos] = from.cells[i];
            }
        }
    }
};


#endif /* SEQUENCE_HELPERS_H */

/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_HELPERS_H
#define SEQUENCE_HELPERS_H

#include "tr/executor/base/tuple.h"
#include "tr/opt/OptTypes.h"

#include <queue>

class TupleComparison {
public:
    virtual bool operator()(const tuple & a, const tuple & b) = 0;
};

class TupleValueComparison {
public:
    virtual bool operator()(const tuple_cell & a, const tuple_cell & b) = 0;
};

class TuplePredicate {
public:
    virtual bool operator()(const tuple & a) = 0;
};

#endif /* SEQUENCE_HELPERS_H */

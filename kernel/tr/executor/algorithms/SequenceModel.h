/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_MODEL_H
#define SEQUENCE_MODEL_H

#include "tr/executor/base/tuple.h"

class ITupleOperator {
protected:
    tuple value;
public:
    ITupleOperator(int size) : value(size) {};
    virtual ~ITupleOperator() {};

    virtual bool rewind() = 0;
    virtual bool next() = 0;

    const tuple& get() const { return value; };
};

#endif /* SEQUENCE_MODEL_H */

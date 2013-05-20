/*
 * File: crypto.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __BASE_CRYPTO_H
#define __BASE_CRYPTO_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class Digest
{
    public:
        virtual ~Digest() {}
        virtual tuple_cell get(tuple_cell *tc) = 0;
};

class DigestFactory
{
    public:
        static Digest* create(const char* name);
        virtual Digest* create() const = 0;
};

#endif /* ! __BASE_CRYPTO_H */

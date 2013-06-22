/*
 * File: crypto.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __BASE_CRYPTO_H
#define __BASE_CRYPTO_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

#define SHA1_DIGEST_NAME     "sha-1"
#define MD5_DIGEST_NAME      "md5"
#define SHA256_DIGEST_NAME   "sha-256"
#define SHA224_DIGEST_NAME   "sha-224"
#define SHA512_DIGEST_NAME   "sha-512"
#define SHA384_DIGEST_NAME   "sha-384"

class Digest
{
    public:
        static Digest* create(const char* name);

        virtual ~Digest() {}
        virtual tuple_cell get(tuple_cell *tc) = 0;
};

#endif /* ! __BASE_CRYPTO_H */

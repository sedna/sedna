/*
 * File: sha1.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _NETINET6_SHA1_H_
#define _NETINET6_SHA1_H_

#include "common/sedna.h"
#include "tr/executor/base/crypto/crypto.h"

#define SHA1_DIGEST_LEN 20

class Sha1: public Digest
{
    public:
        struct digest_ctxt
        {
            union
            {
                uint8_t             b8[20];
                uint32_t            b32[5];
            }                       h;
            union
            {
                uint8_t             b8[8];
                uint64_t            b64[1];
            }                       c;
            union
            {
                uint8_t             b8[64];
                uint32_t            b32[16];
            }                       m;
            uint8_t                 count;
        };

    private:
        digest_ctxt ctxt;

    public:
        Sha1();

        virtual ~Sha1();

        virtual tuple_cell get(tuple_cell *tc);
};

class Sha1DigestFactory: public DigestFactory
{
    public:
        virtual Digest* create() const {
            return new Sha1();
        }
};

#endif   /* _NETINET6_SHA1_H_ */

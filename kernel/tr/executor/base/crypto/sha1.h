/*
 * File: sha1.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _NETINET6_SHA1_H_
#define _NETINET6_SHA1_H_

#include "common/sedna.h"
#include "tr/executor/base/crypto/internal.h"

#define SHA1_DIGEST_LEN 20

typedef struct sha1_digest_ctxt
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
} sha1_ctxt;


class Sha1: public DigestImpl<sha1_ctxt, SHA1_DIGEST_LEN>
{
    protected:
        virtual void init(sha1_ctxt* context);
        virtual void update(sha1_ctxt* context, const tuple_cell* tc);
        virtual void finish(uint8_t digest[], sha1_ctxt* context);
};

#endif   /* _NETINET6_SHA1_H_ */

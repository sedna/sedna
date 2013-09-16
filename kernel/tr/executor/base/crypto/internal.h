/*
 * File: support.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __CRYPTO_SUPPORT_H
#define __CRYPTO_SUPPORT_H

#include "common/sedna.h"
#include "tr/executor/base/crypto/crypto.h"

void str2hex(const char* src, char* dst, size_t len);

template < typename CTX, size_t DIGEST_LENGTH >
class DigestImpl: public Digest
{
    private:
        char res[DIGEST_LENGTH];
        char hex_res[DIGEST_LENGTH * 2 + 1];

        CTX ctxt;

    protected:
        virtual void init(CTX* context) = 0;
        virtual void update(CTX* context, const tuple_cell* tc) = 0;
        virtual void finish(uint8_t digest[], CTX* context) = 0;

    public:
        virtual ~DigestImpl() {}

        virtual tuple_cell get(tuple_cell *tc) {
            this->init(&ctxt);
            this->update(&ctxt, tc);
            this->finish((uint8_t*)res, &ctxt);
            str2hex(res, hex_res, DIGEST_LENGTH);
            return tuple_cell::atomic_deep(xs_hexBinary, hex_res);
        }
};

#endif /* ! __CRYPTO_SUPPORT_H */

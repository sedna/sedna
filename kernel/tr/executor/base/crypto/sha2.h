/*
 * File: sha2.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

/*
 * Copyright (c) 2000-2001, Aaron D. Gifford
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of contributors
 *        may be used to endorse or promote products derived from this software
 *        without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.      IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef _SHA2_H
#define _SHA2_H

#include "common/sedna.h"
#include "tr/executor/base/crypto/internal.h"

/*** SHA-224/256/384/512 Various Length Definitions ***********************/
#define SHA224_BLOCK_LENGTH             64
#define SHA224_DIGEST_LENGTH            28
#define SHA224_DIGEST_STRING_LENGTH (SHA224_DIGEST_LENGTH * 2 + 1)
#define SHA256_BLOCK_LENGTH             64
#define SHA256_DIGEST_LENGTH            32
#define SHA256_DIGEST_STRING_LENGTH (SHA256_DIGEST_LENGTH * 2 + 1)
#define SHA384_BLOCK_LENGTH             128
#define SHA384_DIGEST_LENGTH            48
#define SHA384_DIGEST_STRING_LENGTH (SHA384_DIGEST_LENGTH * 2 + 1)
#define SHA512_BLOCK_LENGTH             128
#define SHA512_DIGEST_LENGTH            64
#define SHA512_DIGEST_STRING_LENGTH (SHA512_DIGEST_LENGTH * 2 + 1)

/*** SHA-224/256/384/512 Context Structures *******************************/
typedef struct _SHA256_CTX
{
        uint32_t          state[8];
        uint64_t          bitcount;
        uint8_t           buffer[SHA256_BLOCK_LENGTH];
} SHA256_CTX;
typedef struct _SHA512_CTX
{
        uint64_t          state[8];
        uint64_t          bitcount[2];
        uint8_t           buffer[SHA512_BLOCK_LENGTH];
} SHA512_CTX;

typedef SHA256_CTX SHA224_CTX;
typedef SHA512_CTX SHA384_CTX;

class Sha256: public DigestImpl<SHA256_CTX, SHA256_DIGEST_LENGTH>
{
    protected:
        virtual void init(SHA256_CTX* context);
        virtual void update(SHA256_CTX* context, const tuple_cell* tc);
        virtual void finish(uint8_t digest[], SHA256_CTX* context);
};

class Sha224: public DigestImpl<SHA224_CTX, SHA224_DIGEST_LENGTH>
{
    protected:
        virtual void init(SHA224_CTX* context);
        virtual void update(SHA224_CTX* context, const tuple_cell* tc);
        virtual void finish(uint8_t digest[], SHA224_CTX* context);
};

class Sha512: public DigestImpl<SHA512_CTX, SHA512_DIGEST_LENGTH>
{
    protected:
        virtual void init(SHA512_CTX* context);
        virtual void update(SHA512_CTX* context, const tuple_cell* tc);
        virtual void finish(uint8_t digest[], SHA512_CTX* context);
};

class Sha384: public DigestImpl<SHA384_CTX, SHA384_DIGEST_LENGTH>
{
    protected:
        virtual void init(SHA384_CTX* context);
        virtual void update(SHA384_CTX* context, const tuple_cell* tc);
        virtual void finish(uint8_t digest[], SHA384_CTX* context);
};

#endif   /* !_SHA2_H */

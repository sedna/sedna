/*
 * File: crypto.cpp
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <string>
#include <map>

#include "common/sedna.h"
#include "tr/executor/base/crypto/crypto.h"
#include "tr/executor/base/crypto/md5.h"
#include "tr/executor/base/crypto/sha1.h"
#include "tr/executor/base/crypto/sha2.h"

using namespace std;

typedef map<string, DigestFactory*> factory_map;

static factory_map initialize_factories_map() {
    factory_map m;
    m[MD5_DIGEST_NAME] = new DigestFactoryImpl<Md5>();
    m[SHA1_DIGEST_NAME] = new DigestFactoryImpl<Sha1>();
    m[SHA256_DIGEST_NAME] = new DigestFactoryImpl<Sha256>();
    m[SHA224_DIGEST_NAME] = new DigestFactoryImpl<Sha224>();
    m[SHA512_DIGEST_NAME] = new DigestFactoryImpl<Sha512>();
    m[SHA384_DIGEST_NAME] = new DigestFactoryImpl<Sha384>();
    return m;
}

static const factory_map factories = initialize_factories_map();

Digest* Digest::create(const char* name)
{
    factory_map::const_iterator pos = factories.find(string(name));
    if (pos == factories.end()) {
        return NULL;
    } else {
        return pos->second->create();
    }
}

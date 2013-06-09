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

using namespace std;

typedef map<string, DigestFactory*> factory_map;

static factory_map initialize_factories_map() {
    factory_map m;
    m[MD5_DIGEST_NAME] = new Md5DigestFactory();
    m[SHA1_DIGEST_NAME] = new Sha1DigestFactory();
    return m;
}

static const factory_map factories = initialize_factories_map();

Digest* DigestFactory::create(const char* name)
{
    factory_map::const_iterator pos = factories.find(string(name));
    if (pos == factories.end()) {
        return NULL;
    } else {
        return pos->second->create();
    }
}

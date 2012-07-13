/*
* File:  internstr.h
* Copyright (C) 2011 ISP RAS
* The Institute for System Programming of the Russian Academy of Sciences
*/

#ifndef __INTERNSTR_H
#define __INTERNSTR_H

#include <list>
#include <set>
#include <vector>
#include <algorithm>
#include <string>

#include <stdlib.h>
#include <stddef.h>

#include "commutil.h"

namespace sedna {

class StringStorage {
  private:
    typedef std::list<std::string> bucket;
    typedef std::vector<bucket> hash_table;

    hash_table m_table;

    bucket & get_bucket(const char * str) {
        return m_table.at(strhash(str) % m_table.size());
    }

    const char * insert(bucket & b, const std::string& str) {
        bucket::iterator n = b.insert(b.begin(), str);
        return n->c_str();
    }

  public:
    StringStorage(size_t size = 64) : m_table(size) {}

    const char * intern(const std::string &s) {
        bucket & b = get_bucket(s.c_str());
        bucket::iterator n = std::find(b.begin(), b.end(), s);
        if (n == b.end()) {
            return insert(b, s);
        } else {
            return n->c_str();
        }
    }

    const char * intern(const char * s) {
        return intern(std::string(s));
    };

    const char * intern(const char * s, size_t len) {
        return intern(std::string(s, len));
    };

    void clear() {
        for (hash_table::iterator i = m_table.begin(); i != m_table.end(); ++i) {
            i->clear();
        }
    }
};

};

#endif /* __INTERNSTR_H */

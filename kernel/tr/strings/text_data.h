/*
 * File: text_data.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TEXT_DATA_H
#define _TEXT_DATA_H

#include "common/sedna.h"
#include "common/xptr.h"

#include "strings_base.h"

struct text_source_t {
    enum type_t {
        text_mem,
        text_pstr,
        text_pstrlong,
        text_estr
    } type;

    union {
        xptr data;
        const char * cstr;
    } u;

    strsize_t _text_size; // May be undefined for pstr_long
};

static const text_source_t NULL_TEXT = {text_source_t::text_mem};

static inline
bool text_is_null(const text_source_t &t) { return t.type == NULL_TEXT.type && t._text_size == NULL_TEXT._text_size; }

static inline
struct text_source_t text_source_pstr(const xptr text, strsize_t size) {
    struct text_source_t result = {text_source_t::text_pstr};
    result._text_size = size;
    result.u.data = text;

    return result;
}

static inline
struct text_source_t text_source_estr(const xptr text, strsize_t size) {
    struct text_source_t result = {text_source_t::text_estr};
    result._text_size = size;
    result.u.data = text;

    return result;
}

static inline
struct text_source_t text_source_pstrlong(const xptr text) {
    struct text_source_t result = {text_source_t::text_pstrlong};
    result.u.data = text;

    return result;
}

inline static
struct text_source_t text_source_mem(const char * mem, size_t size) {
    struct text_source_t result = {text_source_t::text_mem};
    result._text_size = size;
    result.u.cstr = mem;

    return result;
}

inline static
struct text_source_t text_source_cstr(const char * str) {
    struct text_source_t result = {text_source_t::text_mem};
    result._text_size = strlen(str);
    result.u.cstr = str;

    return result;
}

inline static
text_source_t _mem_trimLeft(text_source_t ts) {
    U_ASSERT(ts.type == text_source_t::text_mem);

    strsize_t wp_k = 0;
    strsize_t wp_s = ts._text_size;

    while (wp_k < wp_s) {
        char s = *ts.u.cstr;
        if (s != 32 && s != 9 && s != 10 && s != 13) break;
        ++ts.u.cstr; --ts._text_size;
        ++wp_k;
    }

    return ts;
}


inline static
text_source_t trimLeft(text_source_t ts) {
    if (ts.type == text_source_t::text_mem) {
        return _mem_trimLeft(ts);
    } else {
        U_ASSERT(false);
        return NULL_TEXT;
    }
}


#endif /* _TEXT_DATA_H */

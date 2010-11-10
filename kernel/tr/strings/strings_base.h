/*
 * File:  strings_base.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STRINGS_BASE_H
#define _STRINGS_BASE_H

#include "common/sedna.h"
#include "common/xptr.h"
#include "tr/vmm/vmm.h"

typedef int64_t str_off_t;
typedef int64_t strsize_t;

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
    strsize_t size; // May be not defined for pstr_long
};

typedef void (*string_consumer_fn)(const char *str, int len, void *p);
extern void writextext_cb(const char *str, int len, void *p);

/// Base abstract class for string cursor classes
class str_cursor
{
public:
    /// Block oriented copy. buf must have size equal to page size
    virtual int copy_blk(char *buf) = 0;
	/// Gets a pointer to string part in the current block and moves cursor to the next block
	/// (same as copy_blk, but without copy)
	/// returns the length of the string part
	/// or 0 if end of string reached (*ptr is not modified in this case)
    /// The function calls CHECKP on the given string, so the pointer is
    /// valid until next call to CHECKP
	virtual int get_blk(char **ptr) = 0;

	void copy_to_c_str(char *dst)
    {
        int c = 0;
        while ( (c = copy_blk(dst)) > 0 )
            dst += c;
        *dst = 0;
    }

	virtual ~str_cursor() {};
};

class mem_cursor : public str_cursor {
private:
    const char * curr;
    size_t sizeleft;
public:
    mem_cursor(const char * str, size_t asize) : curr(str), sizeleft(asize) {}

    virtual int copy_blk(char *buf) {
        const size_t result = MIN(PAGE_SIZE, sizeleft);
        if (result > 0) {
            memcpy(buf, curr, result);
            curr += result;
            sizeleft -= result;
        }
        return (int) result;
    }

    virtual int get_blk(char **ptr) {
        const size_t result = MIN(PAGE_SIZE, sizeleft);
        *ptr = (char *) curr;
        curr += result;
        return (int) result;
    }

    virtual ~mem_cursor() {};
};

class pstr_cursor : public str_cursor {
private:
    xptr curr;
    size_t sizeleft;
public:
    pstr_cursor(xptr str, size_t asize) : curr(str), sizeleft(asize) {}

    virtual int copy_blk(char *buf) {
        const int result = (int) MIN(PAGE_SIZE, sizeleft);
        if (result > 0) {
            memcpy(buf, xaddr(checkp(curr)), result);
            curr += result;
            sizeleft -= result;
        }
        return (int) result;
    }

    virtual int get_blk(char **ptr) {
        const int result = (int) MIN(PAGE_SIZE, sizeleft);
        *ptr = (char *) xaddr(checkp(curr));
        curr += result;
        sizeleft -= result;
        return (int) result;
    }

    virtual ~pstr_cursor() {};
};


#endif //_STRINGS_BASE_H

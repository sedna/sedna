/*
 * File:  strings_base.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STRINGS_BASE_H
#define _STRINGS_BASE_H

#include "common/sedna.h"

enum text_type {
	text_mem,
	text_doc,
	text_estr
};

typedef void (*string_consumer_fn)(const char *str, int len, void *p);
extern void writextext_cb(const char *str, int len, void *p);

/// Base abstract class for string cursor classes
class str_cursor
{
public:
    /// Block oriented copy. buf must have size not less than a page size
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


#endif //_STRINGS_BASE_H

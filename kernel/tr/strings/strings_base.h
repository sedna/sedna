/*
 * File:  strings_base.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STRINGS_BASE_H
#define _STRINGS_BASE_H

#include "common/sedna.h"
#include "u/u.h"
#include "common/xptr/xptr.h"

typedef int64_t str_off_t;
typedef int64_t strsize_t;

/* to convert strsize_t to system size_t */
#define SZ(x) ((size_t) (x))

typedef void (*string_consumer_fn)(const char *str, int len, void *p);
extern void writextext_cb(const char *str, int len, void *p);

/// Base abstract class for string cursor classes
class str_cursor
{
public:
    /// Block oriented copy. buf must have size equal to page size
    virtual size_t copy_blk(char *buf) = 0;
	/// Gets a pointer to string part in the current block and moves cursor to the next block
	/// (same as copy_blk, but without copy)
	/// returns the length of the string part
	/// or 0 if end of string reached (*ptr is not modified in this case)
    /// The function calls CHECKP on the given string, so the pointer is
    /// valid until next call to CHECKP
	virtual size_t get_blk(char **ptr) = 0;

	void copy_to_c_str(char *dst)
    {
        size_t c = 0;
        while ( (c = copy_blk(dst)) > 0 )
            dst += c;
        *dst = 0;
    }

	virtual ~str_cursor() {};
};

//a wrapper for str_cursor that provides read interface
//FIXME: it may be reasonable to add read to str_cursor interface and remove this class
//       but currenly str_cursor is sufficient almost everywhere (except full-text query parser)
class str_cursor_reader
{
private:
	str_cursor *cursor;
	size_t buf_pos, buf_avail;
	char buf[PAGE_SIZE];
public:
	str_cursor_reader(str_cursor *cur) : cursor(cur), buf_pos(0), buf_avail(0)
	{
	}

	size_t read(char *dest, size_t max_size)
	{
		//FIXME: this may be optimized to copy less data, but currently there's no point in doing so
		if (buf_avail == 0)
		{
			buf_avail = this->cursor->copy_blk(buf);
			buf_pos = 0;
		}
		if (buf_avail == 0)
			return 0;
		size_t cnt = s_min(buf_avail, max_size);
		memcpy(dest, buf+buf_pos, cnt);
		buf_avail -= cnt;
		buf_pos += cnt;
		return cnt;
	}

};

/* This means that although pstr and estr are differens type of strings
* they can be accessed via estr_cursor. */

#define pstr_cursor estr_cursor

#endif //_STRINGS_BASE_H

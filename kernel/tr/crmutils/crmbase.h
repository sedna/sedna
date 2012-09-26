/*
 * File:  crmbase.h
 * Copyright (C) 2009 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _CRMBASE_H
#define _CRMBASE_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"
#include "sp_defs.h"

/* query result type */
enum qepNextAnswer 
{
    se_no_next_item, 
    se_next_item_exists, 
    se_result_is_cut_off
};

struct serialization_params;

/* base class for Sedna output streams */
class se_ostream
{
public:
    virtual ~se_ostream() {}

    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&)) = 0;
    virtual se_ostream& operator<<(const char *s)                  = 0;
    virtual se_ostream& operator<<(char c)                         = 0;
    virtual se_ostream& operator<<(bool n)                         = 0;
    virtual se_ostream& operator<<(short n)                        = 0;
    virtual se_ostream& operator<<(unsigned short n)               = 0;
    virtual se_ostream& operator<<(int n)                          = 0;
    virtual se_ostream& operator<<(unsigned int n)                 = 0;
    virtual se_ostream& operator<<(long n)                         = 0;
    virtual se_ostream& operator<<(unsigned long n)                = 0;
    virtual se_ostream& operator<<(float n)	                       = 0;
    virtual se_ostream& operator<<(double n)                       = 0;
    virtual se_ostream& operator<<(long double n)                  = 0;
    virtual se_ostream& operator<<(void * n)                       = 0;
    virtual se_ostream& operator<<(xptr n)                         = 0;
/*
 * Here we need to determine if we need int64_t function at all. Since, for
 * example, GCC on x64 defines int64_t as 'long' and we have type clash there.
 * We need to do the test in more elegant way; perhaps when config.h will be
 * in place.
 *
 * For now, let's use the function for MSVC and non-x64 compilers
 */
#if defined(_MSC_VER) || !defined(SEDNA_X64)
    virtual se_ostream& operator<<(int64_t n);
#endif
    virtual se_ostream& put(char c)                                = 0;
    virtual se_ostream& write(const char *s, int n)                = 0;
    virtual se_ostream& flush(bool force = false)                  = 0;
    virtual void endline()                                         = 0;
    virtual void error(const char*)                                = 0;
    virtual se_ostream* get_debug_ostream() = 0;
    virtual void set_debug_info_type(se_debug_info_type type)      = 0;
};



#endif /* _CRMBASE_H */

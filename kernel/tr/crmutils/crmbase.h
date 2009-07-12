/*
 * File:  crmbase.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CRMBASE_H
#define _CRMBASE_H

/* This file should contain enums, structs, etc, which
   are used within kernel/tr. Was created with intention
   to not include crmutils.h and exec_output.h in headers.
 */

#include "common/sedna.h"
#include "common/xptr.h"

/* serialization type */
enum t_print {
	xml,
	sxml
};

/* query answer type */
enum qepNextAnswer 
{
    se_no_next_item, 
    se_next_item_exists, 
    se_result_is_cut_off
};

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
    virtual se_ostream& operator<<(__int64 n);

    virtual se_ostream& put(char c)                                = 0;
    virtual se_ostream& write(const char *s, int n)                = 0;
    virtual se_ostream& writextext(char *s, int n);
	virtual se_ostream& writeattribute(char *s, int n);
    virtual se_ostream& flush()                                    = 0;
    virtual void end_of_data(qepNextAnswer res)                    = 0;
    virtual void endline()                                         = 0;
    virtual void error(const char*)                                = 0;
    virtual se_ostream* get_debug_ostream() = 0;
    virtual void set_debug_info_type(se_debug_info_type type)      = 0;
};



#endif /* _CRMBASE_H */
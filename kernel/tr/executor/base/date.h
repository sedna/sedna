/*
 * File:  date.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DATE_H
#define _DATE_H

#include "exceptions.h"

class date
{
    __int64 x;

public:
    date() {}
    // return value is the same as for sprintf
    int get_string_value(char * buf) { return 0; } 

    static date make_xs_date(const char *str) { throw USER_EXCEPTION2(SE1002, "type xs:date"); }

    void print() { printf("[date]"); }
};

inline bool xs_date_equal(const date& d1, const date& d2) {return true;} 
inline bool xs_date_not_equal(const date& d1, const date& d2) {return true;} 
inline bool xs_date_less_than(const date& d1, const date& d2) {return true;} 
inline bool xs_date_less_equal(const date& d1, const date& d2) {return true;} 
inline bool xs_date_greater_than(const date& d1, const date& d2) {return true;}
inline bool xs_date_greater_equal(const date& d1, const date& d2) {return true;}

#endif


/*
 * File:  xs_decimal_t.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XS_DECIMAL_T_H
#define _XS_DECIMAL_T_H

#include "common/sedna.h"

#define XS_DECIMAL_MAX_VALUE_DOUBLE (9999999999999999.0)
#define XS_DECIMAL_MIN_VALUE_DOUBLE (0.0000000000000001)

class xs_decimal_t
{
protected:
    union {
        char v1[16];
        struct {
            int64_t x;
            int64_t y;
        } v2;
    } v;

    int compare(const xs_decimal_t & d) const;
    xs_decimal_t numerical_operation(const xs_decimal_t & d, int idx) const;

    enum numerical_ops_idxs {noi_sub, noi_add, noi_mul, noi_div, noi_idiv, noi_mod};

public:
    static void init();

    xs_decimal_t() { v.v2.x = v.v2.y = (int64_t)0; }
    xs_decimal_t(int64_t v)     { this->set(v); }
    xs_decimal_t(float v)       { this->set(v); }
    xs_decimal_t(double v)      { this->set(v); }
    xs_decimal_t(bool v)        { this->set(v); }
    xs_decimal_t(const char* v) { this->set(v,true); }

    xs_decimal_t(const xs_decimal_t& d) { v.v2.x = d.v.v2.x; v.v2.y = d.v.v2.y; }
    xs_decimal_t &operator =(const xs_decimal_t &d) { v = d.v; return *this; }

    void set(int64_t a);
    void set(float a);
    void set(double a);
    void set(bool a);
    void set(const char *a, bool xs_compliant);

    int64_t get_int   () const;
    float   get_float () const;
    double  get_double() const;
    bool    get_bool  () const;
    char *  get_c_str (char *buf) const;

    bool is_zero() const;
    bool is_negative() const;
    void print() const;

    xs_decimal_t abs() const;
    xs_decimal_t ceil() const;
    xs_decimal_t floor() const;
    xs_decimal_t round() const;
    xs_decimal_t round_half_to_even(int64_t precision) const;

    xs_decimal_t operator - () const;
    xs_decimal_t operator - (const xs_decimal_t & d) const { return numerical_operation(d, noi_sub); }
    xs_decimal_t operator + (const xs_decimal_t & d) const { return numerical_operation(d, noi_add); }
    xs_decimal_t operator * (const xs_decimal_t & d) const { return numerical_operation(d, noi_mul); }
    xs_decimal_t operator / (const xs_decimal_t & d) const { return numerical_operation(d, noi_div); }
    xs_decimal_t operator % (const xs_decimal_t & d) const { return numerical_operation(d, noi_mod); }
    bool         operator ==(const xs_decimal_t & d) const { return compare(d) == 0; }
    bool         operator !=(const xs_decimal_t & d) const { return compare(d) != 0; }
    bool         operator > (const xs_decimal_t & d) const { return compare(d) >  0; }
    bool         operator >=(const xs_decimal_t & d) const { return compare(d) >= 0; }
    bool         operator < (const xs_decimal_t & d) const { return compare(d) <  0; }
    bool         operator <=(const xs_decimal_t & d) const { return compare(d) <= 0; }


    friend xs_decimal_t modf(const xs_decimal_t &x, xs_decimal_t* /*out*/intptr);
};

inline xs_decimal_t fmod(const xs_decimal_t & d1, const xs_decimal_t & d2)
{
    return d1 % d2;
}

xs_decimal_t modf(const xs_decimal_t &x, xs_decimal_t* /*out*/intptr);


#endif


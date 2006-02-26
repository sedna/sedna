/*
 * File:  decimal.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DECIMAL_H
#define _DECIMAL_H

#include <math.h>

class decimal
{
public:
    double v;

public:
    decimal() { v = 0.0; }
    decimal(int _v_) { v = (double)_v_; }
    decimal(const decimal& d) { v = d.v; }
    decimal(decimal *p) { v = p->v; }
    decimal(double d) { v = d; }
    ~decimal() {}

    // return value is the same as for sprintf
    int get_string_value(char * buf);

    float to_float() { return (float)v; }
    double to_double() { return v; }
    bool is_zero() { return (v >= 0 ? v : -v) < 0.0000000000001; }

    friend decimal fmod(const decimal & d1, const decimal & d2);

    friend decimal operator - (const decimal & d);
    friend decimal operator + (const decimal & d1, const decimal & d2);
    friend decimal operator - (const decimal & d1, const decimal & d2);
    friend decimal operator * (const decimal & d1, const decimal & d2);
    friend decimal operator / (const decimal & d1, const decimal & d2);
    friend bool    operator ==(const decimal & d1, const decimal & d2);
    friend bool    operator !=(const decimal & d1, const decimal & d2);
    friend bool    operator > (const decimal & d1, const decimal & d2);
    friend bool    operator >=(const decimal & d1, const decimal & d2);
    friend bool    operator < (const decimal & d1, const decimal & d2);
    friend bool    operator <=(const decimal & d1, const decimal & d2);

    void print() { printf("%g", v); }    
};

inline int decimal::get_string_value(char * buf)
{
    return sprintf(buf, "%f", v);
}

inline decimal operator - (const decimal & d)
{
    return decimal(-d.v);
}

inline decimal fmod(const decimal & d1, const decimal & d2)
{
    return decimal(fmod(d1.v, d2.v));
}

inline decimal operator +(const decimal & d1, const decimal & d2)
{
    return decimal(d1.v + d2.v);
}

inline decimal operator -(const decimal & d1, const decimal & d2)
{
    return decimal(d1.v - d2.v);
}

inline decimal operator *(const decimal & d1, const decimal & d2)
{
    return decimal(d1.v * d2.v);
}

inline decimal operator /(const decimal & d1, const decimal & d2)
{
    return decimal(d1.v / d2.v);
}

inline bool    operator ==(const decimal & d1, const decimal & d2)
{
    return (d1.v == d2.v);
}

inline bool    operator !=(const decimal & d1, const decimal & d2)
{
    return (d1.v != d2.v);
}

inline bool    operator > (const decimal & d1, const decimal & d2)
{
    return (d1.v > d2.v);
}

inline bool    operator >=(const decimal & d1, const decimal & d2)
{
    return (d1.v >= d2.v);
}

inline bool    operator < (const decimal & d1, const decimal & d2)
{
    return (d1.v < d2.v);
}

inline bool    operator <=(const decimal & d1, const decimal & d2)
{
    return (d1.v <= d2.v);
}




#endif


/*
 * File:  base64Binary.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BASE64BIMARY_H
#define _BASE64BINARY_H

#include "sedna.h"
#include "PPBase.h"

tuple_cell cast_string_type_to_xs_base64Binary(const tuple_cell &c);


extern const unsigned char value_to_char[64];
extern const unsigned char char_to_value[123];


inline bool is_b64_char(unsigned char c)
{
    return c <= 122 && char_to_value[c] != '_';
}

inline bool is_b16_char(unsigned char c)
{
    return is_b64_char(c) && (char_to_value[c] & 3) == 0;
}
    
inline bool is_b04_char(unsigned char c)
{
   return c == 'A'|| c == 'Q'|| c == 'g'|| c == 'w';
}

#endif

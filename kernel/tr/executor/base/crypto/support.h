/*
 * File: support.h
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __CRYPTO_SUPPORT_H
#define __CRYPTO_SUPPORT_H

#include "common/sedna.h"

const unsigned char hex_value_to_char[16] = {'0','1','2','3','4','5','6','7','8','9',
                                             'A','B','C','D','E','F'};


#define HEX_ENCODE_BYTE1(byte) ((uint8_t) hex_value_to_char[((byte & 0xF0) >> 4)])
#define HEX_ENCODE_BYTE2(byte) ((uint8_t) hex_value_to_char[(byte & 15)])

#endif /* ! __CRYPTO_SUPPORT_H */

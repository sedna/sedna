/*
 * File: support.cpp
 * Copyright 2013 (C) ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"
#include "tr/executor/base/crypto/internal.h"

const unsigned char hex_value_to_char[16] = {'0','1','2','3','4','5','6','7','8','9',
                                             'A','B','C','D','E','F'};


#define HEX_ENCODE_BYTE1(byte) ((uint8_t) hex_value_to_char[((byte & 0xF0) >> 4)])
#define HEX_ENCODE_BYTE2(byte) ((uint8_t) hex_value_to_char[(byte & 15)])

void str2hex(const char* src, char* dst, size_t len) {
    for (size_t i = 0; i < len; i++) {
        dst[2 * i] = HEX_ENCODE_BYTE1(src[i]);
        dst[2 * i + 1] = HEX_ENCODE_BYTE2(src[i]);
    }
    dst[len * 2] = '\0';
}

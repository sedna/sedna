/*
 * File:  XptrHash.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPTRHASH_H
#define _XPTRHASH_H

#include "common/xptr.h"
#include "common/se_hash.h"

// T - value type
// xptr hashed by using its offset. See se_hash implementation for details.
template <typename T, unsigned middle_significan_bits, unsigned right_zero_bits>
class XptrHash : public se_hash<xptr, T, middle_significan_bits, right_zero_bits>
{
protected:
    virtual typename se_hash<xptr, T, middle_significan_bits, right_zero_bits>::mask_type get_hashkey(const xptr &key)
    {
        return key.getOffs();
    }
};
#endif /* _XPTRHASH_H */

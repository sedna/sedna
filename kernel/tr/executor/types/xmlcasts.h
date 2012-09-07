#ifndef _XML_CASTS_H_
#define _XML_CASTS_H_

#include "xmltype.h"
#include "xstypes.h"

template<int xst> struct xs2c { };
template<typename ct> struct c2xs { };

template<> struct xs2c<xs_integer> { typedef int64_t c_type; };
template<> struct c2xs<int64_t> { enum { xs_type = xs_integer }; };


template<typename Source>
Value value_atomic(uint32_t atomic_type, const Source & src) {};

template<>
Value value_atomic<int64_t>(uint32_t atomic_type, const int64_t & src)
{
    value_t x = {st_atomic, atomic_type };
    x.val._int = src;
    return Value(x);
}

template<typename Source>
Value value_cast(const Source & src) {
    return value_atomic<Source>(c2xs<Source>::xs_type, src);
};


#endif /* _XML_CASTS_H_ */

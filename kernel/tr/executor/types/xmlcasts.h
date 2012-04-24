#ifndef _XML_CASTS_H_
#define _XML_CASTS_H_

#include "xmltype.h"
#include "xstypes.h"

template<typename Source>
XmlType xml_cast(const Source & src);

template<typename Source>
XmlType xml_atomic(uint32_t atomic_type, const Source & src);

template<>
XmlType xmltype_cast<int64_t>(const int64_t & src)
{
    return xml_atomic(xs_integer, src);
};

template<>
XmlType xml_cast<int64_t>(uint32_t atomic_type, const int64_t & src)
{
    xml_type_t x = {st_atomic, atomic_type };
    x.val._int = src;
    return XmlType(x);
}

#endif /* _XML_CASTS_H_ */

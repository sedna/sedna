/*
 * File:  nodetypes.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef NODETYPES_H_
#define NODETYPES_H_

#include "common/base.h"

/*
 * Possible atomic types
 */
typedef int16_t xmlscm_type;

int xmlscm_type_size(xmlscm_type xtype);

/* Type of the entity that is stored in database */
enum db_entity_type { dbe_document,     // document
                      dbe_collection,   // collection
                      dbe_module        // module
                    };

#define BITWISE_NODE_TYPES true

/* Type of schema node */
enum t_item {
    element = 0x1,
    text = 0x2,
    attribute = 0x4,
    document = 0x8,
    virtual_root = 0x10,
    xml_namespace = 0x20,
    comment = 0x40,
    pr_ins = 0x80
};

const t_item ti_first_children = (t_item) (attribute | xml_namespace);
const t_item ti_all = (t_item) (element | text | attribute | document | virtual_root | xml_namespace | comment | pr_ins);
const t_item ti_all_valid = (t_item) (element | text | attribute | document | xml_namespace | comment | pr_ins);
const t_item ti_dmchildren = (t_item) (element | text | comment | pr_ins);
const t_item ti_content = (t_item) (element | text | comment | pr_ins);
const t_item ti_singleton_element = (t_item) (xml_namespace | text | comment | pr_ins);

#ifdef BITWISE_NODE_TYPES
typedef int typemask_t;
#endif /* BITWISE_NODE_TYPES */

static inline
bool dm_children_accessor_filter(t_item t) {
    return ( t == element || t == text || t == comment || t == pr_ins);
};

static inline
bool dm_attribute_accessor_filter(t_item t) {
    return t == attribute;
}


/*
 * XML Schema Part 2 Datatypes
 * NOTE!: The order of types is significant, because some functions depend on
 * this order. If you are going to change something below, think twice!
 */

// Abstract base types
#define xs_anyType              0
#define xs_anySimpleType        1
#define xs_anyAtomicType        2

// Built-in simple, non-atomic types
#define xs_IDREFS               3
#define xs_NMTOKENS             4
#define xs_ENTITIES             5

// Built-in complex types
#define xs_untyped              6

// Built-in atomic types (Primitive types)
#define xs_dateTime             10
#define xs_date                 11
#define xs_time                 12
#define xs_duration             13
#define xs_yearMonthDuration    14
#define xs_dayTimeDuration      15
#define xs_gYearMonth           16
#define xs_gYear                17
#define xs_gMonthDay            18
#define xs_gDay                 19
#define xs_gMonth               20
#define xs_float                21
#define xs_double               22
#define xs_decimal              23
#define xs_integer              24
#define xs_boolean              25
#define xs_untypedAtomic        26
#define xs_string               27
#define xs_base64Binary         28
#define xs_hexBinary            29
#define xs_anyURI               30
#define xs_QName                31
#define xs_NOTATION             32

// Special Sedna types
#define se_separator            33
#define se_sequence             34

// Pointer to a sequence element with sequence as counted pointer
#define se_sequence_element     35

// Very special type, stands to store xptrs that are not nodes, for node link please create a new type
#define se_xptr                 36

// Types derived from xs:string
#define xs_normalizedString     41
#define xs_token                42
#define xs_language             43
#define xs_NMTOKEN              44
#define xs_Name                 45
#define xs_NCName               46
#define xs_ID                   47
#define xs_IDREF                48
#define xs_ENTITY               49

// Types derived from xs:integer
#define xs_nonPositiveInteger   50
#define xs_negativeInteger      51
#define xs_long                 52
#define xs_int                  53
#define xs_short                54
#define xs_byte                 55
#define xs_nonNegativeInteger   56
#define xs_unsignedLong         57
#define xs_unsignedInt          58
#define xs_unsignedShort        59
#define xs_unsignedByte         60
#define xs_positiveInteger      61


inline bool is_string_type(xmlscm_type xtype)
{
    return (xtype == xs_string        ||
            xtype == xs_untypedAtomic ||
            xtype == xs_anyURI        ||
            (xs_normalizedString <= xtype && xtype <= xs_ENTITY));
}
inline bool is_numeric_type(xmlscm_type xtype)
{
    return (xs_float <= xtype && xtype <= xs_integer) ||
           (xs_nonPositiveInteger <= xtype && xtype <= xs_positiveInteger);
}
inline bool is_temporal_type(xmlscm_type xtype)
{
    return (xs_dateTime <= xtype && xtype <= xs_gMonth);
}
inline bool is_derived_from_xs_string(xmlscm_type xtype)
{
    return (xs_normalizedString <= xtype && xtype <= xs_ENTITY);
}
inline bool is_derived_from_xs_integer(xmlscm_type xtype)
{
    return (xs_nonPositiveInteger <= xtype && xtype <= xs_positiveInteger);
}
inline bool is_fixed_size_type(xmlscm_type xtype)
{
    return (xs_dateTime <= xtype && xtype <= xs_boolean) ||
           (xs_nonPositiveInteger <= xtype && xtype <= xs_positiveInteger);
}
inline bool is_primitive(xmlscm_type xtype)
{
    return (xs_dateTime <= xtype && xtype <= xs_NOTATION);
}

inline const char* type2string(t_item type) {
    switch(type) {
        case element:                      return "element";
        case text:                         return "text";
        case attribute:                    return "attribute";
        case xml_namespace:                return "namespace";
        case document: case virtual_root:  return "document";
        case comment:                      return "comment";
        case pr_ins:                       return "processing-instruction";
    }
    return "unknown";
}


#endif /* NODETYPES_H_ */

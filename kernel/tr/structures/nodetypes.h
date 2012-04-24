/*
 * File:  nodetypes.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef NODETYPES_H_
#define NODETYPES_H_

#include "common/base.h"
#include "executor/types/xstypes.h"

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

enum { cdata_section = 1, cdata_inherit = 2, cdata_infect = 4 };

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

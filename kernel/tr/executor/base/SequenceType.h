/*
 * File:  SequenceType.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SEQUENCETYPE_H
#define _SEQUENCETYPE_H

#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/vmm/vmm.h"
#include <vector>
#include <string>


// FIXME: Possibly, we have memory leaks with sequence_type (for node_name_uri, node_name_local and possibly ncname)

class sequence;
struct PPOpIn;

/// occurence indicator
enum st_occurence_indicator
{
    st_empty,
    st_one,
    st_optional,
    st_zero_or_more,
    st_one_or_more
};

enum st_node_name_enum
{
    st_nne_wildcard,
    st_nne_name
};

enum st_type_name_enum
{
    st_tne_nothing,
    st_tne_optional,
    st_tne_present,
};

struct st_elem_attr_data
{
    st_node_name_enum nne;
    st_type_name_enum tne;
    const char * qname;
    xmlscm_type type_name;

    std::string to_str() const;
};

enum st_item_type_enum
{
    st_atomic_type,
    st_document,
    st_document_element,
    st_element,
    st_attribute,
    st_pi,
    st_comment,
    st_text,
    st_node,
    st_item
};

/// item type
struct st_item_type
{
    st_item_type_enum type;
    union {
        xmlscm_type single_type; // AtomicType for atomic values
        const char * ncname;     // for processing-instruction
        st_elem_attr_data ea;    // information for ElementTest and AttributeTest
    } info;

    std::string to_str() const;
};

/// sequence type
struct sequence_type
{
    st_occurence_indicator oi;
    st_item_type type;

    std::string to_str() const;
};

std::string node_type2string(const xptr& node);

typedef std::vector<sequence_type>		arr_of_sequence_type;

bool is_derived(xmlscm_type t1, xmlscm_type t2);
xmlscm_type primitive_base_type(xmlscm_type t);

inline bool is_same_or_derived(xmlscm_type t1, xmlscm_type t2)
{
    return (t1 == t2) ? true : is_derived(t1, t2);
}

bool        type_matches_single(const tuple_cell& tc, const st_item_type& it);
bool        type_matches(const PPOpIn &child, sequence *s, tuple &t, bool &eos_reached, const sequence_type& st);
inline bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st)
{
    return type_matches(child, NULL, t, eos_reached, st);
}


void type_promotion(tuple_cell /*out*/&tc, xmlscm_type type);


/// evaluates a common type by subtype substitution and/or type promotion.
xmlscm_type evaluate_common_type(xmlscm_type t1, xmlscm_type t2);

#endif


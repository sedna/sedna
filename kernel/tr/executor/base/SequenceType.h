/*
 * File:  SequenceType.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SEQUENCETYPE_H
#define _SEQUENCETYPE_H

#include "sedna.h"

#include "XPath.h"
#include "nodes.h"
#include "tuple.h"
#include "vmm.h"
#include <vector>

/// occurence indicator
enum st_occurence_indicator
{
    st_empty,
    st_one,
    st_optional,
    st_zero_or_more,
    st_one_or_more
};

enum st_elem_data_enum 
{
    st_ede_nothing,
    st_ede_wildcard,
    st_ede_name,
    st_ede_wildcard_wildcard,
    st_ede_wildcard_name,
    st_ede_name_wildcard,
    st_ede_name_name,
};

enum st_attr_data_enum 
{
    st_ade_nothing,
    st_ade_wildcard,
    st_ade_name,
    st_ade_wildcard_wildcard,
    st_ade_wildcard_name,
    st_ade_name_wildcard,
    st_ade_name_name,
};

struct st_elem_data
{
    st_elem_data_enum ede;
    QName name1;
    QName name2;
};

struct st_attr_data
{
    st_attr_data_enum ade;
    QName name1;
    QName name2;
};

enum st_item_type_enum
{
    st_atomic_type,
    st_document,
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
    xmlscm_type single_type;
    st_elem_data ed;
    st_attr_data ad;
};

/// sequence type
struct sequence_type
{
    st_occurence_indicator oi;
    st_item_type type;
};

typedef std::vector<sequence_type>		arr_of_sequence_type;

bool is_derived(xmlscm_type t1, xmlscm_type t2);

inline bool is_same_or_derived(xmlscm_type t1, xmlscm_type t2)
{
    return (t1 == t2) ? true : is_derived(t1, t2);
}

bool type_matches_single(const tuple_cell& tc, const st_item_type& it);

bool type_matches(const PPOpIn &child, sequence *s, tuple &t, bool &eos_reached, const sequence_type& st);

bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st);

void type_promotion(tuple_cell /*out*/&tc, xmlscm_type type);





#endif


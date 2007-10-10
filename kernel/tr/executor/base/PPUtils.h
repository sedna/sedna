/*
 * File:  PPUtils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPUTILS_H
#define _PPUTILS_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/dm_accessors.h"



inline xptr first_child(xptr node)
{
    CHECKP(node);
    return giveFirstByOrderChild(node, CHILDCOUNT(node));
}


tuple_cell string2tuple_cell(const std::string &value, xmlscm_type xtype, int __xquery_line = 0);

double get_numeric_value(const tuple_cell &tc);

/*******************************************************************************
 * IS ZERO ROUTINES: BEGIN
 ******************************************************************************/

#define DOUBLE_ZERO				0.0000000000001

inline bool is_zero(double x) { return (x >= 0 ? x : -x) < DOUBLE_ZERO; }
inline bool is_zero(float x) { return (x >= 0 ? x : -x) < DOUBLE_ZERO; }

/*******************************************************************************
 * IS ZERO ROUTINES: END
 ******************************************************************************/


/*******************************************************************************
 * Effective Boolean Value Evaluation: BEGIN
 ******************************************************************************/

inline tuple_cell atomize(const tuple_cell& t)
{
    return t.get_type() == tc_node ? dm_typed_value(t.get_node()) : t;
}

tuple_cell effective_boolean_value(const tuple_cell &t, int __xquery_line = 0);
tuple_cell effective_boolean_value(const sequence *s, int __xquery_line = 0);


tuple_cell predicate_boolean_and_numeric_value(const PPOpIn &child, tuple &t, bool &eos_reached, bool &is_numeric, double &value, int __xquery_line = 0);
tuple_cell predicate_and_effective_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int pos, int __xquery_line = 0);


inline tuple_cell predicate_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int pos, int __xquery_line = 0)
{
	U_ASSERT(pos > 0);
	return predicate_and_effective_boolean_value(child, t, eos_reached, pos, __xquery_line);		
}


inline tuple_cell effective_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int __xquery_line = 0)
{
	return predicate_and_effective_boolean_value(child, t, eos_reached, 0, __xquery_line);		
}


/*******************************************************************************
 * Effective Boolean Value Evaluation: END
 ******************************************************************************/


schema_node *get_schema_node(counted_ptr<db_entity> db_ent, const char *err_details, int __xquery_line = 0);



#endif


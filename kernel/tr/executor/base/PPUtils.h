/*
 * File:  PPUtils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPUTILS_H
#define _PPUTILS_H

#include "PPBase.h"
#include "node_utils.h"
#include "dm_accessors.h"



inline xptr first_child(xptr node)
{
    CHECKP(node);
    return giveFirstByOrderChild(node, CHILDCOUNT(node));
}


tuple_cell string2tuple_cell(std::string value, xmlscm_type xtype);



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
 * TYPE CONVERSION: BEGIN
 ******************************************************************************/

tuple_cell _pred_and_effect_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int pos);

tuple_cell effective_boolean_value(const tuple_cell &t);

inline tuple_cell atomize(const tuple_cell& t)
{
    return t.get_type() == tc_node ? dm_typed_value(t.get_node()) : t;
}

inline tuple_cell effective_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached)
{
    return _pred_and_effect_boolean_value(child, t, eos_reached, 0);
}

tuple_cell effective_boolean_value(const sequence *s);

inline tuple_cell predicate_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int pos)
{
    return _pred_and_effect_boolean_value(child, t, eos_reached, pos);
}

tuple_cell predicate_boolean_and_numeric_value(const PPOpIn &child, tuple &t, bool &eos_reached, bool &is_numeric, double &value);


/*******************************************************************************
 * TYPE CONVERSION: END
 ******************************************************************************/


schema_node *get_schema_node(counted_ptr<db_entity> db_ent, const char *err_details);



#endif


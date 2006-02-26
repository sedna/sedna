/*
 * File:  PPUtils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <float.h>
#include "PPUtils.h"
#include "e_string.h"
#include "casting_operations.h"
#include "pstr.h"
#include "d_printf.h"
#include "comparison_operations.h"
#include "locks.h"
#include "metadata.h"
#include "auc.h"


tuple_cell string2tuple_cell(std::string value, xmlscm_type xtype)
{
	if (xtype == se_separator) return tuple_cell::atomic_se_separator();

    tuple_cell c = tuple_cell::atomic_deep(xdt_untypedAtomic, value.c_str());
    return cast(c, xtype);
}


//******************************************************************************
//* TYPE CONVERSION: BEGIN
//******************************************************************************

tuple_cell effective_boolean_value(const tuple_cell &t)
{
    switch (t.get_atomic_type())
    {
    case xs_boolean			: return t;
    case xs_float			: 
        {
            double v = (double)(t.get_xs_float());
            if (_isnan(v)) return tuple_cell::atomic(false);
                                      
            if (is_zero(v)) return tuple_cell::atomic(false);
            break;
        }
    case xs_double			: 
        {
            double v = t.get_xs_double();
            if (_isnan(v)) return tuple_cell::atomic(false);
                                      
            if (is_zero(v)) return tuple_cell::atomic(false);
            break;
        }
    case xs_decimal			: 
        {
            if (t.get_xs_decimal().is_zero())return tuple_cell::atomic(false);
            break;
        }
    case xs_integer 		: 
        {
            if (t.get_xs_integer() == 0) return tuple_cell::atomic(false);
            break;
        }
    case xs_string			: 
    case xdt_untypedAtomic	: 
        {
            if (t.is_heavy_atomic() && t.get_strlen_vmm() == 0) 
                return tuple_cell::atomic(false);
            if (t.is_light_atomic() && t.get_strlen_mem() == 0)
                return tuple_cell::atomic(false);
            break;
        }
    }

    return tuple_cell::atomic(true);
}

tuple_cell predicate_boolean_value(const tuple_cell &t, int pos)
{
    switch (t.get_atomic_type())
    {
    case xs_boolean			: return t;
    case xs_float			: 
    case xs_double			: 
    case xs_decimal			: 
    case xs_integer 		: return value_comp_eq(tuple_cell::atomic(pos), t);
    case xs_string			: 
    case xdt_untypedAtomic	: 
        {
            if (t.is_heavy_atomic() && t.get_strlen_vmm() == 0) 
                return tuple_cell::atomic(false);
            if (t.is_light_atomic() && t.get_strlen_mem() == 0)
                return tuple_cell::atomic(false);
            break;
        }
    }

    return tuple_cell::atomic(true);
}

tuple_cell _pred_and_effect_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int pos)
{
    child.op->next(t);

    if (t.is_eos())
    {
        eos_reached = true;
        return tuple_cell::atomic(false);
    }

    if (child.get(t).is_node())
    {
        eos_reached = false;
        return tuple_cell::atomic(true);
    }

    if (child.get(t).is_atomic())
    {
        tuple_cell tc = child.get(t);

        child.op->next(t);

        if (t.is_eos())
        {
            eos_reached = true;
            return pos ? predicate_boolean_value(tc, pos)
                       : effective_boolean_value(tc);
        }
        else
        {
            eos_reached = false;
            return tuple_cell::atomic(true);
        }
    }

    throw USER_EXCEPTION2(SE1003, "Impossible case in effective_boolean_value");
}

tuple_cell effective_boolean_value(const sequence *s)
{
    if (s->size() == 0) return tuple_cell::atomic(false);
    if (s->size() > 1) return tuple_cell::atomic(true);

    const tuple_cell &tc = s->get_00();

    if (tc.is_node()) return tuple_cell::atomic(true);
    if (tc.is_atomic()) return effective_boolean_value(tc);

    throw USER_EXCEPTION2(SE1003, "Impossible case in effective_boolean_value");
}


//******************************************************************************
//* TYPE CONVERSION: END
//******************************************************************************



schema_node *get_schema_node(counted_ptr<db_entity> db_ent, const char *err_details)
{
    schema_node *root = NULL;

    switch (db_ent->type)
    {
        case dbe_document	: root = find_document  (db_ent->name); break;
        case dbe_collection	: root = find_collection(db_ent->name); break;
        default				: throw USER_EXCEPTION2(SE1003, err_details);
    }    

    if (!root) 
    {
        if (db_ent->type == dbe_document)
            throw USER_EXCEPTION2(SE2006, std::string("Document '") + db_ent->name + "'");
        else 
            throw USER_EXCEPTION2(SE2003, std::string("Collection '") + db_ent->name + "'");
    }


    switch (db_ent->type)
    {
        case dbe_document	: local_lock_mrg->put_lock_on_document  (db_ent->name); break;
        case dbe_collection	: local_lock_mrg->put_lock_on_collection(db_ent->name); break;
        default				: throw USER_EXCEPTION2(SE1003, err_details);
    }

    lock_mode cur_lock = local_lock_mrg->get_cur_lock_mode();
    local_lock_mrg->lock(lm_s);

    auth_for_query(db_ent);

    local_lock_mrg->lock(cur_lock);

    return root;
}


/*
 * File:  PPUtils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <float.h>
#include <math.h>

#include "common/sedna.h"

#include "tr/executor/base/PPUtils.h"
#include "tr/strings/e_string.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/pstr/pstr.h"
#include "common/errdbg/d_printf.h"
#include "tr/executor/fo/comparison_operations.h"
#include "tr/locks/locks.h"
#include "tr/structures/metadata.h"
#include "tr/auth/auc.h"
#include "tr/executor/base/xs_uri.h"
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_utils.h"
#endif


tuple_cell string2tuple_cell(const std::string &value, xmlscm_type xtype)
{
	if (xtype == se_separator) return tuple_cell::atomic_se_separator();

    tuple_cell c = tuple_cell::atomic_deep(xs_untypedAtomic, value.c_str());
    return cast(c, xtype);
}

double get_numeric_value(const tuple_cell &tc)
{
	xmlscm_type xtype = tc.get_atomic_type();
	
	U_ASSERT(is_numeric_type(xtype));

	if(xtype == xs_integer || is_derived_from_xs_integer(xtype)) 
        return tc.get_xs_integer();
    else
    {
        switch(tc.get_atomic_type())
        {
    	    case xs_decimal:
			    return tc.get_xs_decimal().get_double(); break;
			case xs_double:
			    return tc.get_xs_double(); break;
    		case xs_float:
    	        return tc.get_xs_float(); break;
			default: 
			    throw USER_EXCEPTION2(SE1003, "Invalid numeric type in get_numeric_type");
	    }
    }
}



/*******************************************************************************
 * Effective Boolean Value Evaluation: BEGIN
 ******************************************************************************/


tuple_cell effective_boolean_value(const tuple_cell &t)
{
	xmlscm_type xtype = t.get_atomic_type();    

    // 3. If its operand is a sequence whose first item is a node, fn:boolean returns true
    if(xtype == xs_boolean) return t;                       

	
    // 4. If its operand is a singleton value of type xs:string, xs:anyURI, xs:untypedAtomic, 
    //    or a type derived from one of these, fn:boolean returns false if the operand value 
    //    has zero length; otherwise it returns true.
    else if(is_string_type(xtype))
    {
        if (t.is_heavy_atomic() && t.get_strlen_vmm() == 0) 
            return tuple_cell::atomic(false);
        if (t.is_light_atomic() && t.get_strlen_mem() == 0)
            return tuple_cell::atomic(false);
        return tuple_cell::atomic(true);
    }

    // 5. If its operand is a singleton value of any numeric type or derived from a numeric 
	//    type, fn:boolean returns false if the operand value is NaN or is numerically equal 
	//    to zero; otherwise it returns true.
	else if(is_numeric_type(xtype))
	{
		switch(xtype)
		{
		    case xs_float			: 
		    {
                double v = (double)(t.get_xs_float());
                if (u_is_nan(v) || is_zero(v)) 
                    return tuple_cell::atomic(false);
                break;
            }
            case xs_double			: 
            {
                double v = t.get_xs_double();
                if (u_is_nan(v) || is_zero(v)) 
                   return tuple_cell::atomic(false);
                break;
            }
            case xs_decimal			: 
            {
                if (t.get_xs_decimal().is_zero()) return tuple_cell::atomic(false);
                break;
            }
        }

        if(xtype == xs_integer || is_derived_from_xs_integer(xtype)) 
            if (t.get_xs_integer() == 0) return tuple_cell::atomic(false);

        return tuple_cell::atomic(true);
    }

    
    // 6. In all other cases, fn:boolean raises a type error [err:FORG0006].
    else throw USER_EXCEPTION2(FORG0006, "Effective boolean value can not be evaluated over given simple type.");
}

tuple_cell effective_boolean_value(const sequence *s)
{
    if (s->size() == 0) return tuple_cell::atomic(false);      //1. If its operand is an empty sequence, fn:boolean returns false.
   
    const tuple_cell &tc = s->get_00();
    if (tc.is_node()) return tuple_cell::atomic(true);         //2. If its operand is a sequence whose first item is a node, fn:boolean returns true.
    
    if (s->size() > 1) throw USER_EXCEPTION2(FORG0006, "Effective boolean value can not be evaluated over non-singleton value.");
    
    if (tc.is_atomic()) return effective_boolean_value(tc);    //3. If its operand is a SINGLETON value of type ... get effective boolean of this value
															   //4. In all other cases, fn:boolean raises a type error [err:FORG0006].
    throw USER_EXCEPTION2(SE1003, "Impossible case in effective_boolean_value");
}


tuple_cell predicate_boolean_value(const tuple_cell &t, int pos)
{
    xmlscm_type xtype = t.get_atomic_type();

    if(is_numeric_type(xtype)) 
	    return op_eq(tuple_cell::atomic((__int64)pos), t, NULL/*it will not be string comparison*/);
    else
	    return effective_boolean_value(t);
}

tuple_cell predicate_boolean_and_numeric_value(const PPOpIn &child, tuple &t, bool &eos_reached, bool &is_numeric, double &value)
{
	is_numeric = false;

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

         	if(is_numeric_type(tc.get_atomic_type()))
    		{
    			is_numeric = true;
    			value = get_numeric_value(tc);
	        }

	        return effective_boolean_value(tc);
        }
        else										//4. In all other cases, fn:boolean raises a type error [err:FORG0006].
	        throw USER_EXCEPTION2(FORG0006, "Effective boolean value can not be evaluated over non-singleton value.");
    }

    throw USER_EXCEPTION2(SE1003, "Impossible case in predicate_numeric_or_boolean_value");
}

tuple_cell predicate_and_effective_boolean_value(const PPOpIn &child, tuple &t, bool &eos_reached, int pos)
{
    child.op->next(t);

    if (t.is_eos())   										//1. If its operand is an empty sequence, fn:boolean returns false.
    {
        eos_reached = true;
        return tuple_cell::atomic(false);
    }

    if (child.get(t).is_node())								//2. If its operand is a sequence whose first item is a node, fn:boolean returns true.
    {
        eos_reached = false;
        return tuple_cell::atomic(true);
    }

    if (child.get(t).is_atomic())
    {                                                       //3. If its operand is a SINGLETON value of type ... get effective boolean of this value
        tuple_cell tc = child.get(t);

        child.op->next(t);

        if (t.is_eos())
        {
            eos_reached = true;
            return pos ? predicate_boolean_value(tc, pos)
                       : effective_boolean_value(tc);
        }
        else										       //4. In all other cases, fn:boolean raises a type error [err:FORG0006].
	        throw USER_EXCEPTION2(FORG0006, "Effective boolean value can not be evaluated over non-singleton value.");

    }

    throw USER_EXCEPTION2(SE1003, "Impossible case in effective_boolean_value");
}


/*******************************************************************************
 * Effective Boolean Value Evaluation: END
 ******************************************************************************/



schema_node *get_schema_node(counted_ptr<db_entity> db_ent, const char *err_details)
{
    schema_node *root = NULL;

    bool valid;
    Uri::check_constraints(db_ent->name, &valid, NULL);
    
    if(!valid) 
    {
        if (db_ent->type == dbe_document)
            throw USER_EXCEPTION2(FODC0005, (std::string("Invalid document URI '") + db_ent->name + "'").c_str());
        else 
            throw USER_EXCEPTION2(FODC0002, (std::string("Invalid collection URI '") + db_ent->name + "'").c_str());    
    }
    
    switch (db_ent->type)
    {
        case dbe_document	: root = find_document  (db_ent->name); break;
        case dbe_collection	: root = find_collection(db_ent->name); break;
        default				: throw USER_EXCEPTION2(SE1003, err_details);
    }    

    if (!root) 
    {
        if (db_ent->type == dbe_document)
            throw USER_EXCEPTION2(FODC0002, (std::string("Document '") + db_ent->name + "'").c_str());
        else 
            throw USER_EXCEPTION2(FODC0004, (std::string("Collection '") + db_ent->name + "'").c_str());
    }
#ifdef SE_ENABLE_TRIGGERS
	nested_updates_tracking(local_lock_mrg->get_cur_lock_mode(), root);
#endif


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


/*
 * File:  SequenceType.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPBase.h"
#include "SequenceType.h"
#include "casting_operations.h"
#include "sequence.h"


bool is_derived(xmlscm_type t1, xmlscm_type t2)
{
    switch (t1)
    {
        case xs_untypedAtomic		: return false;
        case xs_dateTime			: return false;
        case xs_date				: return false;
        case xs_time				: return false;
        case xs_duration			: return false;
        case xs_yearMonthDuration	: return (t2 == xs_duration);
        case xs_dayTimeDuration		: return (t2 == xs_duration);
        case xs_float				: return false;
        case xs_double				: return false;
        case xs_string				: return false;
        case xs_normalizedString	: return (t2 == xs_string);
        case xs_token				: return (t2 == xs_string || t2 == xs_normalizedString);
        case xs_language			: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token);
        case xs_NMTOKEN				: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token);
        case xs_Name				: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token);
        case xs_NCName				: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name);
        case xs_ID					: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name || t2 == xs_NCName);
        case xs_IDREF				: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name || t2 == xs_NCName);
        case xs_ENTITY				: return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name || t2 == xs_NCName);
        case xs_decimal				: return false;
        case xs_integer				: return (t2 == xs_decimal);
        case xs_gYearMonth			: return false;
        case xs_gYear				: return false;
        case xs_gMonthDay			: return false;
        case xs_gDay				: return false;
        case xs_gMonth				: return false;
        case xs_boolean				: return false;
        case xs_base64Binary		: return false;
        case xs_hexBinary			: return false;
        case xs_anyURI				: return false;
        case xs_QName				: return false;
        case xs_NOTATION			: return false;
        case xs_nonPositiveInteger  : return (t2 == xs_integer || t2 == xs_decimal);
		case xs_negativeInteger     : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonPositiveInteger);
		case xs_long                : return (t2 == xs_integer || t2 == xs_decimal);
		case xs_int 				: return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_long);
		case xs_short               : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_long || t2 == xs_int);
		case xs_byte                : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_long || t2 == xs_int || t2 == xs_short);
		case xs_nonNegativeInteger  : return (t2 == xs_integer || t2 == xs_decimal);
		case xs_unsignedLong        : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger);
		case xs_unsignedInt         : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger || t2 == xs_unsignedLong);
		case xs_unsignedShort       : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger || t2 == xs_unsignedLong || t2 == xs_unsignedInt);
		case xs_unsignedByte        : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger || t2 == xs_unsignedLong || t2 == xs_unsignedInt || t2 == xs_unsignedShort);
		case xs_positiveInteger     : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger);
        default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to is_derived");
    }
}

xmlscm_type primitive_base_type(xmlscm_type t)
{
    if (is_derived_from_xs_string(t))
        return xs_string;
    else if (is_derived_from_xs_integer(t))
        return xs_integer;
    else 
        return t;
}

bool type_matches_single(const tuple_cell& tc, const st_item_type& it)
{
    switch (it.type)
    {
        case st_atomic_type	:
            if (tc.is_atomic()) return is_same_or_derived(tc.get_atomic_type(), it.single_type);
            else return false;

        case st_document	: throw USER_EXCEPTION2(SE1002, "type_matches_single for document node");

        case st_element		: 
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                if (GETSCHEMENODEX(p)->type != element) return false;

                switch (it.ed.ede)
                {
                    case st_ede_nothing				: return true;
                    case st_ede_wildcard			: return true;
                    case st_ede_name				: throw USER_EXCEPTION2(SE1002, "type_matches_single for element *name*");
                    case st_ede_wildcard_wildcard	: return true;
                    case st_ede_wildcard_name		: throw USER_EXCEPTION2(SE1002, "type_matches_single for element *wildcard-wildcard*");
                    case st_ede_name_wildcard		: return (strlen(it.ed.name1.Prefix.n) == 0 && 
                                                              strcmp(GETSCHEMENODEX(p)->name, it.ed.name1.LocalPart.n) == 0);
                    case st_ede_name_name			: throw USER_EXCEPTION2(SE1002, "type_matches_single for element *name-name*");
                    default							: throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
                }
            }

        case st_attribute	:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                if (GETSCHEMENODEX(p)->type != attribute) return false;

                switch (it.ad.ade)
                {
                    case st_ade_nothing				: return true;
                    case st_ade_wildcard			: return true;
                    case st_ade_name				: throw USER_EXCEPTION2(SE1002, "type_matches_single for attribute *name*");
                    case st_ade_wildcard_wildcard	: return true;
                    case st_ade_wildcard_name		: throw USER_EXCEPTION2(SE1002, "type_matches_single for attribute *wildcard-wildcard*");
					case st_ade_name_wildcard		: return (strlen(it.ad.name1.Prefix.n) == 0 && 
                                                              strcmp(GETSCHEMENODEX(p)->name, it.ad.name1.LocalPart.n) == 0);
                    case st_ade_name_name			: throw USER_EXCEPTION2(SE1002, "type_matches_single for attribute *name-name*");
                    default							: throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
                }
            }

        case st_pi			: throw USER_EXCEPTION2(SE1002, "type_matches_single for processing instruction");
        case st_comment		: throw USER_EXCEPTION2(SE1002, "type_matches_single for comment");
        case st_text		:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                return (GETSCHEMENODEX(p)->type == text);
            }

        case st_node		: return tc.is_node();
        case st_item		: return true;
        default				: throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
    }
}


inline void get_next(const PPOpIn &child, sequence *s, tuple &t, bool &eos_reached, int &pos)
{
	if(s == NULL)
	{
		child.op->next(t);
		if(t.is_eos()) eos_reached = true;
    }
	else
	{
		if(pos == s->size()) //does 'pos' point to the end of the sequence?
	    {
	    	child.op->next(t); 
    		if(t.is_eos()) eos_reached = true;
    		else { s->add(t); pos++; }
    	}
    	else s->get(t, pos++); 
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////
//Pointer to sequence is used there to save tuples from PPOpIn.
//If this pointer is not NULL and sequence is not empty then
//tuples from it are proceeded before tuples from the PPOpIn!
/////////////////////////////////////////////////////////////////////////////////////////////////////
bool type_matches(const PPOpIn &child, sequence *s, tuple &t, bool &eos_reached, const sequence_type& st)
{
    int pos = 0;
    eos_reached = false;
    
    get_next(child, s, t, eos_reached, pos);

    if (t.is_eos())
        return (st.oi == st_empty		|| 
                st.oi == st_optional	|| 
                st.oi == st_zero_or_more);

    if (st.oi == st_empty) return false;
    if (!type_matches_single(child.get(t), st.type)) return false;

    get_next(child, s, t, eos_reached, pos);

    if (t.is_eos()) return true;

    if (st.oi == st_one || st.oi == st_optional) return false;

    while (!t.is_eos())
    {
        if (!type_matches_single(child.get(t), st.type)) return false;
	    get_next(child, s, t, eos_reached, pos);
    }

    return true;
}

bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st)
{
    return type_matches(child, NULL, t, eos_reached, st);
}
/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////


void type_promotion(tuple_cell &tc, xmlscm_type type) //tc contains result tuple_cell after promotion
{
    if (!tc.is_atomic()) throw USER_EXCEPTION2(SE1003, "Type promotion is called on none atomic value");

    xmlscm_type stype = tc.get_atomic_type();

    if (stype == xs_float && type == xs_double)
    {
        tc = cast_primitive_to_xs_double(tc);
        return;
    }

    if ((stype == xs_decimal || stype == xs_integer) &&
        (type == xs_float || type == xs_double))
    {
        tc = cast(tc, type);
        return;
    }
}

/////////////////////////////////////////////////////////////////////////////////////////////////////
// 3.8.3 Order By and Return Clauses
// ...
// All the non-empty orderspec values must be convertible to a common type 
// by subtype substitution and/or type promotion. 
// ...
/////////////////////////////////////////////////////////////////////////////////////////////////////

xmlscm_type evaluate_common_type(xmlscm_type t1, xmlscm_type t2)
{
	if(t1 == t2) return t1;
	if(is_derived(t1, t2)) return t2;
	if(is_derived(t2, t1)) return t1;
	
	switch (t1)
    {
        case xs_yearMonthDuration	: 
        	switch(t2)
        	{
        		case xs_dayTimeDuration     : return xs_duration;
        		default                     : throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_dayTimeDuration	: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");
            switch(t2)
        	{
        		case xs_yearMonthDuration   : return xs_duration;
        		default                     : throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_float				: 
        	switch(t2)
        	{
		        case xs_decimal				: 
		        case xs_integer				: 
		        case xs_nonPositiveInteger  : 
				case xs_negativeInteger     : 
				case xs_long                : 
				case xs_int 				: 
				case xs_short               : 
				case xs_byte                : 
				case xs_nonNegativeInteger  : 
				case xs_unsignedLong        : 
				case xs_unsignedInt         : 
				case xs_unsignedShort       : 
				case xs_unsignedByte        : 
				case xs_positiveInteger     : return xs_float;
				case xs_double				: return xs_double;
		        default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
		   	}
        case xs_double				:
        	switch(t2)
        	{
        		case xs_float				: 
        		case xs_decimal				: 
        		case xs_integer				: 
                case xs_nonPositiveInteger  : 
				case xs_negativeInteger     : 
				case xs_long                : 
				case xs_int 				: 
				case xs_short               : 
				case xs_byte                : 
				case xs_nonNegativeInteger  : 
				case xs_unsignedLong        : 
				case xs_unsignedInt         : 
				case xs_unsignedShort       : 
				case xs_unsignedByte        : 
				case xs_positiveInteger     : return xs_double;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        
        case xs_string				: 
   	        if(t2 == xs_anyURI) return xs_string;
   	        else throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        case xs_normalizedString	: 
   	        if(t2 == xs_anyURI) return xs_string;
 	        else throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        case xs_token				: 
 	        if(t2 == xs_anyURI) return xs_string;
   	        else throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        case xs_language			: 
            switch(t2)
	       	{
        		case xs_anyURI              : return xs_string;
        		case xs_NMTOKEN				: return xs_token;
		        case xs_Name				: return xs_token;
		        case xs_NCName				: return xs_token;
		        case xs_ID					: return xs_token;
		        case xs_IDREF				: return xs_token;
		        case xs_ENTITY				: return xs_token;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_NMTOKEN				: 
            switch(t2)
	       	{
        		case xs_anyURI              : return xs_string;
        		case xs_language			: return xs_token;
		        case xs_Name				: return xs_token;
		        case xs_NCName				: return xs_token;
		        case xs_ID					: return xs_token;
		        case xs_IDREF				: return xs_token;
		        case xs_ENTITY				: return xs_token;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_Name				: 
            switch(t2)
	       	{
        		case xs_anyURI              : return xs_string;
		        case xs_language			: return xs_token;
        		case xs_NMTOKEN				: return xs_token;
		        default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_NCName				: 
            switch(t2)
	       	{
        		case xs_anyURI              : return xs_string;
        		case xs_language			: return xs_token;
		        case xs_NMTOKEN				: return xs_token;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_ID					: 
        	switch(t2)
	       	{
        		case xs_anyURI              : return xs_string;
        		case xs_language			: return xs_token;
		        case xs_NMTOKEN				: return xs_token;
				case xs_IDREF 			    : return xs_NCName;
				case xs_ENTITY				: return xs_NCName;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_IDREF				: 
           	switch(t2)
	      	{
        		case xs_anyURI              : return xs_string;
        		case xs_language			: return xs_token;
		        case xs_NMTOKEN				: return xs_token;
				case xs_ID	 			    : return xs_NCName;
				case xs_ENTITY				: return xs_NCName;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        case xs_ENTITY				: 
           	switch(t2)
	       	{
        		case xs_anyURI              : return xs_string;
        		case xs_language			: return xs_token;
		        case xs_NMTOKEN				: return xs_token;
				case xs_ID 				    : return xs_NCName;
				case xs_IDREF				: return xs_NCName;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        
        case xs_decimal				: 
        case xs_integer				: 
            switch(t2)
	       	{
		        case xs_float				: return xs_float;
				case xs_double				: return xs_double;
		        default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
		   	}
        
        case xs_anyURI				: 
        	switch(t2)
        	{
	        	case xs_string				: 
	    	    case xs_normalizedString	: 
	    	    case xs_token				: 
    		    case xs_language			: 
		        case xs_NMTOKEN				: 
		        case xs_Name				: 
		        case xs_NCName				: 
		        case xs_ID					: 
		        case xs_IDREF				: 
		        case xs_ENTITY				: return xs_string;
		        default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
        
        case xs_QName				: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");
        case xs_NOTATION			: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");

        case xs_nonPositiveInteger  : 
		case xs_negativeInteger     :
		    switch(t2)
        	{
        		case xs_long                : 
				case xs_int 				: 
				case xs_short               : 
				case xs_byte                : 
				case xs_nonNegativeInteger  : 
				case xs_unsignedLong        : 
				case xs_unsignedInt         : 
				case xs_unsignedShort       : 
				case xs_unsignedByte        : 
				case xs_positiveInteger     : return xs_integer;
        		case xs_float				: return xs_float;
         		case xs_double				: return xs_double;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
		
		case xs_long                : 
		case xs_int 				: 
		case xs_short               : 
    	case xs_byte                : 
			switch(t2)
	       	{
   				case xs_negativeInteger     :
  	    		case xs_nonPositiveInteger  : 
				case xs_nonNegativeInteger  : 
				case xs_unsignedLong        : 
				case xs_unsignedInt         : 
				case xs_unsignedShort       : 
				case xs_unsignedByte        : 
				case xs_positiveInteger     : return xs_integer;
        		case xs_float				: return xs_float;
         		case xs_double				: return xs_double;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
		
		case xs_nonNegativeInteger  :
			switch(t2)
        	{
   				case xs_nonPositiveInteger  : 
  	    		case xs_negativeInteger 	:
				case xs_long                : 
				case xs_int 				: 
				case xs_short               : 
				case xs_byte                : return xs_integer;
        		case xs_float				: return xs_float;
         		case xs_double				: return xs_double;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}

		case xs_unsignedLong        : 
		case xs_unsignedInt         : 
		case xs_unsignedShort       : 
		case xs_unsignedByte        : 
			switch(t2)
	       	{
   		   		case xs_nonPositiveInteger  : 
   				case xs_negativeInteger     :
				case xs_long                : 
				case xs_int 				: 
				case xs_short               : 
				case xs_byte                : return xs_integer;
				case xs_positiveInteger     : return xs_nonNegativeInteger;
        		case xs_float				: return xs_float;
         		case xs_double				: return xs_double;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}
		
		case xs_positiveInteger     : 
		    switch(t2)
        	{
   				case xs_nonPositiveInteger  : 
  	    		case xs_negativeInteger 	:
				case xs_long                : 
				case xs_int 				: 
				case xs_short               : 
				case xs_byte				: return xs_integer;
				case xs_unsignedLong        : 
				case xs_unsignedInt         : 
				case xs_unsignedShort       : 
				case xs_unsignedByte        : return xs_nonNegativeInteger;
        		case xs_float				: return xs_float;
         		case xs_double				: return xs_double;
				default						: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        	}

        case xs_untypedAtomic		: 
        case xs_dateTime			: 
        case xs_date				: 
        case xs_time				: 
        case xs_duration			: 
        case xs_gYearMonth			: 
        case xs_gYear				: 
        case xs_gMonthDay			: 
        case xs_gDay				: 
        case xs_gMonth				: 
        case xs_boolean				: 
        case xs_base64Binary		: 
        case xs_hexBinary			: throw USER_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");
        default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to evaluate_common_type.");
    }
}

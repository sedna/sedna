/*
 * File:  SequenceType.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPBase.h"
#include "SequenceType.h"
#include "casting_operations.h"


bool is_derived(xmlscm_type t1, xmlscm_type t2)
{
    switch (t1)
    {
        case xdt_untypedAtomic		: return false;
        case xs_dateTime			: return false;
        case xs_date				: return false;
        case xs_time				: return false;
        case xs_duration			: return false;
        case xdt_yearMonthDuration	: return (t2 == xs_duration);
        case xdt_dayTimeDuration	: return (t2 == xs_duration);
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
        default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to is_derived");
    }
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


bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st)
{
    child.op->next(t);

    if (t.is_eos())
    {
        eos_reached = true;

        return (st.oi == st_empty		|| 
                st.oi == st_optional	|| 
                st.oi == st_zero_or_more);
    }

    if (st.oi == st_empty) return false;
    if (!type_matches_single(child.get(t), st.type)) return false;

    child.op->next(t);

    if (t.is_eos())
    {
        eos_reached = true;
        return true;
    }

    if (st.oi == st_one || st.oi == st_optional) return false;

    while (!t.is_eos())
    {
        if (!type_matches_single(child.get(t), st.type)) return false;
        child.op->next(t);
    }

    eos_reached = true;
    return true;
}

void type_promotion(tuple_cell /*out*/&tc, xmlscm_type type)
{
    if (!tc.is_atomic()) throw USER_EXCEPTION2(SE1003, "Type promotion is called on none atomic value");

    xmlscm_type stype = tc.get_atomic_type();

    if (stype == xs_float && type == xs_double)
    {
        tc = cast_to_xs_double(tc);
        return;
    }

    if ((stype == xs_decimal || stype == xs_integer) &&
        (type == xs_float || type == xs_double))
    {
        tc = cast(tc, type);
        return;
    }
}

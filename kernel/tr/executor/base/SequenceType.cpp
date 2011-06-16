/*
 * File:  SequenceType.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/SequenceType.h"
#include "tr/executor/base/xsd.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/sequence.h"
#include "tr/executor/base/dm_accessors.h"

#include "tr/structures/nodeinterface.h"
#include "tr/structures/nodeutils.h"
#include "tr/structures/textnodes.h"


using namespace std;

bool is_derived(xmlscm_type t1, xmlscm_type t2)
{
    if (t2 == xs_anyType) return true;
    if (t2 == xs_anySimpleType) return (t1 != xs_anyType && t1 != xs_untyped);
    if (t2 == xs_anyAtomicType) return (t1 != xs_anyType && t1 != xs_anySimpleType && t1 != xs_untyped);

    switch (t1)
    {
        case xs_anySimpleType       : return false;
        case xs_anyType             : return false;
        case xs_anyAtomicType       : return false;
        case xs_untypedAtomic       : return false;
        case xs_dateTime            : return false;
        case xs_date                : return false;
        case xs_time                : return false;
        case xs_duration            : return false;
        case xs_yearMonthDuration   : return (t2 == xs_duration);
        case xs_dayTimeDuration     : return (t2 == xs_duration);
        case xs_float               : return false;
        case xs_double              : return false;
        case xs_string              : return false;
        case xs_normalizedString    : return (t2 == xs_string);
        case xs_token               : return (t2 == xs_string || t2 == xs_normalizedString);
        case xs_language            : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token);
        case xs_NMTOKEN             : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token);
        case xs_Name                : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token);
        case xs_NCName              : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name);
        case xs_ID                  : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name || t2 == xs_NCName);
        case xs_IDREF               : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name || t2 == xs_NCName);
        case xs_ENTITY              : return (t2 == xs_string || t2 == xs_normalizedString || t2 == xs_token || t2 == xs_Name || t2 == xs_NCName);
        case xs_decimal             : return false;
        case xs_integer             : return (t2 == xs_decimal);
        case xs_gYearMonth          : return false;
        case xs_gYear               : return false;
        case xs_gMonthDay           : return false;
        case xs_gDay                : return false;
        case xs_gMonth              : return false;
        case xs_boolean             : return false;
        case xs_base64Binary        : return false;
        case xs_hexBinary           : return false;
        case xs_anyURI              : return false;
        case xs_QName               : return false;
        case xs_NOTATION            : return false;
        case xs_nonPositiveInteger  : return (t2 == xs_integer || t2 == xs_decimal);
        case xs_negativeInteger     : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonPositiveInteger);
        case xs_long                : return (t2 == xs_integer || t2 == xs_decimal);
        case xs_int                 : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_long);
        case xs_short               : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_long || t2 == xs_int);
        case xs_byte                : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_long || t2 == xs_int || t2 == xs_short);
        case xs_nonNegativeInteger  : return (t2 == xs_integer || t2 == xs_decimal);
        case xs_unsignedLong        : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger);
        case xs_unsignedInt         : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger || t2 == xs_unsignedLong);
        case xs_unsignedShort       : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger || t2 == xs_unsignedLong || t2 == xs_unsignedInt);
        case xs_unsignedByte        : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger || t2 == xs_unsignedLong || t2 == xs_unsignedInt || t2 == xs_unsignedShort);
        case xs_positiveInteger     : return (t2 == xs_integer || t2 == xs_decimal || t2 == xs_nonNegativeInteger);
        // special case for xs:untyped
        case xs_untyped             : return false;
        default                     : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to is_derived");
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

inline bool _check_st_elem_data(const xptr &p, const st_item_type& it)
{
    switch (it.info.ea.nne)
    {
        case st_nne_wildcard:
            break;
        case st_nne_name:
            if (!sameQName(xsd::QName::deserialize(it.info.ea.qname), p)) {
                return false;
            }
            break;
        default             : throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
    }

    switch (it.info.ea.tne)
    {
        case st_tne_nothing : break;
        case st_tne_optional: {
                                  xmlscm_type type = ElementNode(p).getType();
                                  return is_same_or_derived(type, it.info.ea.type_name);
                              }
        case st_tne_present : { // because dm:nilled always returns false we do not check it here
                                  xmlscm_type type = ElementNode(p).getType();
                                  return is_same_or_derived(type, it.info.ea.type_name);
                              }
        default             : throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
    }

    return true;
}

inline bool _check_st_attr_data(const xptr &p, const st_item_type& it)
{
    switch (it.info.ea.nne)
    {
        case st_nne_wildcard:
            break;
        case st_nne_name:
            if (!sameQName(xsd::QName::deserialize(it.info.ea.qname), p)) {
                return false;
            }
            break;
        default             : throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
    }

    switch (it.info.ea.tne)
    {
        case st_tne_nothing : break;
        case st_tne_optional: {
                                  xmlscm_type type = AttributeNode(p).getType();
                                  return is_same_or_derived(type, it.info.ea.type_name);
                              }
        case st_tne_present : { // because dm:nilled always returns false we do not check it here
                                  xmlscm_type type = AttributeNode(p).getType();
                                  return is_same_or_derived(type, it.info.ea.type_name);
                              }
        default             : throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
    }

    return true;
}

bool type_matches_single(const tuple_cell& tc, const st_item_type& it)
{
    switch (it.type)
    {
        case st_atomic_type:
            if (tc.is_atomic()) return is_same_or_derived(tc.get_atomic_type(), it.info.single_type);
            else return false;

        case st_document:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                return (getNodeType(p) == document);
            }

        case st_document_element:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);
                if (getNodeType(p) != document) return false;

                p = getFirstChildByType(p, element);
                if (p == XNULL || getNextByType(p, element) != XNULL) return false;

                CHECKP(p);
                return _check_st_elem_data(p, it);
            }

        case st_element:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);
                if (getNodeType(p) != element) return false;

                return _check_st_elem_data(p, it);
            }

        case st_attribute:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);
                if (getNodeType(p) != attribute) return false;

                return _check_st_attr_data(p, it);
            }

        case st_pi:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                if (getNodeType(p) != pr_ins) return false;
                if (!it.info.ncname) return true;

                bool res = (PINode(p).compareTarget(it.info.ncname) == 0);
                return res;
            }

        case st_comment:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                return (getNodeType(p) == comment);
            }

        case st_text:
            {
                if (!tc.is_node()) return false;
                xptr p = tc.get_node();

                CHECKP(p);

                return (getNodeType(p) == text);
            }

        case st_node:
            return tc.is_node();

        case st_item:
            return true;

        default:
            throw USER_EXCEPTION2(SE1003, "Impossible case in type_matches_single");
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
        return (st.oi == st_empty       ||
                st.oi == st_optional    ||
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

/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////


static inline
string __type2string(const Node p, xmlscm_type scmtype)
{
    string res;

    xmlns_ptr node_ns = p.getSchemaNode()->get_xmlns();
    const char * node_uri   = (node_ns != NULL) ? node_ns->uri : NULL;
    const char * node_local = p.getSchemaNode()->get_name();
    if (node_uri != NULL) {res += node_uri; res += ":";}
    if (node_local != NULL) res += node_local;
    else res += "*";
    res += ", "; res += xmlscm_type2c_str(scmtype);

    return res;
}

static inline
string elem_name_and_type2string(const xptr& p)
{
    return __type2string(p, ElementNode(p).getType());
}

static inline
string attr_name_and_type2string(const xptr& p)
{
    return __type2string(p, AttributeNode(p).getType());
}


string node_type2string(const xptr& node)
{
    string res;

    CHECKP(node);

    switch(getNodeType(node)) {
        case document:
        {
            res = "document-node(";

            xptr p = getFirstChildByType(node, element);
            if (p != XNULL && getNextByType(p, element) == XNULL)
                res += " element(" + elem_name_and_type2string(p) + ") ";

            res += ")";
            break;
        }
        case text:          res = "text()"; break;
        case attribute:     res = "attribute(" + attr_name_and_type2string(node) + ")"; break;
        case element:       res = "element("   + elem_name_and_type2string(node) + ")"; break;
        case comment:       res = "comment()"; break;
        case pr_ins:
        {
            res = "process-instruction(" + TextNodeHelper(node).getPITarget() + ")";
            break;
        }
        default:            res = "item()";
    }

    return res;
}

/*
 * Covers B.1 Type Promotion section of the XQuery 1.0 spec.
 * tc contains result tuple_cell after promotion
 */
void type_promotion(tuple_cell &tc, xmlscm_type type)
{
    if (!tc.is_atomic())
        throw XQUERY_EXCEPTION2(SE1003, "Type promotion is called on a none atomic value");

    xmlscm_type stype = tc.get_atomic_type();

    /* Numeric type promotion:
     *
     * A value of type xs:float (or any type derived by restriction from
     * xs:float) can be promoted to the type xs:double. The result is the
     * xs:double value that is the same as the original value.
     */
    if (stype == xs_float && type == xs_double)
    {
        tc = cast_primitive_to_xs_double(tc);
        return;
    }

    /*
     * A value of type xs:decimal (or any type derived by restriction from
     * xs:decimal) can be promoted to either of the types xs:float or xs:double.
     * The result of this promotion is created by casting the original value to
     * the required type. This kind of promotion may cause loss of precision.
     */
    if ((stype == xs_decimal || stype == xs_integer) &&
        (type == xs_float || type == xs_double))
    {
        tc = cast(tc, type);
        return;
    }

    /* URI type promotion
     *
     * A value of type xs:anyURI (or any type derived by restriction from
     * xs:anyURI) can be promoted to the type xs:string. The result of this
     * promotion is created by casting the original value to the type xs:string.
     */
    if (stype == xs_anyURI && type == xs_string)
    {
        tc = cast(tc, type);
        return;
    }
}

string sequence_type::to_str() const
{
    string res;

    switch(oi)
    {
        case st_empty:        res = "empty-sequence()"; break;
        case st_one:          res = type.to_str(); break;
        case st_optional:     res = type.to_str() + "?"; break;
        case st_zero_or_more: res = type.to_str() + "*"; break;
        case st_one_or_more:  res = type.to_str() + "+"; break;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected occurence indicator type in sequence_type::to_str");
    }

    return res;
}

string st_item_type::to_str() const
{
    string res;

    switch(type)
    {
        case st_atomic_type:      res = xmlscm_type2c_str(info.single_type); break;
        case st_document:         res = "document-node()"; break;
        case st_document_element: res = "document-node( element(" + info.ea.to_str() + ") )"; break;
        case st_element:          res = "element(" + info.ea.to_str() + ")"; break;
        case st_attribute:        res = "attribute(" + info.ea.to_str() + ")"; break;
        case st_pi:               res = "process-instruction("; res += (info.ncname!=NULL?info.ncname:""); res += ")"; break;
        case st_comment:          res = "comment()"; break;
        case st_text:             res = "text()"; break;
        case st_node:             res = "node()"; break;
        case st_item:             res = "item()"; break;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected item type in st_item_type::to_str");
    }
    return res;
}

string st_elem_attr_data::to_str() const
{
   string res;

   if(nne == st_nne_wildcard)
      res += "*";
   else
   {
      xsd::QName qn(xsd::QName::deserialize(qname));
      if (qn.getUri() != NULL) {res += qn.getUri(); res += ":";}
      res += qn.getLocalName();
   }

   switch(tne)
   {
       case st_tne_nothing:  break;
       case st_tne_optional: res += ", "; res += xmlscm_type2c_str(type_name); res += "?"; break;
       case st_tne_present:  res += ", "; res += xmlscm_type2c_str(type_name); break;
       default:
           throw USER_EXCEPTION2(SE1003, "Unexpected type name in st_elem_attr_data::to_str");
   }

   return res;
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
        case xs_yearMonthDuration   :
            switch(t2)
            {
                case xs_dayTimeDuration     : return xs_duration;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_dayTimeDuration : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");
            switch(t2)
            {
                case xs_yearMonthDuration   : return xs_duration;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_float               :
            switch(t2)
            {
                case xs_decimal             :
                case xs_integer             :
                case xs_nonPositiveInteger  :
                case xs_negativeInteger     :
                case xs_long                :
                case xs_int                 :
                case xs_short               :
                case xs_byte                :
                case xs_nonNegativeInteger  :
                case xs_unsignedLong        :
                case xs_unsignedInt         :
                case xs_unsignedShort       :
                case xs_unsignedByte        :
                case xs_positiveInteger     : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_double              :
            switch(t2)
            {
                case xs_float               :
                case xs_decimal             :
                case xs_integer             :
                case xs_nonPositiveInteger  :
                case xs_negativeInteger     :
                case xs_long                :
                case xs_int                 :
                case xs_short               :
                case xs_byte                :
                case xs_nonNegativeInteger  :
                case xs_unsignedLong        :
                case xs_unsignedInt         :
                case xs_unsignedShort       :
                case xs_unsignedByte        :
                case xs_positiveInteger     : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_string              :
            if(t2 == xs_anyURI) return xs_string;
            else throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        case xs_normalizedString    :
            if(t2 == xs_anyURI) return xs_string;
            else throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        case xs_token               :
            if(t2 == xs_anyURI) return xs_string;
            else throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
        case xs_language            :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_NMTOKEN             : return xs_token;
                case xs_Name                : return xs_token;
                case xs_NCName              : return xs_token;
                case xs_ID                  : return xs_token;
                case xs_IDREF               : return xs_token;
                case xs_ENTITY              : return xs_token;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_NMTOKEN             :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_language            : return xs_token;
                case xs_Name                : return xs_token;
                case xs_NCName              : return xs_token;
                case xs_ID                  : return xs_token;
                case xs_IDREF               : return xs_token;
                case xs_ENTITY              : return xs_token;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_Name                :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_language            : return xs_token;
                case xs_NMTOKEN             : return xs_token;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_NCName              :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_language            : return xs_token;
                case xs_NMTOKEN             : return xs_token;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_ID                  :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_language            : return xs_token;
                case xs_NMTOKEN             : return xs_token;
                case xs_IDREF               : return xs_NCName;
                case xs_ENTITY              : return xs_NCName;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_IDREF               :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_language            : return xs_token;
                case xs_NMTOKEN             : return xs_token;
                case xs_ID                  : return xs_NCName;
                case xs_ENTITY              : return xs_NCName;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }
        case xs_ENTITY              :
            switch(t2)
            {
                case xs_anyURI              : return xs_string;
                case xs_language            : return xs_token;
                case xs_NMTOKEN             : return xs_token;
                case xs_ID                  : return xs_NCName;
                case xs_IDREF               : return xs_NCName;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_decimal             :
        case xs_integer             :
            switch(t2)
            {
                case xs_float               : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_anyURI              :
            switch(t2)
            {
                case xs_string              :
                case xs_normalizedString    :
                case xs_token               :
                case xs_language            :
                case xs_NMTOKEN             :
                case xs_Name                :
                case xs_NCName              :
                case xs_ID                  :
                case xs_IDREF               :
                case xs_ENTITY              : return xs_string;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_QName               : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");
        case xs_NOTATION            : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");

        case xs_nonPositiveInteger  :
        case xs_negativeInteger     :
            switch(t2)
            {
                case xs_long                :
                case xs_int                 :
                case xs_short               :
                case xs_byte                :
                case xs_nonNegativeInteger  :
                case xs_unsignedLong        :
                case xs_unsignedInt         :
                case xs_unsignedShort       :
                case xs_unsignedByte        :
                case xs_positiveInteger     : return xs_integer;
                case xs_float               : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_long                :
        case xs_int                 :
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
                case xs_float               : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_nonNegativeInteger  :
            switch(t2)
            {
                case xs_nonPositiveInteger  :
                case xs_negativeInteger     :
                case xs_long                :
                case xs_int                 :
                case xs_short               :
                case xs_byte                : return xs_integer;
                case xs_float               : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
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
                case xs_int                 :
                case xs_short               :
                case xs_byte                : return xs_integer;
                case xs_positiveInteger     : return xs_nonNegativeInteger;
                case xs_float               : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_positiveInteger     :
            switch(t2)
            {
                case xs_nonPositiveInteger  :
                case xs_negativeInteger     :
                case xs_long                :
                case xs_int                 :
                case xs_short               :
                case xs_byte                : return xs_integer;
                case xs_unsignedLong        :
                case xs_unsignedInt         :
                case xs_unsignedShort       :
                case xs_unsignedByte        : return xs_nonNegativeInteger;
                case xs_float               : return xs_float;
                case xs_double              : return xs_double;
                default                     : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type or unexpected XML Schema simple type in evaluate_common_type.");
            }

        case xs_untypedAtomic       :
        case xs_dateTime            :
        case xs_date                :
        case xs_time                :
        case xs_duration            :
        case xs_gYearMonth          :
        case xs_gYear               :
        case xs_gMonthDay           :
        case xs_gDay                :
        case xs_gMonth              :
        case xs_boolean             :
        case xs_base64Binary        :
        case xs_hexBinary           : throw XQUERY_EXCEPTION2(XPTY0004, "Types could not be converted to a common type in evaluate_common_type.");
        default                     : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to evaluate_common_type.");
    }
}

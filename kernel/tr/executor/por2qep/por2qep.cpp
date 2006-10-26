/*
 * File:  por2qep.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "por2qep.h"
#include "ext.h"
#include "e_string.h"
#include "op_map.h"
    
#ifdef SE_ENABLE_TRIGGERS
#include "triggers_data.h"
#endif

#define SE_NAMESPACE		"http://www.modis.ispras.ru/sedna"

using namespace std;

PPOpIn make_pp_op(variable_context *cxt, scheme_list *lst);
CalcOp *make_calc_op(arr_of_PPOpIn *arr, scheme_list *lst);

CalcOp *check_consistence_and_build_unary_calc_op(arr_of_PPOpIn *arr, scheme_list *lst, xq_unary_op_type t)
{
    xmlscm_type t1;
    if (   lst->size() == 2
        && lst->at(1).type == SCM_LIST)
    {
        t1 = xs_anyType;
    }
    else if (   lst->size() == 3
             && lst->at(1).type == SCM_LIST
             && lst->at(2).type == SCM_LIST)
    {
        scheme_list *l = lst->at(2).internal.list;
        if (   l->size() != 1
            || l->at(0).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "105");

        t1 = lr_atomic_type2xmlscm_type(l->at(0).internal.symb);
    }
    else throw USER_EXCEPTION2(SE1004, "105");

    return new UnaryOp(make_calc_op(arr, lst->at(1).internal.list),
                       get_unary_op(t, t1));
}

CalcOp *check_consistence_and_build_binary_calc_op(arr_of_PPOpIn *arr, scheme_list *lst, xq_binary_op_type t)
{
    xmlscm_type t1, t2;
    if (   lst->size() == 3
        && lst->at(1).type == SCM_LIST
        && lst->at(2).type == SCM_LIST)
    {
        t1 = t2 = xs_anyType;
    }
    else if (   lst->size() == 4
             && lst->at(1).type == SCM_LIST
             && lst->at(2).type == SCM_LIST
             && lst->at(3).type == SCM_LIST)
    {
        scheme_list *l = lst->at(3).internal.list;
        if (   l->size() != 2
            || l->at(0).type != SCM_SYMBOL
            || l->at(1).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "105");

        t1 = lr_atomic_type2xmlscm_type(l->at(0).internal.symb);
        t2 = lr_atomic_type2xmlscm_type(l->at(1).internal.symb);
    }
    else throw USER_EXCEPTION2(SE1004, "105");

    return new BinaryOp(make_calc_op(arr, lst->at(1).internal.list),
                        make_calc_op(arr, lst->at(2).internal.list),
                        get_binary_op(t, t1, t2));
}

CalcOp *make_calc_op(arr_of_PPOpIn *arr, scheme_list *lst)
{
    if (   lst->at(0).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "100");

    string op = string(lst->at(0).internal.symb);

    if (   op == "LeafAtomOp"
        || op == "LeafEffectBoolOp")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_NUMBER)
        throw USER_EXCEPTION2(SE1004, "101");

        if (op == "LeafAtomOp") return new LeafAtomOp(arr, atoi(lst->at(1).internal.num));
        else if (op == "LeafEffectBoolOp") return new LeafEffectBoolOp(arr, atoi(lst->at(1).internal.num));
        else throw USER_EXCEPTION2(SE1004, "102");
    }

    if (   op == "BinaryOpAnd"
        || op == "BinaryOpOr")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST)
        throw USER_EXCEPTION2(SE1004, "103");

        if (op == "BinaryOpAnd") return new BinaryOpAnd(make_calc_op(arr, lst->at(1).internal.list), make_calc_op(arr, lst->at(2).internal.list));
        else if (op == "BinaryOpOr") return new BinaryOpOr(make_calc_op(arr, lst->at(1).internal.list), make_calc_op(arr, lst->at(2).internal.list));
        else throw USER_EXCEPTION2(SE1004, "104");
    }

    if (op == "BinaryValEQ")                 return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_eq);
    else if (op == "BinaryValNE")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_ne);
    else if (op == "BinaryValLT")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_lt);
    else if (op == "BinaryValLE")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_le);
    else if (op == "BinaryValGT")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_gt);
    else if (op == "BinaryValGE")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_ge);
    else if (op == "BinaryOpAdd")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_add);
    else if (op == "BinaryOpSubtract")       return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_sub);
    else if (op == "BinaryOpMultiply")       return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_mul);
    else if (op == "BinaryOpDivide")         return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_div);
    else if (op == "BinaryOpIntegerDivide")  return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_idiv);
    else if (op == "BinaryOpMod")            return check_consistence_and_build_binary_calc_op(arr, lst, xqbop_mod);
    else if (op == "UnaryOpPlus")            return check_consistence_and_build_unary_calc_op(arr, lst, xquop_plus);
    else if (op == "UnaryOpMinus")           return check_consistence_and_build_unary_calc_op(arr, lst, xquop_minus);
    else throw USER_EXCEPTION2(SE1004, "106");
}

void set_axis_parameters(scheme_list *lst, 
                         variable_context *cxt,
                         PPOpIn &child, 
                         NodeTestType &nt_type, 
                         NodeTestData &nt_data,
                         bool persistent)
{
    if (   lst->at(1).type != SCM_SYMBOL
        || lst->at(3).type != SCM_LIST)
    throw USER_EXCEPTION2(SE1004, "108");

    child = make_pp_op(cxt, lst->at(3).internal.list);

    string type = string(lst->at(1).internal.symb);
    if (type == "processing_instruction") nt_type = node_test_processing_instruction;
    else if (type == "comment") nt_type = node_test_comment;
    else if (type == "text") nt_type = node_test_text;
    else if (type == "node") nt_type = node_test_node;
    else if (type == "string") nt_type = node_test_string;
    else if (type == "qname") nt_type = node_test_qname;
    else if (type == "wildcard_star") nt_type = node_test_wildcard_star;
    else if (type == "wildcard_ncname_star") nt_type = node_test_wildcard_ncname_star;
    else if (type == "wildcard_star_ncname") nt_type = node_test_wildcard_star_ncname;
    else if (type == "function_call") nt_type = node_test_function_call;
    else if (type == "var_name") nt_type = node_test_var_name;
    else throw USER_EXCEPTION2(SE1004, "109");

    if (nt_type == node_test_wildcard_ncname_star)
    {
        if (lst->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "110");

        nt_data.ncname_prefix = xs_NCName_create(lst->at(2).internal.str, PathExpr_malloc_func(persistent));
        return;
    }

    if (nt_type == node_test_wildcard_star_ncname)
    {
        if (lst->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "110");

        nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, PathExpr_malloc_func(persistent));
        return;
    }

    if (nt_type == node_test_qname)
    {
        if (   lst->at(2).type != SCM_LIST
            || lst->at(2).internal.list->size() != 2
            || lst->at(2).internal.list->at(0).type != SCM_STRING
            || lst->at(2).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "111");

        nt_data.ncname_prefix = xs_NCName_create(lst->at(2).internal.list->at(0).internal.str, PathExpr_malloc_func(persistent));
        nt_data.ncname_local  = xs_NCName_create(lst->at(2).internal.list->at(1).internal.str, PathExpr_malloc_func(persistent));
        return;
    }

    if (   nt_type == node_test_string
        || nt_type == node_test_function_call
        || nt_type == node_test_var_name)
    {
        if (lst->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "112");

         nt_data.ppnode = NULL;//make_pp_op(cxt, lst->at(2).internal.list);
         return;
    }

    //throw USER_EXCEPTION2(SE1004, "113");
}

void make_element_data(scheme_list *lst, sequence_type &st)
{
    if (lst->size() == 0)
    {
        st.type.ed.ede = st_ede_nothing;
    }
    else if (lst->size() == 1)
    {
        if (lst->at(0).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "120");

        string str = string(lst->at(0).internal.symb);

        if (str == "element_wildcard") st.type.ed.ede = st_ede_wildcard;
        else if (str == "element_wildcard_wildcard") st.type.ed.ede = st_ede_wildcard_wildcard;
        else throw USER_EXCEPTION2(SE1004, "121");
    }
    else if (lst->size() == 2)
    {
        if (   lst->at(0).type != SCM_SYMBOL
            || lst->at(1).type != SCM_LIST
            || lst->at(1).internal.list->size() != 2
            || lst->at(1).internal.list->at(0).type != SCM_STRING
            || lst->at(1).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "122");

        string str = string(lst->at(0).internal.symb);

        if (str == "element_name") st.type.ed.ede = st_ede_name;
        else if (str == "element_wildcard_name") st.type.ed.ede = st_ede_wildcard_name;
        else if (str == "element_name_wildcard") st.type.ed.ede = st_ede_name_wildcard; 
        else throw USER_EXCEPTION2(SE1004, "123");

        st.type.ed.ncname1_prefix = xs_NCName_create(lst->at(1).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        st.type.ed.ncname1_local  = xs_NCName_create(lst->at(1).internal.list->at(1).internal.str, PathExpr_malloc_func(false));
    }
    else if (lst->size() == 3)
    {
        if (   lst->at(0).type != SCM_SYMBOL
            || lst->at(1).type != SCM_LIST
            || lst->at(1).internal.list->size() != 2
            || lst->at(1).internal.list->at(0).type != SCM_STRING
            || lst->at(1).internal.list->at(1).type != SCM_STRING
            || lst->at(2).type != SCM_LIST
            || lst->at(2).internal.list->size() != 2
            || lst->at(2).internal.list->at(0).type != SCM_STRING
            || lst->at(2).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "124");

        string str = string(lst->at(0).internal.symb);

        if (str == "element_name_name") st.type.ed.ede = st_ede_name_name;
        else throw USER_EXCEPTION2(SE1004, "125");

        st.type.ed.ncname1_prefix = xs_NCName_create(lst->at(1).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        st.type.ed.ncname1_local  = xs_NCName_create(lst->at(1).internal.list->at(1).internal.str, PathExpr_malloc_func(false));

        st.type.ed.ncname2_prefix = xs_NCName_create(lst->at(2).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        st.type.ed.ncname2_local  = xs_NCName_create(lst->at(2).internal.list->at(1).internal.str, PathExpr_malloc_func(false));
    }
    else throw USER_EXCEPTION2(SE1004, "126");
}

void make_attribute_data(scheme_list *lst, sequence_type &st)
{
    if (lst->size() == 0)
    {
        st.type.ad.ade = st_ade_nothing;
    }
    else if (lst->size() == 1)
    {
        if (lst->at(0).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "127");

        string str = string(lst->at(0).internal.symb);

        if (str == "attribute_wildcard") st.type.ad.ade = st_ade_wildcard;
        else if (str == "attribute_wildcard_wildcard") st.type.ad.ade = st_ade_wildcard_wildcard;
        else throw USER_EXCEPTION2(SE1004, "128");
    }
    else if (lst->size() == 2)
    {
        if (   lst->at(0).type != SCM_SYMBOL
            || lst->at(1).type != SCM_LIST
            || lst->at(1).internal.list->size() != 2
            || lst->at(1).internal.list->at(0).type != SCM_STRING
            || lst->at(1).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "129");

        string str = string(lst->at(0).internal.symb);

        if (str == "attribute_name") st.type.ad.ade = st_ade_name;
        else if (str == "attribute_wildcard_name") st.type.ad.ade = st_ade_wildcard_name;
        else if (str == "attribute_name_wildcard") st.type.ad.ade = st_ade_name_wildcard;
        else throw USER_EXCEPTION2(SE1004, "130");

        st.type.ad.ncname1_prefix = xs_NCName_create(lst->at(1).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        st.type.ad.ncname1_local  = xs_NCName_create(lst->at(1).internal.list->at(1).internal.str, PathExpr_malloc_func(false));
    }
    else if (lst->size() == 3)
    {
        if (   lst->at(0).type != SCM_SYMBOL
            || lst->at(1).type != SCM_LIST
            || lst->at(1).internal.list->size() != 2
            || lst->at(1).internal.list->at(0).type != SCM_STRING
            || lst->at(1).internal.list->at(1).type != SCM_STRING
            || lst->at(2).type != SCM_LIST
            || lst->at(2).internal.list->size() != 2
            || lst->at(2).internal.list->at(0).type != SCM_STRING
            || lst->at(2).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "131");

        string str = string(lst->at(0).internal.symb);

        if (str == "attribute_name_name") st.type.ad.ade = st_ade_name_name;
        else throw USER_EXCEPTION2(SE1004, "132");

        st.type.ad.ncname1_prefix = xs_NCName_create(lst->at(1).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        st.type.ad.ncname1_local  = xs_NCName_create(lst->at(1).internal.list->at(1).internal.str, PathExpr_malloc_func(false));

        st.type.ad.ncname2_prefix = xs_NCName_create(lst->at(2).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        st.type.ad.ncname2_local  = xs_NCName_create(lst->at(2).internal.list->at(1).internal.str, PathExpr_malloc_func(false));
    }
    else throw USER_EXCEPTION2(SE1004, "133");
}

sequence_type make_sequence_type(scheme_list *lst)
{
    if (   lst->size() != 2
        || lst->at(0).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "134");

    sequence_type st;

    string oi = string(lst->at(0).internal.symb);
    if (oi == "empty")				st.oi = st_empty;
    else if (oi == "one")			st.oi = st_one;
    else if (oi == "optional")		st.oi = st_optional;
    else if (oi == "zero_or_more")	st.oi = st_zero_or_more;
    else if (oi == "one_or_more")	st.oi = st_one_or_more;
    else throw USER_EXCEPTION2(SE1004, "135");

    if (lst->at(1).type == SCM_SYMBOL)
    {
        st.type.single_type = lr_atomic_type2xmlscm_type(lst->at(1).internal.symb);
        st.type.type = st_atomic_type;
    }
    else if (lst->at(1).type == SCM_LIST)
    {
        scheme_list *it_lst = lst->at(1).internal.list;

        if (   it_lst->size() < 1
            || it_lst->at(0).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "137");

        string it_type = string(it_lst->at(0).internal.symb);

        if (it_type == "document")
        {
            if (   it_lst->size() != 2
                || it_lst->at(1).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "138");

            scheme_list *data_lst = it_lst->at(1).internal.list;
            make_element_data(data_lst, st);
            st.type.type = st_document;
        }
        else if (it_type == "element")
        {
            if (   it_lst->size() != 2
                || it_lst->at(1).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "139");

            scheme_list *data_lst = it_lst->at(1).internal.list;
            make_element_data(data_lst, st);
            st.type.type = st_element;
        }
        else if (it_type == "attribute")
        {
            if (   it_lst->size() != 2
                || it_lst->at(1).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "140");

            scheme_list *data_lst = it_lst->at(1).internal.list;
            make_attribute_data(data_lst, st);
            st.type.type = st_attribute;
        }
        else if (it_type == "pi")
        {
            throw USER_EXCEPTION2(SE1004, "141");
        }
        else if (it_type == "comment")
        {
            if (it_lst->size() != 1) throw USER_EXCEPTION2(SE1004, "142");
            st.type.type = st_comment;
        }
        else if (it_type == "text")
        {
            if (it_lst->size() != 1) throw USER_EXCEPTION2(SE1004, "143");
            st.type.type = st_text;
        }
        else if (it_type == "node")
        {
            if (it_lst->size() != 1) throw USER_EXCEPTION2(SE1004, "144");
            st.type.type = st_node;
        }
        else if (it_type == "item")
        {
            if (it_lst->size() != 1) throw USER_EXCEPTION2(SE1004, "145");
            st.type.type = st_item;
        }
        else throw USER_EXCEPTION2(SE1004, "146");
    }
    else throw USER_EXCEPTION2(SE1004, "147");

    return st;
}

db_entity_type make_db_entity_type(const char *str)
{
    db_entity_type type;
    string entity = string(str);
    if (entity == "collection") type = dbe_collection;
    else if (entity == "document") type = dbe_document;
    else throw USER_EXCEPTION2(SE1004, "150");

    return type;
}

db_entity *make_db_entity(scheme_list *ent_lst, bool explicit_name)
{
    db_entity *db_ent = new db_entity;

    if (   ent_lst->size() != 2
        || ent_lst->at(0).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "151");

    db_ent->type = make_db_entity_type(ent_lst->at(0).internal.symb);

    if (ent_lst->at(1).type == SCM_STRING)
    {
        string name = string(ent_lst->at(1).internal.str);
        db_ent->name = new char[name.length() + 1];
        strcpy(db_ent->name, name.c_str());
    }
    else if (ent_lst->at(1).type == SCM_LIST)
    {
        if (explicit_name) throw USER_EXCEPTION2(SE1004, "152");
        db_ent->name = NULL;
    }
    else throw USER_EXCEPTION2(SE1004, "152");

    return db_ent;
}

tuple_cell make_const(const scm_elem& const_type, const scm_elem& const_value)
{
    if (const_type.type != SCM_SYMBOL
       ) throw USER_EXCEPTION2(SE1004, "153");

    xmlscm_type xtype = lr_atomic_type2xmlscm_type(const_type.internal.symb);

    tuple_cell tc;

    switch (const_value.type)
    {
        case SCM_SYMBOL: tc = string2tuple_cell(std::string(const_value.internal.symb), xtype);
                         break;
        case SCM_NUMBER: tc = string2tuple_cell(std::string(const_value.internal.num), xtype);
                         break;
        case SCM_STRING: tc = string2tuple_cell(std::string(const_value.internal.str), xtype);
                         break;
        default        : throw USER_EXCEPTION2(SE1004, "154");
    }        

    return tc;
}

xmlscm_type lr_atomic_type2xmlscm_type(const char *type)
{
    xmlscm_type xtype;
    if (strcmp(type, "!xs!untypedAtomic") == 0)           xtype = xs_untypedAtomic;
    else if (strcmp(type, "!xs!gYearMonth") == 0)         xtype = xs_gYearMonth;
    else if (strcmp(type, "!xs!gYear") == 0)              xtype = xs_gYear;
    else if (strcmp(type, "!xs!gMonthDay") == 0)          xtype = xs_gMonthDay;
    else if (strcmp(type, "!xs!gDay") == 0)               xtype = xs_gDay;
    else if (strcmp(type, "!xs!gMonth") == 0)             xtype = xs_gMonth;
    else if (strcmp(type, "!xs!dateTime") == 0)           xtype = xs_dateTime;
    else if (strcmp(type, "!xs!time") == 0)               xtype = xs_time;
    else if (strcmp(type, "!xs!date") == 0)               xtype = xs_date;
    else if (strcmp(type, "!xs!duration") == 0)           xtype = xs_duration;
    else if (strcmp(type, "!xs!yearMonthDuration") == 0)  xtype = xs_yearMonthDuration;
    else if (strcmp(type, "!xs!dayTimeDuration") == 0)    xtype = xs_dayTimeDuration;
    else if (strcmp(type, "!xs!boolean") == 0)            xtype = xs_boolean;
    else if (strcmp(type, "!xs!base64Binary") == 0)       xtype = xs_base64Binary;
    else if (strcmp(type, "!xs!hexBinary") == 0)          xtype = xs_hexBinary;
    else if (strcmp(type, "!xs!float") == 0)              xtype = xs_float;
    else if (strcmp(type, "!xs!double") == 0)             xtype = xs_double;
    else if (strcmp(type, "!xs!anyURI") == 0)             xtype = xs_anyURI;
    else if (strcmp(type, "!xs!QName") == 0)              xtype = xs_QName;
    else if (strcmp(type, "!xs!NOTATION") == 0)           xtype = xs_NOTATION;
    else if (strcmp(type, "!xs!string") == 0)             xtype = xs_string;
    else if (strcmp(type, "!xs!decimal") == 0)            xtype = xs_decimal;
    else if (strcmp(type, "!xs!integer") == 0)            xtype = xs_integer;
    else if (strcmp(type, "!xs!nonPositiveInteger") == 0) xtype = xs_nonPositiveInteger;
    else if (strcmp(type, "!xs!negativeInteger") == 0)    xtype = xs_negativeInteger;
    else if (strcmp(type, "!xs!long") == 0)               xtype = xs_long;
    else if (strcmp(type, "!xs!int") == 0)                xtype = xs_int;
    else if (strcmp(type, "!xs!short") == 0)              xtype = xs_short;
    else if (strcmp(type, "!xs!byte") == 0)               xtype = xs_byte;
    else if (strcmp(type, "!xs!nonNegativeInteger") == 0) xtype = xs_nonNegativeInteger;
    else if (strcmp(type, "!xs!unsignedLong") == 0)       xtype = xs_unsignedLong;
    else if (strcmp(type, "!xs!unsignedInt") == 0)        xtype = xs_unsignedInt;
    else if (strcmp(type, "!xs!unsignedShort") == 0)      xtype = xs_unsignedShort;
    else if (strcmp(type, "!xs!unsignedByte") == 0)       xtype = xs_unsignedByte;
    else if (strcmp(type, "!xs!positiveInteger") == 0)    xtype = xs_positiveInteger;
    else if (strcmp(type, "!xs!normalizedString") == 0)   xtype = xs_normalizedString;
    else if (strcmp(type, "!xs!token") == 0)              xtype = xs_token;
    else if (strcmp(type, "!xs!language") == 0)           xtype = xs_language;
    else if (strcmp(type, "!xs!NMTOKEN") == 0)            xtype = xs_NMTOKEN;
    else if (strcmp(type, "!xs!Name") == 0)               xtype = xs_Name;
    else if (strcmp(type, "!xs!NCName") == 0)             xtype = xs_NCName;
    else if (strcmp(type, "!xs!ID") == 0)                 xtype = xs_ID;
    else if (strcmp(type, "!xs!IDREF") == 0)              xtype = xs_IDREF;
    else if (strcmp(type, "!xs!ENTITY") == 0)             xtype = xs_ENTITY;
    else if (strcmp(type, "!se!separator") == 0)          xtype = se_separator;
    else throw USER_EXCEPTION2(SE1004, "155");

    // !xs!anySimpleType --> xs_anySimpleType
    // also xs:anyAtomicType

    return xtype;
}

orb_modifier make_order_by_modifier(scheme_list *lst)
{
    if (   lst->size() != 2
        || lst->at(0).type != SCM_SYMBOL
        || lst->at(1).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "156");
        
    orb_modifier m;    
        
    string status = string(lst->at(0).internal.symb);
    
    if(status == "greatest") m.status = ORB_EMPTY_GREATEST;
    else if(status == "least") m.status = ORB_EMPTY_LEAST;
    else if(status == "default")
    	m.status = tr_globals::st_ct.empty_order == xq_empty_order_least ? ORB_EMPTY_LEAST : ORB_EMPTY_GREATEST;	
    else throw USER_EXCEPTION2(SE1004, "157");

    string order = string(lst->at(1).internal.symb);
    
    if(order == "ascending" || order == "default") m.order = ORB_ASCENDING;
    else if(order == "descending") m.order = ORB_DESCENDING;
    else throw USER_EXCEPTION2(SE1004, "158");
    
    return m;
}


PPOpIn make_pp_op(variable_context *cxt, scheme_list *lst)
{
    if (   lst->size() != 2
        || lst->at(0).type != SCM_NUMBER
        || lst->at(1).type != SCM_LIST
        || lst->at(1).internal.list->at(0).type != SCM_SYMBOL) 
        throw USER_EXCEPTION2(SE1004, "01");

    string op = string(lst->at(1).internal.list->at(0).internal.symb);
    int ts = atoi(lst->at(0).internal.num);

    lst = lst->at(1).internal.list;

    PPIterator *opit = NULL;

    if (op == "PPReturn")
    {
        if (   lst->size() < 5
            || lst->size() > 6
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
            || lst->at(4).type != SCM_NUMBER
           ) throw USER_EXCEPTION2(SE1004, "02");

        arr_of_var_dsc vars;
        scheme_list *_vars_ = lst->at(1).internal.list;
        for (int i = 0; i != _vars_->size(); i++)
        {
            if (_vars_->at(i).type != SCM_NUMBER)
                throw USER_EXCEPTION2(SE1004, "03");

            int var = atoi(_vars_->at(i).internal.num);
            vars.push_back(var);
        }

        var_dsc pos = atoi(lst->at(4).internal.num);

        if(lst->size() == 6)
        {
            if(lst->at(5).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "02.1");
   
            opit = new PPReturn(cxt,
                                vars, 
                                make_pp_op(cxt, lst->at(2).internal.list),
                                make_pp_op(cxt, lst->at(3).internal.list),
                                pos,
                                make_sequence_type(lst->at(5).internal.list));
 
        }
        else
        {
            opit = new PPReturn(cxt,
                                vars, 
                                make_pp_op(cxt, lst->at(2).internal.list),
                                make_pp_op(cxt, lst->at(3).internal.list),
                                pos);
        }
    }
    else if (op == "PPSelect")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "04");

        arr_of_var_dsc vars;
        scheme_list *_vars_ = lst->at(1).internal.list;
        for (int i = 0; i != _vars_->size(); i++)
        {
            if (_vars_->at(i).type != SCM_NUMBER)
                throw USER_EXCEPTION2(SE1004, "05");

            int var = atoi(_vars_->at(i).internal.num);
            vars.push_back(var);
        }

        opit = new PPSelect(cxt,
                            vars,
                            make_pp_op(cxt, lst->at(2).internal.list),
                            make_pp_op(cxt, lst->at(3).internal.list));
    }
    else if (op == "PPLet")
    {
        if (   lst->size() < 4
            || lst->size() > 5 
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "06");

        arr_of_var_dsc vars;
        scheme_list *_vars_ = lst->at(1).internal.list;
        for (int i = 0; i != _vars_->size(); i++)
        {
            if (_vars_->at(i).type != SCM_NUMBER)
                throw USER_EXCEPTION2(SE1004, "07");

            int var = atoi(_vars_->at(i).internal.num);
            vars.push_back(var);
        }
        
        if(lst->size() == 5)
        {
            if(lst->at(4).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "06.1");

            opit = new PPLet(cxt,
                             vars,
                             make_pp_op(cxt, lst->at(2).internal.list),
                             make_pp_op(cxt, lst->at(3).internal.list),
                             make_sequence_type(lst->at(4).internal.list));
 
        }
        else
        {
            opit = new PPLet(cxt,
                             vars,
                             make_pp_op(cxt, lst->at(2).internal.list),
                             make_pp_op(cxt, lst->at(3).internal.list));
        }
    }
    else if (op == "PPConst")
    {
        if (lst->size() != 3
           ) throw USER_EXCEPTION2(SE1004, "08");

        tuple_cell tc = make_const(lst->at(2), lst->at(1));

        opit = new PPConst(cxt, tc);
    }
    else if (op == "PPVariable")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_NUMBER
           ) throw USER_EXCEPTION2(SE1004, "10");

        int var = atoi(lst->at(1).internal.num);
        opit = new PPVariable(cxt, var);
    }
    else if (op == "PPSequence")
    {
        if (   lst->size() == 1
           ) throw USER_EXCEPTION2(SE1004, "11");

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "12");
        }

        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size(); i++)
        {
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }

        opit = new PPSequence(cxt, arr);
    }
    else if (op == "PPDmNodeKind")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "13");

        opit = new PPDmNodeKind(cxt, 
                                make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPDmNodeName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "14");

        opit = new PPDmNodeName(cxt, 
                                make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPDmStringValue")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "15");

        opit = new PPDmStringValue(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPDmTypedValue")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "16");

        opit = new PPDmTypedValue(cxt, 
                                  make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnEmpty")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "17");

        opit = new PPFnEmpty(cxt, 
                             make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnExists")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "18");

        opit = new PPFnExists(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPIf")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "19");

        opit = new PPIf(cxt,
                        make_pp_op(cxt, lst->at(1).internal.list),
                        make_pp_op(cxt, lst->at(2).internal.list),
                        make_pp_op(cxt, lst->at(3).internal.list));
    }
    else if (op == "PPNil")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "20");

        opit = new PPNil(cxt);
    }
    else if (op == "PPStore")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "21");

        opit = new PPStore(cxt, 
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPCalculate")
    {
        if (   lst->size() < 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "22");

        int i = 0;

        for (i = 2; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "23");
        }

        arr_of_PPOpIn *arr = new arr_of_PPOpIn;
        for (i = 2; i < lst->size(); i++)
        {
            arr->push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }

        CalcOp * tree = make_calc_op(arr, lst->at(1).internal.list);

        opit = new PPCalculate(cxt,
                               arr,
                               tree);
    }
    else if (op == "PPAxisChild")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "24");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisChild(cxt,
                               child,
                               nt_type,
                               nt_data);
    }
    else if (op == "PPAxisAttribute")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "25");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisAttribute(cxt,
                                   child,
                                   nt_type,
                                   nt_data);
    }
    else if (op == "PPAxisParent")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "26");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisParent(cxt,
                                child,
                                nt_type,
                                nt_data);
    }
    else if (op == "PPAxisSelf")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "27");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisSelf(cxt,
                              child,
                              nt_type,
                              nt_data);
    }
    else if (op == "PPAxisDescendant")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "28");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisDescendant(cxt,
                                    child,
                                    nt_type,
                                    nt_data);
    }
	 else if (op == "PPAxisAncestor")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "28");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisAncestor(cxt,
                                    child,
                                    nt_type,
                                    nt_data);
    }
    else if (op == "PPAxisDescendantOrSelf")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "29");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisDescendantOrSelf(cxt,
                                          child,
                                          nt_type,
                                          nt_data);
    }
	else if (op == "PPAxisAncestorOrSelf")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "29");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisAncestorOrSelf(cxt,
                                          child,
                                          nt_type,
                                          nt_data);
    }
	else if (op == "PPAxisFollowing")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "29");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisFP(cxt,
                                          child,
                                          nt_type,
                                          nt_data,true);
    }
	else if (op == "PPAxisFollowingSibling")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "29");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisSibling(cxt,
                                          child,
                                          nt_type,
                                          nt_data,true);
    }
	else if (op == "PPAxisPrecedingSibling")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "29");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisSibling(cxt,
                                          child,
                                          nt_type,
                                          nt_data,false);
    }
	else if (op == "PPAxisPreceding")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "29");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisFP(cxt,
                                          child,
                                          nt_type,
                                          nt_data,false);
    }
    else if (op == "PPAxisDescendantAttr")
    {
        if (   lst->size() != 4)
            throw USER_EXCEPTION2(SE1004, "30");

        PPOpIn child;
        NodeTestType nt_type;
        NodeTestData nt_data;

        set_axis_parameters(lst, cxt, child, nt_type, nt_data, false);
        
        opit = new PPAxisDescendantAttr(cxt,
                                        child,
                                        nt_type,
                                        nt_data);
    }
    else if (op == "PPElement")
    {
        if (   lst->size() != 5
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
			|| lst->at(3).type != SCM_BOOL
			|| lst->at(4).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "31");

        scheme_list *name_lst = lst->at(1).internal.list;

        if (   name_lst->size() == 2 
            && name_lst->at(0).type == SCM_STRING
            && name_lst->at(1).type == SCM_STRING)
        {
            string name = string(name_lst->at(0).internal.str) + ":" + string(name_lst->at(1).internal.str);

            opit = new PPElementConstructor(cxt,
                                            name.c_str(),
                                            make_pp_op(cxt, lst->at(2).internal.list)
											,lst->at(3).internal.b,lst->at(4).internal.b);
        }
        else
        {
            opit = new PPElementConstructor(cxt,
                                            make_pp_op(cxt, lst->at(1).internal.list),
                                            make_pp_op(cxt, lst->at(2).internal.list)
											,lst->at(3).internal.b,lst->at(4).internal.b);
        }
    }
    else if (op == "PPAttribute")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
			|| lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "32");

        scheme_list *name_lst = lst->at(1).internal.list;

        if (   name_lst->size() == 2 
            && name_lst->at(0).type == SCM_STRING
            && name_lst->at(1).type == SCM_STRING)
        {
            string name = string(name_lst->at(0).internal.str) + ":" + string(name_lst->at(1).internal.str);

            opit = new PPAttributeConstructor(cxt,
                                              name.c_str(),
                                              make_pp_op(cxt, lst->at(2).internal.list),lst->at(3).internal.b);
        }
        else
        {
            opit = new PPAttributeConstructor(cxt,
                                              make_pp_op(cxt, lst->at(1).internal.list),
                                              make_pp_op(cxt, lst->at(2).internal.list),lst->at(3).internal.b);
        }
    }
    else if (op == "PPPI")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
			|| lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "32.1");

        scheme_list *pi_target_lst = lst->at(1).internal.list;

        if (   pi_target_lst->size() == 2
            && pi_target_lst->at(0).type == SCM_STRING
            && pi_target_lst->at(1).type == SCM_STRING)
        {
            string target = string(pi_target_lst->at(0).internal.str) + ":"
                            + string(pi_target_lst->at(1).internal.str);

            opit = new PPPIConstructor( cxt,
                                        target.c_str(),
                                        make_pp_op(cxt, lst->at(2).internal.list),
                                        lst->at(3).internal.b);
        }
        else
        {
            opit = new PPPIConstructor( cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list),
                                        make_pp_op(cxt, lst->at(2).internal.list),
                                        lst->at(3).internal.b);
        }
    }
	else if (op == "PPComment")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "32.2");

        opit = new PPCommentConstructor(cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list),
                                        lst->at(2).internal.b);
    }
	else if (op == "PPText")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "32.7");

        opit = new PPTextConstructor(cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list),
                                        lst->at(2).internal.b);
    }
	else if (op == "PPDocument")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST            
           ) throw USER_EXCEPTION2(SE1004, "32.7");

        opit = new PPDocumentConstructor(cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list) );
    }
    else if (op == "PPFnNot")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "33");

        opit = new PPFnNot(cxt, 
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnBoolean")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "33.1");

        opit = new PPFnBoolean(cxt, 
                               make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnTrue")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "34");

        opit = new PPFnTrue(cxt);
    }
    else if (op == "PPFnFalse")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "35");

        opit = new PPFnFalse(cxt);
    }
    else if (op == "PPAbsPath")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "36");

        PathExpr *path_expr = lr2PathExpr(cxt, lst->at(2).internal.list, false);

        scheme_list *ent_lst = lst->at(1).internal.list;
        db_entity *db_ent = make_db_entity(ent_lst, false);

        if (ent_lst->at(1).type == SCM_STRING)
        {
            opit = new PPAbsPath(cxt, 
                                 path_expr, 
                                 counted_ptr<db_entity>(db_ent));
        }
        else if (ent_lst->at(1).type == SCM_LIST)
        {
            opit = new PPAbsPath(cxt, 
                                 path_expr, 
                                 counted_ptr<db_entity>(db_ent),
                                 make_pp_op(cxt, ent_lst->at(1).internal.list));
        }
        else throw USER_EXCEPTION2(SE1004, "41");
    }
    else if (op == "PPFnCount")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "42");

        opit = new PPFnCount(cxt,
                             make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnSum")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "43");

        opit = new PPFnSum(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnAvg")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "44");

        opit = new PPFnAvg(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnMax")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "45");

        opit = new PPFnMax(cxt, 
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnMin")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46");

        opit = new PPFnMin(cxt, 
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnEncodeForUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.1");

        opit = new PPFnUriEncoding(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   PPFnUriEncoding::ENCODE_FOR_URI);
    }
    else if (op == "PPFnIriToUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.2");

        opit = new PPFnUriEncoding(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   PPFnUriEncoding::IRI_TO_URI);
    }
    else if (op == "PPFnEscapeHtmlUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.3");

        opit = new PPFnUriEncoding(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   PPFnUriEncoding::ESCAPE_HTML_URI);
    }
    else if (op == "PPFnResolveUri")
    {
        if (   lst->size() < 2
            || lst->size() > 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.4");

        if(lst->size() == 3)
        {
            if(lst->at(2).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "46.5");
            opit = new PPFnResolveUri(cxt, 
                                      make_pp_op(cxt, lst->at(1).internal.list),
                                      make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = new PPFnResolveUri(cxt, 
                                      make_pp_op(cxt, lst->at(1).internal.list));
        
    }
    else if (op == "PPFnItemAt")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "47");

        opit = new PPFnItemAt(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnOneOrMore")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-8");

        opit = new PPFnOneOrMore(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list));
                                 
    }
    else if (op == "PPFnExactlyOne")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-7");

        opit = new PPFnExactlyOne(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list));
                                 
    }
    else if (op == "PPFnZeroOrOne")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-6");

        opit = new PPFnZeroOrOne(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list));
                                 
    }
    else if (op == "PPFnInsertBefore")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-5");

        opit = new PPFnInsertBefore(cxt,
                                    make_pp_op(cxt, lst->at(1).internal.list),
                                    make_pp_op(cxt, lst->at(2).internal.list),
                                    make_pp_op(cxt, lst->at(3).internal.list));
    }
    else if (op == "PPFnRemove")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-4");

        opit = new PPFnRemove(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnDistinctValues")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-3");

        opit = new PPFnDistinctValues(cxt,
                                      make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnReverse")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-2");

        opit = new PPFnReverse(cxt,
                               make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnSubsequence")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-1");

        if(lst->size() == 4)
        {
        	if(lst->at(3).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "48.0");
            opit = new PPFnSubsequence(cxt,
                                       make_pp_op(cxt, lst->at(1).internal.list),
                                       make_pp_op(cxt, lst->at(2).internal.list),
                                       make_pp_op(cxt, lst->at(3).internal.list));
        }
        else
            opit = new PPFnSubsequence(cxt,
                                       make_pp_op(cxt, lst->at(1).internal.list),
                                       make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPTypeswitch")
    {
    	if (   lst->size() != 6
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
            || lst->at(4).type != SCM_LIST
            || lst->at(5).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.1");

        int i = 0;
       
        arr_of_var_dsc vars;
        scheme_list *_vars_ = lst->at(1).internal.list;
        for (i = 0; i != _vars_->size(); i++)
        {
            if (_vars_->at(i).type != SCM_NUMBER)
                throw USER_EXCEPTION2(SE1004, "48.2");

            var_dsc var = atoi(_vars_->at(i).internal.num);
            vars.push_back(var);
        }

        arr_of_sequence_type _types_;
        scheme_list *types_list = lst->at(3).internal.list;
        for (i = 0; i < types_list->size(); i++)
        {
        	if (types_list->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "48.3");

            _types_.push_back(make_sequence_type(types_list->at(i).internal.list));
        }
                
        arr_of_PPOpIn _cases_;
        scheme_list *cases_list = lst->at(4).internal.list;
        for (i = 0; i < cases_list->size(); i++)
        {
            if (cases_list->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "48.4");

            _cases_.push_back(make_pp_op(cxt, cases_list->at(i).internal.list));
        }

        opit = new PPTypeswitch(cxt,
        						vars,
        						make_pp_op(cxt, lst->at(2).internal.list),
        						_types_,
        						_cases_,
								make_pp_op(cxt, lst->at(5).internal.list));
    }
    else if (op == "PPCast")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_SYMBOL
            || lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "49");

        xmlscm_type target_type = lr_atomic_type2xmlscm_type(lst->at(2).internal.symb);

        opit = new PPCast(cxt,
                          make_pp_op(cxt, lst->at(1).internal.list),
                          target_type,
                          lst->at(3).internal.b);
    }
    else if (op == "PPCastable")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_SYMBOL
            || lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "50");

        xmlscm_type target_type = lr_atomic_type2xmlscm_type(lst->at(2).internal.symb);

        opit = new PPCastable(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              target_type,
                              lst->at(3).internal.b);
    }
    else if (op == "PPInstanceOf")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "69");

        opit = new PPInstanceOf(cxt,
                                make_pp_op(cxt, lst->at(1).internal.list),
                                make_sequence_type(lst->at(2).internal.list));
    }
    else if (op == "PPTreat")
    {
    	if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "69.1");

        opit = new PPTreat(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list),
                           make_sequence_type(lst->at(2).internal.list));
    }
    else if (op == "PPGeneralCompGT")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "51");

        opit = PPGeneralComparison::PPGTGeneralComparison(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPGeneralCompLT")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "52");

        opit = PPGeneralComparison::PPLTGeneralComparison(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPGeneralCompGE")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "53");

        opit = PPGeneralComparison::PPGEGeneralComparison(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPGeneralCompLE")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "54");

        opit = PPGeneralComparison::PPLEGeneralComparison(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPGeneralCompNE")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "55");

        opit = PPGeneralComparison::PPNEGeneralComparison(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPGeneralCompEQ")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "56");

        opit = PPGeneralComparison::PPEQGeneralComparison(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPDDO")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "57");

        opit = new PPDDO(cxt,
                         make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFEL")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "157");

        opit = new PPFilterEL(cxt,
                         make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnContains")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "60");
       //TEMPORALLY CHANGED
        opit = PPSubsMatch::PPFnContains(cxt,
                                         make_pp_op(cxt, lst->at(1).internal.list),
            		make_pp_op(cxt, lst->at(2).internal.list));
		//opit = PPPatMatch::PPFnMatch(cxt,
		//	make_pp_op(cxt, lst->at(1).internal.list),
		//	make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPIndexScan")
    {
        if (   lst->size() != 5
            || lst->at(1).type != SCM_STRING
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
            || lst->at(4).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "61");

        string index_name = string(lst->at(1).internal.str);

        string isc_string = string(lst->at(4).internal.symb);
        index_scan_condition isc;
        if (isc_string == "isc_eq")			isc = isc_eq;
        else if (isc_string == "isc_lt")	isc = isc_lt;
        else if (isc_string == "isc_le")	isc = isc_le;
        else if (isc_string == "isc_gt")	isc = isc_gt;
        else if (isc_string == "isc_ge")	isc = isc_ge;
        else if (isc_string == "isc_gt_lt")	isc = isc_gt_lt;
        else if (isc_string == "isc_gt_le")	isc = isc_gt_le;
        else if (isc_string == "isc_ge_lt")	isc = isc_ge_lt;
        else if (isc_string == "isc_ge_le")	isc = isc_ge_le;
        else throw USER_EXCEPTION2(SE1004, "62");

        scheme_list *p1 = lst->at(2).internal.list;
        scheme_list *p2 = lst->at(3).internal.list;

        if (   p1->size() != 2
            || p1->at(0).type != SCM_NUMBER
            || p1->at(1).type != SCM_LIST
            || p1->at(1).internal.list->at(0).type != SCM_SYMBOL 
            || p2->size() != 2
            || p2->at(0).type != SCM_NUMBER
            || p2->at(1).type != SCM_LIST
            || p2->at(1).internal.list->at(0).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "63");

        string op1 = string(p1->at(1).internal.list->at(0).internal.symb);
        string op2 = string(p2->at(1).internal.list->at(0).internal.symb);

        if (op1 == "PPConst" && op2 == "PPConst")
        {
            tuple_cell tc = make_const(p1->at(1).internal.list->at(2), p1->at(1).internal.list->at(1));
            tuple_cell tc2 = make_const(p2->at(1).internal.list->at(2), p2->at(1).internal.list->at(1));

            opit = new PPIndexScan(cxt,
                                   index_name,
                                   tc,
                                   tc2,
                                   isc); 
        }
        else if (op1 == "PPConst")
        {
            tuple_cell tc = make_const(p1->at(1).internal.list->at(2), p1->at(1).internal.list->at(1));

            opit = new PPIndexScan(cxt,
                                   index_name,
                                   tc,
                                   make_pp_op(cxt, p2),
                                   isc); 
        }
        else if (op2 == "PPConst")
        {
            tuple_cell tc2 = make_const(p2->at(1).internal.list->at(2), p2->at(1).internal.list->at(1));

            opit = new PPIndexScan(cxt,
                                   index_name,
                                   make_pp_op(cxt, p1),
                                   tc2,
                                   isc); 
        }
        else
        {
            opit = new PPIndexScan(cxt,
                                   index_name,
                                   make_pp_op(cxt, p1),
                                   make_pp_op(cxt, p2),
                                   isc); 
        }
    }
    else if (op == "PPEQNodeComparison")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "66");

        opit = PPNodeComparison::PPEQNodeComparison(cxt,
                                                    make_pp_op(cxt, lst->at(1).internal.list),
                                                    make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPLTNodeComparison")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "67");

        opit = PPNodeComparison::PPLTNodeComparison(cxt,
                                                    make_pp_op(cxt, lst->at(1).internal.list),
                                                    make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPANNodeComparison")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "158");

        opit = PPNodeComparison::PPANNodeComparison(cxt,
                                                    make_pp_op(cxt, lst->at(1).internal.list),
                                                    make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPGTNodeComparison")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "68");

        opit = PPNodeComparison::PPGTNodeComparison(cxt,
                                                    make_pp_op(cxt, lst->at(1).internal.list),
                                                    make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFunCall")
    {
        if (   lst->size() < 2
            || lst->at(1).type != SCM_NUMBER
           ) throw USER_EXCEPTION2(SE1004, "70");

        int i = 0;

        for (i = 2; i < lst->size(); i++)
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "71");

        int fn_id = atoi(lst->at(1).internal.num);

        arr_of_PPOpIn ch_arr;
        for (i = 2; i < lst->size(); i++)
            ch_arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));

        opit = new PPFunCall(cxt,
                             ch_arr,
                             fn_id);
    }
    else if (op == "PPExtFunCall")
    {
        if (   lst->size() < 2
			|| lst->at(1).type != SCM_STRING
           ) throw USER_EXCEPTION2(SE1004, "PPExtFunCall-1");

		char *name = lst->at(1).internal.str;

        int i = 0;
        for (i = 2; i < lst->size(); i++)
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "PPExtFunCall-2");

        arr_of_PPOpIn ch_arr;
        for (i = 2; i < lst->size(); i++)
            ch_arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));

		opit = ext_function_manager.make_pp_ext_func(name, cxt, ch_arr);
    }
	else if (op == "PPNamespace")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_STRING
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "72");

        opit = new PPNamespaceConstructor(cxt,
                                          lst->at(1).internal.str,
                                          make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = new PPFnName(cxt, 
                            make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnNumber")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = new PPFnNumber(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnAbs" || op == "PPFnCeiling" || op == "PPFnFloor" || op == "PPFnRound")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        PPNumericFuncs::value_func func = NULL;
        if (op == "PPFnAbs") func = &PPNumericFuncs::fn_abs;
        else if (op == "PPFnCeiling") func = &PPNumericFuncs::fn_ceiling;
        else if (op == "PPFnFloor") func = &PPNumericFuncs::fn_floor;
        else if (op == "PPFnRound") func = &PPNumericFuncs::fn_round;
        else throw USER_EXCEPTION2(SE1004, "73");

        opit = new PPNumericFuncs(cxt, 
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  func);
    }
    else if (op == "PPSpaceSequence")
    {
        if (   lst->size() == 1
           ) throw USER_EXCEPTION2(SE1004, "74");

        int i = 0;

        for (i = 1; i < lst->size()-1; i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "75");
        }
		if (lst->at(lst->size()-1).type != SCM_BOOL)
			throw USER_EXCEPTION2(SE1004, "751");
        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size()-1; i++)
        {
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }
		//bool tmp=false;
        opit = new PPSpaceSequence(cxt, arr,lst->at(lst->size()-1).internal.b);
    }


    /////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////
    if (opit) return PPOpIn(opit, ts);
    /////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////


    if (op == "PPFnError")
    {
        if (   lst->size() < 1 
            || lst->size() > 4
           ) throw USER_EXCEPTION2(SE1004, "76");

        PPOpIn child_err, child_descr, child_obj;
        if (lst->size() == 4)
        {
            if (lst->at(3).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "76");
            child_obj = make_pp_op(cxt, lst->at(3).internal.list);
        }

        if (lst->size() >= 3)
        {
            if (lst->at(2).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "76");
            child_descr = make_pp_op(cxt, lst->at(2).internal.list);
        }

        if (lst->size() >= 2)
        {
            if (lst->at(1).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "76");
            child_err = make_pp_op(cxt, lst->at(1).internal.list);
        }

        opit = new PPFnError(cxt, 
                             child_err, child_descr, child_obj);
    }
	else if (op == "PPFnTrace")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "76");

		opit = new PPFnTrace(cxt,
                             make_pp_op(cxt, lst->at(1).internal.list),
                             make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPTest")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "77");

        opit = new PPTest(cxt, 
                          make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPPatMatch")
    {
        if (   lst->size() < 4
            || lst->size() > 6
            || lst->at(1).type != SCM_SYMBOL
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "78");

        __int16 pm = (__int16)0;

        string pm_str = string(lst->at(1).internal.symb);
        if (pm_str == "pm_replace") pm = pm_replace;
        else if (pm_str == "pm_matches") pm = pm_match;
        else throw USER_EXCEPTION2(SE1004, "79");


        if (lst->size() == 4)
        {
            opit = new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
		                          pm);
        }
        else if (lst->size() == 5)
        {
            opit = new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
                                  make_pp_op(cxt, lst->at(4).internal.list),
		                          pm);
        }
        else if (lst->size() == 6)
        {
            opit = new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
                                  make_pp_op(cxt, lst->at(4).internal.list),
                                  make_pp_op(cxt, lst->at(5).internal.list),
		                          pm);
        }
        else throw USER_EXCEPTION2(SE1004, "80");
    }
    else if (op == "PPDocInCol")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "81");

        opit = new PPDocInCol(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPTuple")
    {
        if (   lst->size() == 1
           ) throw USER_EXCEPTION2(SE1004, "82");

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "83");
        }

        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size(); i++)
        {
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }

        opit = new PPTuple(cxt, arr);
    }
    else if (op == "PPPred1" || op == "PPPred2")	//PPPred1 and PPPred2 have much in common; hence, they are joined in one "if"
    {
        if(op == "PPPred1")
        {
        	if (   lst->size() < 6
            	|| lst->size() > 7
            	|| lst->at(1).type != SCM_LIST
            	|| lst->at(2).type != SCM_LIST
            	|| lst->at(3).type != SCM_LIST
            	|| lst->at(4).type != SCM_LIST
            	|| lst->at(5).type != SCM_NUMBER
				) throw USER_EXCEPTION2(SE1004, "84");
        }
        else
        {
			if (   lst->size() < 7 
           		|| lst->size() > 8
           		|| lst->at(1).type != SCM_LIST
            	|| lst->at(2).type != SCM_LIST
            	|| lst->at(3).type != SCM_LIST
            	|| lst->at(4).type != SCM_LIST
            	|| lst->at(5).type != SCM_NUMBER
            	|| lst->at(6).type != SCM_NUMBER
           		) throw USER_EXCEPTION2(SE1004, "85");
        }

        int i = 0;

        arr_of_var_dsc vars;
        scheme_list *_vars_ = lst->at(1).internal.list;
        for (i = 0; i != _vars_->size(); i++)
        {
            if (_vars_->at(i).type != SCM_NUMBER)
                throw USER_EXCEPTION2(SE1004, "86");

            var_dsc var = atoi(_vars_->at(i).internal.num);
            vars.push_back(var);
        }

        arr_of_comp_cond _conditions_;
        arr_of_PPOpIn _conjuncts_;
        scheme_list *conjuncts_list = lst->at(3).internal.list;
        for (i = 0; i < conjuncts_list->size(); i++)
        {
            if (conjuncts_list->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "86.1");
            scheme_list *conjunct = conjuncts_list->at(i).internal.list;
            
            if (   conjunct->size() != 2 
                || conjunct->at(0).type != SCM_SYMBOL 
                || conjunct->at(1).type != SCM_LIST
               )
                throw USER_EXCEPTION2(SE1004, "86.2");

            string occ_string = string(conjunct->at(0).internal.symb);
        	operation_compare_condition occ;
	        if (occ_string == "eqv")            occ = OCC_VALUE_EQUAL;
    	    else if (occ_string == "nev")	    occ = OCC_VALUE_NOT_EQUAL;
	        else if (occ_string == "lev")	    occ = OCC_VALUE_LESS_EQUAL;
    	    else if (occ_string == "gtv")	    occ = OCC_VALUE_GREATER;
	        else if (occ_string == "gev")	    occ = OCC_VALUE_GREATER_EQUAL;
    	    else if (occ_string == "ltv")	    occ = OCC_VALUE_LESS;
      	    else if (occ_string == "eqg")       occ = OCC_GENERAL_EQUAL;
    	    else if (occ_string == "neg")	    occ = OCC_GENERAL_NOT_EQUAL;
	        else if (occ_string == "leg")	    occ = OCC_GENERAL_LESS_EQUAL;
    	    else if (occ_string == "gtg")	    occ = OCC_GENERAL_GREATER;
	        else if (occ_string == "geg")	    occ = OCC_GENERAL_GREATER_EQUAL;
    	    else if (occ_string == "ltg")	    occ = OCC_GENERAL_LESS;
	        else throw USER_EXCEPTION2(SE1004, "86.3");
    
            _conditions_.push_back(occ);
            _conjuncts_.push_back(make_pp_op(cxt, conjunct->at(1).internal.list));
        }


        bool _once_ = (atoi(lst->at(5).internal.num) == 1);
        
        if(op == "PPPred1")
        {
        	if (lst->size() == 7)
	        {
    	        if (lst->at(6).type != SCM_NUMBER)
        	        throw USER_EXCEPTION2(SE1004, "87");
            	var_dsc pos = atoi(lst->at(6).internal.num);
	
    	        opit = new PPPred1(cxt,
        	                       vars,
            	                   make_pp_op(cxt, lst->at(2).internal.list),
                	               _conjuncts_,
                    	           _conditions_,
                        	       make_pp_op(cxt, lst->at(4).internal.list),
                            	   _once_,
	                               pos);
    	    }
	        else
    	        opit = new PPPred1(cxt,
        	                       vars,
            	                   make_pp_op(cxt, lst->at(2).internal.list),
                	               _conjuncts_,
                    	           _conditions_,
                        	       make_pp_op(cxt, lst->at(4).internal.list),
                            	   _once_);
        }
        else
        {
        	var_dsc last = atoi(lst->at(6).internal.num);

        	if (lst->size() == 8)
        	{
            	if (lst->at(7).type != SCM_NUMBER)
                	throw USER_EXCEPTION2(SE1004, "88");
	            var_dsc pos = atoi(lst->at(7).internal.num);
		
        		opit = new PPPred2(cxt,
            		               vars,
               			           make_pp_op(cxt, lst->at(2).internal.list),
               	    		       _conjuncts_,
                        		   _conditions_,
                           		   make_pp_op(cxt, lst->at(4).internal.list),
	                           	   _once_,
    	                           last,
        	                       pos);
	        }
    	    else
        		opit = new PPPred2(cxt,
            	               vars,
                	           make_pp_op(cxt, lst->at(2).internal.list),
                    	       _conjuncts_,
                        	   _conditions_,
	                           make_pp_op(cxt, lst->at(4).internal.list),
    	                       _once_,
        	                   last);
    	}
    }
    else if (op == "PPUnion")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "89");

        opit = new PPUnion(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list),
                           make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPIntersect")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "90");

        opit = new PPIntersect(cxt,
                               make_pp_op(cxt, lst->at(1).internal.list),
                               make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPExcept")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "91");

        opit = new PPExcept(cxt,
                            make_pp_op(cxt, lst->at(1).internal.list),
                            make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPADFilter")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "92");

        opit = new PPADFilter(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPDAFilter")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "93");

        opit = new PPDAFilter(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPScan")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_NUMBER
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "94");

        schema_node *scm_node = (schema_node*)(atoi(lst->at(1).internal.num));

        scheme_list *ent_lst = lst->at(2).internal.list;
        db_entity *db_ent = make_db_entity(ent_lst, false);

        opit = new PPScan(cxt,
                          scm_node,
                          counted_ptr<db_entity>(db_ent));
    }
    else if (op == "PPUp")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_NUMBER
           ) throw USER_EXCEPTION2(SE1004, "95");

        schema_node *scm_node = (schema_node*)(atoi(lst->at(2).internal.num));

        opit = new PPUp(cxt,
                        make_pp_op(cxt, lst->at(1).internal.list),
                        scm_node);
    }
    else if (op == "PPFnConcat")
    {
        if (   lst->size() < 3
           ) throw USER_EXCEPTION2(SE1004, "96");

        int i = 0;
        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "97");
        }

        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size(); i++)
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));

        opit = new PPFnConcat(cxt, 
                              arr);
    }
    else if (op == "PPFnStringLength")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "98");

        opit = new PPFnStringLength(cxt,
                                    make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnTranslate")
	{
		if (   lst->size() != 4
			|| lst->at(1).type != SCM_LIST
			) throw USER_EXCEPTION2(SE1004, "98.5");

		opit = new PPFnTranslate(cxt,
			make_pp_op(cxt, lst->at(1).internal.list),
			make_pp_op(cxt, lst->at(2).internal.list),
			make_pp_op(cxt, lst->at(3).internal.list));
	}
    else if (op == "PPFnString")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "99");

        opit = new PPFnString(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnData")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "100");

        opit = new PPFnData(cxt,
                            make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnDocumentURI")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "101");

        opit = new PPFnDocumentURI(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPRange")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "102");

        opit = new PPRange(cxt, 
                           make_pp_op(cxt, lst->at(1).internal.list),
                           make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPXptr")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_SYMBOL
           ) throw USER_EXCEPTION2(SE1004, "103");

        trigger_parameter_type var_type;
        if (strcmp(lst->at(1).internal.symb, "NEW") == 0) var_type = TRIGGER_PARAMETER_NEW;
        else if (strcmp(lst->at(1).internal.symb, "OLD") == 0) var_type = TRIGGER_PARAMETER_OLD;
        else if (strcmp(lst->at(1).internal.symb, "WHERE") == 0) var_type = TRIGGER_PARAMETER_WHERE;
        else throw USER_EXCEPTION2(SE1004, "104");

        PPXptr *pp_xptr = new PPXptr(cxt, 
                                     var_type);
#ifdef SE_ENABLE_TRIGGERS
        if (qep_parameters) qep_parameters->push_back(pp_xptr);
#endif
        opit = pp_xptr;
    }
    else if (op == "PPCheckpoint")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "103");

        opit = new PPCheckpoint(cxt);
    }

    // Date-time functions with no arguments

    else if (op == "PPFnCurrentDateTime" ||
		op == "PPFnCurrentDate" ||
		op == "PPFnCurrentTime" ||
		op == "PPFnImplicitTimezone")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "103");

        int type;

        if (op == "PPFnCurrentDateTime") type = PPFnDateTimeFuncNoParam::currentDateTime;
        else if (op == "PPFnCurrentDate") type = PPFnDateTimeFuncNoParam::currentDate;
        else if (op == "PPFnCurrentTime") type = PPFnDateTimeFuncNoParam::currentTime;
        else if (op == "PPFnImplicitTimezone") type = PPFnDateTimeFuncNoParam::implicitTimezone;
        else throw USER_EXCEPTION2(SE1004, "103");

        opit = new PPFnDateTimeFuncNoParam(cxt, type);
    }

    // Date-time functions with at most one argument

    else if ( op == "PPFnYearsFromDuration" ||
                op == "PPFnMonthsFromDuration" ||
                op == "PPFnDaysFromDuration" ||
                op == "PPFnHoursFromDuration" ||
                op == "PPFnMinutesFromDuration" ||
                op == "PPFnSecondsFromDuration" ||
                op == "PPFnYearFromDateTime" ||
                op == "PPFnMonthFromDateTime" ||
                op == "PPFnDayFromDateTime" ||
                op == "PPFnHoursFromDateTime" ||
                op == "PPFnMinutesFromDateTime" ||
                op == "PPFnSecondsFromDateTime" ||
                op == "PPFnTimezoneFromDateTime" ||
                op == "PPFnYearFromDate" ||
                op == "PPFnMonthFromDate" ||
                op == "PPFnDayFromDate" ||
                op == "PPFnTimezoneFromDate" ||
                op == "PPFnHoursFromTime" ||
                op == "PPFnMinutesFromTime" ||
                op == "PPFnSecondsFromTime" ||
                op == "PPFnTimezoneFromTime")
    {
        int type;

        if (lst->size() == 2 &&
                lst->at(1).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "103");

        if (op == "PPFnYearsFromDuration") 	{ type = PPFnDateTimeFunc::yearsFromDuration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMonthsFromDuration")	{ type = PPFnDateTimeFunc::monthsFromDuration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnDaysFromDuration")	{ type = PPFnDateTimeFunc::daysFromDuration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnHoursFromDuration")	{ type = PPFnDateTimeFunc::hoursFromDuration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMinutesFromDuration")	{ type = PPFnDateTimeFunc::minutesFromDuration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnSecondsFromDuration")	{ type = PPFnDateTimeFunc::secondsFromDuration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnYearFromDateTime")	{ type = PPFnDateTimeFunc::yearFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMonthFromDateTime")	{ type = PPFnDateTimeFunc::monthFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnDayFromDateTime")	{ type = PPFnDateTimeFunc::dayFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnHoursFromDateTime")	{ type = PPFnDateTimeFunc::hoursFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMinutesFromDateTime")	{ type = PPFnDateTimeFunc::minutesFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnSecondsFromDateTime")	{ type = PPFnDateTimeFunc::secondsFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromDateTime")	{ type = PPFnDateTimeFunc::timezoneFromDateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnYearFromDate")		{ type = PPFnDateTimeFunc::yearFromDate; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMonthFromDate")		{ type = PPFnDateTimeFunc::monthFromDate; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnDayFromDate")		{ type = PPFnDateTimeFunc::dayFromDate; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromDate")	{ type = PPFnDateTimeFunc::timezoneFromDate; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnHoursFromTime")		{ type = PPFnDateTimeFunc::hoursFromTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMinutesFromTime")	{ type = PPFnDateTimeFunc::minutesFromTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnSecondsFromTime")	{ type = PPFnDateTimeFunc::secondsFromTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromTime")	{ type = PPFnDateTimeFunc::timezoneFromTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromTime")	{ type = PPFnDateTimeFunc::timezoneFromTime; goto fn_dt_funcs_correct_type; }

	throw USER_EXCEPTION2(SE1004, "invalid date time operator");

fn_dt_funcs_correct_type:

        opit = new PPFnDateTimeFunc(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list), type);
    }

    // Date-time functions with at most 2 operands

    else if ( op == "PPFnAdjustDateTimeToTimezone" ||
                op == "PPFnAdjustDateToTimezone" ||
                op == "PPFnAdjustTimeToTimezone" ||
		op == "PPFnDateTime" )
    {
        int type;

        if (lst->size() == 2 &&
                lst->at(1).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "103");
        if (   (lst->size() == 3 && (
                lst->at(1).type != SCM_LIST ||
                lst->at(2).type != SCM_LIST )))
            throw USER_EXCEPTION2(SE1004, "103");
        if (lst->size() != 2 && lst->size() != 3)
            throw USER_EXCEPTION2(SE1004, "103");

	if (lst->size() == 3)
 	{
        	if (op == "PPFnAdjustDateTimeToTimezone") type = PPFnDateTimeFunc2Params::adjustDateTimeToTimezone;
		else if (op == "PPFnAdjustDateToTimezone" ) type = PPFnDateTimeFunc2Params::adjustDateToTimezone;
        	else if (op == "PPFnAdjustTimeToTimezone") type = PPFnDateTimeFunc2Params::adjustTimeToTimezone;
        	else if (op == "PPFnDateTime") type = PPFnDateTimeFunc2Params::dateTime;
		else throw USER_EXCEPTION2(SE1004, "Invalid date time function");

                opit = new PPFnDateTimeFunc2Params(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list),
                           make_pp_op(cxt, lst->at(2).internal.list), type);
	}
	else
	{
        	if (op == "PPFnAdjustDateTimeToTimezone") type = PPFnDateTimeFunc::adjustDateTimeToTimezone;
		else if (op == "PPFnAdjustDateToTimezone" ) type = PPFnDateTimeFunc::adjustDateToTimezone;
        	else if (op == "PPFnAdjustTimeToTimezone") type = PPFnDateTimeFunc::adjustTimeToTimezone;
		else throw USER_EXCEPTION2(SE1004, "Invalid date time function");
	
                opit = new PPFnDateTimeFunc(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list), type); 
	}
    }

    else if (op == "PPOrderBy")
    {
    	if (   lst->size() != 4
            || lst->at(1).type != SCM_BOOL
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST 
           ) throw USER_EXCEPTION2(SE1004, "104");
        
		scheme_list *modifiers_list = lst->at(3).internal.list;
		arr_of_orb_modifier _modifiers_;

		for (int i = 0; i < modifiers_list->size(); i++)
        {
            if (modifiers_list->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "105");
            scheme_list *modifier = modifiers_list->at(i).internal.list;
            orb_modifier om = make_order_by_modifier(modifier);
            _modifiers_.push_back(om);
		}	        

		opit = new PPOrderBy(cxt,							
							 lst->at(1).internal.b,    	
							 make_pp_op(cxt, lst->at(2).internal.list), 
		                     _modifiers_,
							 ts);
    }
	else if (op == "PPFnDeepEqual")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "107");

		opit = new PPFnDeepEqual(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list),
                                 make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPFnQName")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "108");

		opit = new PPFnQName(cxt,
                             make_pp_op(cxt, lst->at(1).internal.list),
                             make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPFnPrefixFromQName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "109");

		opit = new PPFnPrefixFromQName(cxt,
                                       make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnLocalNameFromQName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "110");

		opit = new PPFnLocalNameFromQName(cxt,
                                          make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnNamespaceUriFromQName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "111");

		opit = new PPFnNamespaceUriFromQName(cxt,
                                             make_pp_op(cxt, lst->at(1).internal.list));
    }



#ifdef SQL_CONNECTION
    else if (op == "PPFnSQLConnect")
    {
        if (   lst->size() < 2
           ) throw USER_EXCEPTION(SE1004);

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION(SE1004);
        }

        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size(); i++)
        {
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }


		opit = new PPFnSQLConnect(cxt, arr);
    }

    else if (op == "PPFnSQLPrepare")
    {
		if (   lst->size() < 3 || lst->size() > 4
           ) throw USER_EXCEPTION(SE1004);

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION(SE1004);
        }

		if (lst->size() == 3)
			opit = new PPFnSQLPrepare(cxt,
							make_pp_op(cxt, lst->at(1).internal.list),
							make_pp_op(cxt, lst->at(2).internal.list));
		else
			opit = new PPFnSQLPrepare(cxt,
							make_pp_op(cxt, lst->at(1).internal.list),
							make_pp_op(cxt, lst->at(2).internal.list),
							make_pp_op(cxt, lst->at(3).internal.list));
    }

    else if (op == "PPFnSQLExecute")
    {
        if (   lst->size() < 2
           ) throw USER_EXCEPTION(SE1004);

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION(SE1004);
        }

        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size(); i++)
        {
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }


		opit = new PPFnSQLExecute(cxt, arr, false);
    }
    else if (op == "PPFnSQLExecUpdate")
    {
        if (   lst->size() < 2
           ) throw USER_EXCEPTION(SE1004);

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION(SE1004);
        }

        arr_of_PPOpIn arr;
        for (i = 1; i < lst->size(); i++)
        {
            arr.push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }


		opit = new PPFnSQLExecute(cxt, arr, true);
    }

    else if (op == "PPFnSQLClose")
    {
        if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST

           ) throw USER_EXCEPTION(SE1004);

		opit = new PPFnSQLClose(cxt, make_pp_op(cxt, lst->at(1).internal.list));
    }

    else if (op == "PPFnSQLCommit")
    {
        if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST

           ) throw USER_EXCEPTION(SE1004);

		opit = new PPFnSQLCommit(cxt, make_pp_op(cxt, lst->at(1).internal.list));
    }

    else if (op == "PPFnSQLRollback")
    {
        if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST

           ) throw USER_EXCEPTION(SE1004);

		opit = new PPFnSQLRollback(cxt, make_pp_op(cxt, lst->at(1).internal.list));
    }
#else
 else if (op == "PPFnSQLConnect"
	 || op == "PPFnSQLPrepare"
	 || op == "PPFnSQLExecute"
	 || op == "PPFnSQLExecUpdate"
	 || op == "PPFnSQLClose"
	 || op == "PPFnSQLCommit"
	 || op == "PPFnSQLRollback")
    {
		throw USER_EXCEPTION(SE2113);
    }
#endif //SQL_CONNECTION


#ifdef SE_ENABLE_FTSEARCH
	else if (op == "PPFtScan")
    {
        if (   lst->size() < 4 ||
			   lst->size() > 5
           ) throw USER_EXCEPTION(SE1004);

        int i = 0;
        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION(SE1004);
        }

		if (lst->size() == 4)
			opit = new PPFtScan(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list), 
				make_pp_op(cxt, lst->at(3).internal.list));
		else
			opit = new PPFtScan(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list), 
				make_pp_op(cxt, lst->at(3).internal.list),
				make_pp_op(cxt, lst->at(4).internal.list));
    }
	else if (op == "PPFtIndexScan")
    {
        if (   lst->size() != 3
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

		opit = new PPFtIndexScan(cxt,
			make_pp_op(cxt, lst->at(1).internal.list), 
			make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPFtHighlight")
    {
        if (   lst->size() != 3
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

		opit = new PPFtHighlight(cxt,
			make_pp_op(cxt, lst->at(1).internal.list), 
			make_pp_op(cxt, lst->at(2).internal.list),
			false);
    }
	else if (op == "PPFtHighlight2")
    {
        if (   lst->size() != 3
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

		opit = new PPFtHighlight(cxt,
			make_pp_op(cxt, lst->at(1).internal.list), 
			make_pp_op(cxt, lst->at(2).internal.list),
			true);
    }

#else
 else if (   op == "PPFtScan"
	      || op == "PPFtIndexScan"
		  || op == "PPFtHighlight"
		  || op == "PPFtHighlight2")
    {
		throw USER_EXCEPTION2(SE1002, "full-text search support disabled");
    }
#endif //SE_ENABLE_FTSEARCH


/*
    else if (op == "PPAJoin")
    {   // (PPAJoin outer inner outer_names inner_names order pred)
        if (lst->size() != 7) throw USER_EXCEPTION2(SE1004, "20");

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "21");
        }

        arr_of_QNames outer_names;
        scheme_list *_outer_names_ = lst->at(3).internal.list;
        for (i = 0; i != _outer_names_->size(); i++)
        {
            if (_outer_names_->at(i).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "22");

            QName name;
            name.Prefix = "";
            name.LocalPart = string(_outer_names_->at(i).internal.symb);
            outer_names.push_back(name);
        }

        arr_of_QNames inner_names;
        scheme_list *_inner_names_ = lst->at(4).internal.list;
        for (i = 0; i != _inner_names_->size(); i++)
        {
            if (_inner_names_->at(i).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "23");

            QName name;
            name.Prefix = "";
            name.LocalPart = string(_inner_names_->at(i).internal.symb);
            outer_names.push_back(name);
        }

        arr_of_int_pairs order;
        scheme_list *_order_ = lst->at(5).internal.list;
        for (i = 0; i != _order_->size(); i++)
        {
            if (   _order_->at(i).type != SCM_LIST
                || _order_->at(i).internal.list->size() != 2
                || _order_->at(i).internal.list->at(0).type != SCM_NUMBER
                || _order_->at(i).internal.list->at(1).type != SCM_NUMBER) 
                throw USER_EXCEPTION2(SE1004, "24");

            order.push_back(int_pair(atoi(_order_->at(i).internal.list->at(0).internal.num), 
                                     atoi(_order_->at(i).internal.list->at(1).internal.num)));
        }

        opit = new PPAJoin(qp, 
                           PPOpOut(),
                           make_pp_op(qp, sqv, lst->at(1).internal.list),
                           make_pp_op(qp, sqv, lst->at(2).internal.list),
                           outer_names,
                           inner_names,
                           order,
                           make_pp_op(qp, sqv, lst->at(6).internal.list));
    }
    else if (op == "PPASemiJoin")
    {
        if (lst->size() != 6) throw USER_EXCEPTION2(SE1004, "25");

        int i = 0;

        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "26");
        }

        arr_of_QNames outer_names;
        scheme_list *_outer_names_ = lst->at(3).internal.list;
        for (i = 0; i != _outer_names_->size(); i++)
        {
            if (_outer_names_->at(i).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "27");

            QName name;
            name.Prefix = "";
            name.LocalPart = string(_outer_names_->at(i).internal.symb);
            outer_names.push_back(name);
        }

        arr_of_QNames inner_names;
        scheme_list *_inner_names_ = lst->at(4).internal.list;
        for (i = 0; i != _inner_names_->size(); i++)
        {
            if (_inner_names_->at(i).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "28");

            QName name;
            name.Prefix = "";
            name.LocalPart = string(_inner_names_->at(i).internal.symb);
            outer_names.push_back(name);
        }

        opit = new PPASemiJoin(qp, 
                               PPOpOut(),
                               make_pp_op(qp, sqv, lst->at(1).internal.list),
                               make_pp_op(qp, sqv, lst->at(2).internal.list),
                               outer_names,
                               inner_names,
                               make_pp_op(qp, sqv, lst->at(5).internal.list));
    }
*/



    else throw USER_EXCEPTION2(SE1004, ("Wrong plan representation, unknown operation " + op).c_str());

    return PPOpIn(opit, ts);
}

void make_pp_fun(scheme_list *lst, function_declaration &fd)
{
    if (   lst->size() != 5
        || lst->at(0).type != SCM_SYMBOL
        || string(lst->at(0).internal.symb) != "PPFunDecl"
        || lst->at(1).type != SCM_NUMBER
        || lst->at(2).type != SCM_LIST
        || lst->at(3).type != SCM_LIST
        || lst->at(4).type != SCM_LIST
       )   throw USER_EXCEPTION2(SE1004, "201");

    scheme_list *args_list = lst->at(2).internal.list;
    int i = 0;
    for (i = 0; i < args_list->size(); i++)
        if (args_list->at(i).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "202");

    fd.ret_st = make_sequence_type(lst->at(3).internal.list);
    fd.num = args_list->size();
    fd.args = new sequence_type[fd.num];
    fd.cxt_size = atoi(lst->at(1).internal.num);
    for (i = 0; i < fd.num; i++) 
        fd.args[i] = make_sequence_type(args_list->at(i).internal.list);
    fd.op = make_pp_op(NULL, lst->at(4).internal.list).op;
}

PPQueryEssence *make_pp_qe(scheme_list *qe, se_ostream &s, t_print print_mode)
{
    if (   qe->size() < 1 
        || qe->at(0).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "301");

    string op = string(qe->at(0).internal.symb);

    if (op == "PPQueryRoot")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "302");

        int cxt_size = atoi(qe->at(1).internal.num);

        variable_context *cxt = new variable_context(cxt_size);

        return new PPQueryRoot(cxt,
                               make_pp_op(cxt, qe->at(2).internal.list),
                               s,
                               print_mode);
    }
    else if (op == "PPInsertTo")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "303");

        int cxt1_size = atoi(qe->at(1).internal.num);
        int cxt2_size = atoi(qe->at(3).internal.num);

        variable_context *cxt1 = new variable_context(cxt1_size);
        variable_context *cxt2 = new variable_context(cxt2_size);

        return new PPInsertTo(make_pp_op(cxt1, qe->at(2).internal.list),
                              cxt1,
                              make_pp_op(cxt2, qe->at(4).internal.list),
                              cxt2);
    }
    else if (op == "PPInsertFollowing")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "304");

        int cxt1_size = atoi(qe->at(1).internal.num);
        int cxt2_size = atoi(qe->at(3).internal.num);

        variable_context *cxt1 = new variable_context(cxt1_size);
        variable_context *cxt2 = new variable_context(cxt2_size);

        return new PPInsertFollowing(make_pp_op(cxt1, qe->at(2).internal.list),
                                     cxt1,
                                     make_pp_op(cxt2, qe->at(4).internal.list),
                                     cxt2);
    }
    else if (op == "PPInsertBefore")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "305");

        int cxt1_size = atoi(qe->at(1).internal.num);
        int cxt2_size = atoi(qe->at(3).internal.num);

        variable_context *cxt1 = new variable_context(cxt1_size);
        variable_context *cxt2 = new variable_context(cxt2_size);

        return new PPInsertBefore(make_pp_op(cxt1, qe->at(2).internal.list),
                                  cxt1,
                                  make_pp_op(cxt2, qe->at(4).internal.list),
                                  cxt2);
    }
    else if (op == "PPDeleteDeep")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "306");

        int cxt_size = atoi(qe->at(1).internal.num);

        variable_context *cxt = new variable_context(cxt_size);

        return new PPDeleteDeep(make_pp_op(cxt, qe->at(2).internal.list),
                                cxt);
    }
    else if (op == "PPDeleteUndeep")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "307");

        int cxt_size = atoi(qe->at(1).internal.num);

        variable_context *cxt = new variable_context(cxt_size);

        return new PPDeleteUndeep(make_pp_op(cxt, qe->at(2).internal.list),
                                  cxt);
    }
    else if (op == "PPRename")
    {
        if (   qe->size() != 4
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_LIST
            || qe->at(3).internal.list->size() != 2
            || qe->at(3).internal.list->at(0).type != SCM_STRING
            || qe->at(3).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "308");

        int cxt_size = atoi(qe->at(1).internal.num);

        variable_context *cxt = new variable_context(cxt_size);

        char *ncname_prefix = xs_NCName_create(qe->at(3).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        char *ncname_local  = xs_NCName_create(qe->at(3).internal.list->at(1).internal.str, PathExpr_malloc_func(false));

        return new PPRename(make_pp_op(cxt, qe->at(2).internal.list),
                            cxt,
                            ncname_prefix,
                            ncname_local);
    }
    else if (op == "PPReplace")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "309");

        int cxt_size = atoi(qe->at(1).internal.num);

        variable_context *cxt = new variable_context(cxt_size);

        return new PPReplace(make_pp_op(cxt, qe->at(2).internal.list),
                             cxt);
    }
    else if (op == "PPBulkLoad")
    {
        if (   !(qe->size() == 5 || qe->size() == 7 )
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "310");

        int cxt_size1 = atoi(qe->at(1).internal.num);
        variable_context *cxt1 = new variable_context(cxt_size1);

        int cxt_size2 = atoi(qe->at(3).internal.num);
        variable_context *cxt2 = new variable_context(cxt_size2);

        PPOpIn collection;
        if (qe->size() == 7)
        {
            if (   qe->at(5).type != SCM_NUMBER
                || qe->at(6).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "311");

            int cxt_size3 = atoi(qe->at(5).internal.num);
            variable_context *cxt3 = new variable_context(cxt_size3);

            collection = make_pp_op(cxt3, qe->at(6).internal.list);
        }


        return new PPBulkLoad(make_pp_op(cxt1, qe->at(2).internal.list),
                              make_pp_op(cxt2, qe->at(4).internal.list),
                              collection,
                              s);
    }
    else if (op == "PPCreateDocument")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "312");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPCreateDocument(make_pp_op(cxt, qe->at(2).internal.list));
    }
    else if (op == "PPCreateCollection")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "313");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPCreateCollection(make_pp_op(cxt, qe->at(2).internal.list));
    }
    else if (op == "PPCreateDocumentInCollection")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "314");

        int cxt_size1 = atoi(qe->at(1).internal.num);
        variable_context *cxt1 = new variable_context(cxt_size1);

        int cxt_size2 = atoi(qe->at(3).internal.num);
        variable_context *cxt2 = new variable_context(cxt_size2);

        return new PPCreateDocumentInCollection(make_pp_op(cxt1, qe->at(2).internal.list),
                                                make_pp_op(cxt2, qe->at(4).internal.list));
    }
    else if (op == "PPDropDocument")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "315");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPDropDocument(make_pp_op(cxt, qe->at(2).internal.list));
    }
    else if (op == "PPDropCollection")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "316");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPDropCollection(make_pp_op(cxt, qe->at(2).internal.list));
    }
    else if (op == "PPDropDocumentInCollection")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "317");

        int cxt_size1 = atoi(qe->at(1).internal.num);
        variable_context *cxt1 = new variable_context(cxt_size1);

        int cxt_size2 = atoi(qe->at(3).internal.num);
        variable_context *cxt2 = new variable_context(cxt_size2);

        return new PPDropDocumentInCollection(make_pp_op(cxt1, qe->at(2).internal.list),
                                              make_pp_op(cxt2, qe->at(4).internal.list));
    }
    else if (op == "PPRetrieveDSForDocument")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "318");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPRetrieveDS(make_pp_op(cxt, qe->at(2).internal.list),
                                dbe_document,
                                s);
    }
    else if (op == "PPRetrieveDSForCollection")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "319");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPRetrieveDS(make_pp_op(cxt, qe->at(2).internal.list),
                                dbe_collection,
                                s);
    }
    else if (op == "PPRetrieveMetadata")
    {
        PPOpIn collection;
        db_entity_type type;
        bool b;

        if (qe->size() == 3)
        {
            if (   qe->at(1).type != SCM_SYMBOL
                || qe->at(2).type != SCM_BOOL)
                throw USER_EXCEPTION2(SE1004, "320");

            type = make_db_entity_type(qe->at(1).internal.symb);
            b = qe->at(2).internal.b;
        }
        else if (qe->size() == 4)
        {
            if (   qe->at(1).type != SCM_NUMBER
                || qe->at(2).type != SCM_LIST
                || qe->at(3).type != SCM_BOOL)
                throw USER_EXCEPTION2(SE1004, "321");

            int cxt_size = atoi(qe->at(1).internal.num);
            variable_context *cxt = new variable_context(cxt_size);

            collection = make_pp_op(cxt, qe->at(2).internal.list);
            type = dbe_document;
            b = qe->at(3).internal.b;
        }
        else throw USER_EXCEPTION2(SE1004, "322");

        return new PPRetrieveMetadata(type,
                                      collection,
                                      b,
                                      s);
    }
    else if (op == "PPCreateIndex")
    {
        if (   qe->size() != 7
            || qe->at(1).type != SCM_LIST
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_LIST
            || qe->at(4).type != SCM_SYMBOL
            || qe->at(5).type != SCM_NUMBER
            || qe->at(6).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "323");

        scheme_list *lst = qe;

        /// db_entity
        db_entity *db_ent = make_db_entity(lst->at(1).internal.list);

        /// object path
        PathExpr *object_path = lr2PathExpr(NULL, lst->at(2).internal.list, true);

        /// key path
        PathExpr *key_path = lr2PathExpr(NULL, lst->at(3).internal.list, true);

        xmlscm_type key_type = lr_atomic_type2xmlscm_type(lst->at(4).internal.symb);

        int cxt_size = atoi(qe->at(5).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPCreateIndex(object_path,
                                 key_path,
                                 key_type,
                                 counted_ptr<db_entity>(db_ent),
                                 make_pp_op(cxt, qe->at(6).internal.list));
    }
    else if (op == "PPDropIndex")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "324");

        int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

        return new PPDropIndex(make_pp_op(cxt, qe->at(2).internal.list));
    }
#ifdef SE_ENABLE_FTSEARCH
    else if (op == "PPCreateFtIndex")
    {
        if (   qe->size() < 6
			|| qe->size() > 7
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_LIST
			|| qe->at(4).type != SCM_STRING
            || qe->at(5).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "325");

		if (   qe->size() == 7
			&& qe->at(6).type != SCM_LIST)
			throw USER_EXCEPTION2(SE1004, "325");

		scheme_list *lst = qe;
		db_entity *db_ent = make_db_entity(lst->at(2).internal.list);
		PathExpr *object_path = lr2PathExpr(NULL, lst->at(3).internal.list, true);

		int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

		if (qe->size() == 7)
			return new PPCreateFtIndex(object_path,
				qe->at(4).internal.str, 
				counted_ptr<db_entity>(db_ent),
				make_pp_op(cxt, qe->at(5).internal.list), //index name
				make_pp_op(cxt, qe->at(6).internal.list));  //cust_rules
		else
			return new PPCreateFtIndex(object_path,
				qe->at(4).internal.str,
				counted_ptr<db_entity>(db_ent),
				make_pp_op(cxt, qe->at(5).internal.list)); //index name
    }
    else if (op == "PPDropFtIndex")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "326");

		int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

		return new PPDropFtIndex(make_pp_op(cxt, qe->at(2).internal.list));
    }
#else
	else if (   op == "PPCreateFtIndex"
		     || op == "PPDropFtIndex")
		throw USER_EXCEPTION2(SE1002, "full-text search support disabled");
#endif
#ifdef SE_ENABLE_TRIGGERS
    else if (op == "PPCreateTrigger")
    {
/*d_printf2("\n%d",qe->at(0).type);

d_printf2("\n%d",qe->at(3).type);
d_printf2("\n%d",qe->at(4).type);
d_printf2("\n%d",qe->at(5).type);
d_printf2("\n%d",qe->at(6).type);
d_printf2("\n%d",qe->at(7).type);
d_printf2("\n%d",qe->at(8).type);
d_printf2("\n%d",qe->size());
*/
        if (  !(( qe->size() == 9 )||( qe->size() == 12 ))
            || qe->at(1).type != SCM_NUMBER    
            || qe->at(2).type != SCM_SYMBOL    // time
            || qe->at(3).type != SCM_SYMBOL    // event
			|| qe->at(4).type != SCM_LIST      // on (db_entity)
			|| qe->at(5).type != SCM_LIST      // on
            || qe->at(6).type != SCM_SYMBOL    // granularity
			|| qe->at(7).type != SCM_LIST      // action
            || qe->at(8).type != SCM_LIST)     // name
            throw USER_EXCEPTION2(SE1004, "327");

		scheme_list *lst = qe;
		db_entity *db_ent = make_db_entity(lst->at(4).internal.list);
		PathExpr *trigger_path = lr2PathExpr(NULL, lst->at(5).internal.list, true);

		int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

//		trigger_action *tr_action = lr2TriggerAction(lst->at(7).internal.list);
		PathExpr *path_to_parent = NULL;

        // trigger on insert
        if( qe->size() == 12 )
        {
            if (   qe->at(9).type != SCM_STRING    // name of the inserting node
                || qe->at(10).type != SCM_NUMBER   // type of the inserting node (element/attr)
                || qe->at(11).type != SCM_LIST)    // rewritten path to the target node
                throw USER_EXCEPTION2(SE1004, "328");
   			path_to_parent = lr2PathExpr(NULL, lst->at(11).internal.list, true);
            return new PPCreateTrigger(qe->at(2).internal.symb, 			// time 
					               qe->at(3).internal.symb,			// event
                                   counted_ptr<db_entity>(db_ent),	// on (db_entity)
                                   trigger_path,					// on            
					               qe->at(6).internal.symb,			// granularity 
                                   lst->at(7).internal.list,		// action
            					   qe->at(9).internal.str,			// inserting name
            					   atoi(qe->at(1).internal.num),	// inserting type
            					   path_to_parent,					// path to parent of the inserted node
								   make_pp_op(cxt, qe->at(8).internal.list)); // trigger name
        }
        else
            return new PPCreateTrigger(qe->at(2).internal.str, 			// time 
					               qe->at(3).internal.str,			// event
                                   counted_ptr<db_entity>(db_ent),	// on (db_entity)
                                   trigger_path,					// on            
					               qe->at(6).internal.str,			// granularity 
                                   lst->at(7).internal.list,		// action
								   make_pp_op(cxt, qe->at(8).internal.list)); // trigger name
    }
    else if (op == "PPDropTrigger")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "329");

		int cxt_size = atoi(qe->at(1).internal.num);
        variable_context *cxt = new variable_context(cxt_size);

		return new PPDropTrigger(make_pp_op(cxt, qe->at(2).internal.list));
    }
#else
	else if (   op == "PPCreateTrigger"
		     || op == "PPDropTrigger")
		throw USER_EXCEPTION2(SE1002, "Triggers support disabled. Compile Sedna with ENABLE_TRIGGERS=1 if you want to turn this feature on.");
#endif
    else throw USER_EXCEPTION2(SE1004, "399");

    return NULL;
}

PPQueryEssence *scheme_list2qep(scheme_list *lst, se_ostream &s, t_print print_mode)
{
    int i = 0;

    if (   lst->size() != 3
        || lst->at(0).type != SCM_SYMBOL 
        || string(lst->at(0).internal.symb) != "query"
        || lst->at(1).type != SCM_LIST 
        || lst->at(2).type != SCM_LIST
       )    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    scheme_list *qp = lst->at(1).internal.list;
    scheme_list *qe = lst->at(2).internal.list;

    // query-prolog sublist
    if (   qp->size() < 1
        || qp->at(0).type != SCM_SYMBOL
        || string(qp->at(0).internal.symb) != "query-prolog"
       )    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    int qp_size = 0;
    for (i = 1; i < qp->size(); i++)
    {
        if (   qp->at(i).type != SCM_LIST
            || qp->at(i).internal.list->size() < 1
            || qp->at(i).internal.list->at(0).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

        if (strcmp(qp->at(i).internal.list->at(0).internal.symb, "PPFunDecl") == 0) qp_size++;
    }

    tr_globals::qp.size = qp_size;
    tr_globals::qp.fun_decls = new function_declaration[qp_size];

    int j = 0;
    for (i = 1; i < qp->size(); i++) 
    {
        string prolog_decl(qp->at(i).internal.list->at(0).internal.symb);

        if (prolog_decl == "PPNSDecl") 
        {
            if (   qp->at(i).internal.list->size() != 3
                || qp->at(i).internal.list->at(1).type != SCM_STRING
                || qp->at(i).internal.list->at(2).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            tr_globals::st_ct.add_to_context(qp->at(i).internal.list->at(1).internal.str,
                                             qp->at(i).internal.list->at(2).internal.str);
        }
        /*
    	Not implemented yet!
		else if (prolog_decl == "PPBaseUriDecl")
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            tr_globals::st_ct.set_base_uri(qp->at(i).internal.list->at(1).internal.str);
        }
        */
        else if (prolog_decl == "PPDefNSDeclElem")
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            tr_globals::st_ct.add_to_context(NULL,
                                             qp->at(i).internal.list->at(1).internal.str);
        }
        else if (prolog_decl == "PPDefNSDeclFun")  {}
        else if (prolog_decl == "PPOptionDecl")  
        {
            if (qp->at(i).internal.list->size() < 2)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            int k = 0, q = 0;
            for (k = 1; k < qp->at(i).internal.list->size(); k++)
            {
                if (qp->at(i).internal.list->at(k).type != SCM_LIST)
                    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

                scheme_list *single = qp->at(i).internal.list->at(k).internal.list;
/*
                int x1 = single->size();
                scheme_type x2 = single->at(0).type;
                int x3 = single->at(0).internal.list->size();
                scheme_type x4 = single->at(0).internal.list->at(0).type;
                scheme_type x5 = single->at(0).internal.list->at(1).type;
*/

                if (   single->size() < 2
                    || single->at(0).type != SCM_LIST
                    || single->at(0).internal.list->size() != 2
                    || single->at(0).internal.list->at(0).type != SCM_STRING
                    || single->at(0).internal.list->at(1).type != SCM_STRING)
                    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

                char *pr = single->at(0).internal.list->at(0).internal.str;
                char *lp = single->at(0).internal.list->at(1).internal.str;

                for (q = 1; q < single->size(); q++)
                {
                    if (   single->at(q).type != SCM_LIST
                        || single->at(q).internal.list->size() != 2
                        || single->at(q).internal.list->at(0).type != SCM_STRING
                        || single->at(q).internal.list->at(1).type != SCM_STRING)
                    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

                    char *name = single->at(q).internal.list->at(0).internal.str;
                    char *value = single->at(q).internal.list->at(1).internal.str;

                    if (strcmp(pr, SE_NAMESPACE) == 0 && strcmp(lp, "output") == 0)
                    {
                        if (strcmp(name, "method") == 0)
                        {
                            if (strcmp(value, "xml") == 0) 
                            {
                                tr_globals::st_ct.output_method = se_output_method_xml;
                            }
                            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                        }
                        else if (strcmp(name, "indent") == 0)
                        {
                            if (strcmp(value, "yes") == 0) 
                            {
                                tr_globals::st_ct.output_indent = se_output_indent_yes;
                            }
                            else if (strcmp(value, "no") == 0) 
                            {
                                tr_globals::st_ct.output_indent = se_output_indent_no;
                            }
                            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                        }
                        else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                    }
                    else if (strcmp(pr, SE_NAMESPACE) == 0 && strcmp(lp, "character-map") == 0)
                    {
                        // Call add_char_mapping in string matcher. We should substitute 'name' with 
                        // 'value'. Addition check could be needed for name and value

                        // !!!   Uncomment this when everybody is ready   !!!
                        tr_globals::st_ct.add_char_mapping(name, value);
                    }
                    else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                }
            }
        }
        else if (prolog_decl == "PPBoundarySpaceDecl")  
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(1).internal.str, "strip") == 0)
            {
                tr_globals::st_ct.boundary_space = xq_boundary_space_strip;
            }
            else if (strcmp(qp->at(i).internal.list->at(1).internal.str, "preserve") == 0)
            {
                tr_globals::st_ct.boundary_space = xq_boundary_space_preserve;
            }
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        else if (prolog_decl == "PPEmptyOrderDecl")  
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            string s = string(qp->at(i).internal.list->at(1).internal.symb);
            if (s == "greatest")
            {
                tr_globals::st_ct.empty_order = xq_empty_order_greatest;
            }
            else if (s == "least")
            {
                tr_globals::st_ct.empty_order = xq_empty_order_least;
            }
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        else if (prolog_decl == "PPFunDecl") { make_pp_fun(qp->at(i).internal.list, tr_globals::qp.fun_decls[j]); j++; }
        else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
    }

    return make_pp_qe(qe, s, print_mode);
}

PPQueryEssence *build_qep(const char* por, se_ostream& s, t_print print_mode)
{
    scheme_list *qep_tree_in_scheme_lst = NULL;

    // parse LR (extended representation by C++ part)
    //d_printf1("\nmake_tree_from_scheme_list...\n");
    qep_tree_in_scheme_lst = make_tree_from_scheme_list(por);

    // constructs QEP tree
    //d_printf1("scheme_list2qep...\n");
    PPQueryEssence *qep = scheme_list2qep(qep_tree_in_scheme_lst, s, print_mode);

    delete_scheme_list(qep_tree_in_scheme_lst);

    return qep;
}

PPQueryEssence *build_qep(scheme_list *por, se_ostream& s, t_print print_mode)
{
    // constructs QEP tree
    //d_printf1("scheme_list2qep...\n");
    PPQueryEssence *qep = scheme_list2qep(por, s, print_mode);
    return qep;
}

qep_subtree *build_qep(const char* por, int cxt_size)
{
    scheme_list *pp_op_in_scheme_lst = NULL;
    pp_op_in_scheme_lst = make_tree_from_scheme_list(por);

    qep_subtree *res = new qep_subtree();

    res->cxt = new variable_context(cxt_size);
    res->tree = make_pp_op(res->cxt, pp_op_in_scheme_lst);

    delete_scheme_list(pp_op_in_scheme_lst);

    return res;
}

void delete_qep(PPQueryEssence *qep)
{
    delete qep;

    delete [] tr_globals::qp.fun_decls;
    tr_globals::qp.fun_decls = NULL;

    tr_globals::qp.size = 0;
}

void delete_qep(qep_subtree *qep)
{
    delete (qep->cxt);
    qep->cxt = NULL;
    delete (qep->tree.op);
    qep->tree.op = NULL;
    delete qep;
    qep = NULL;
}



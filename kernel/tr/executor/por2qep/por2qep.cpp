/*
 * File:  por2qep.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/por2qep/por2qep.h"
#include "tr/executor/por2qep/ext.h"
#include "tr/strings/e_string.h"
#include "tr/executor/fo/op_map.h"

#include "tr/tr_globals.h"
    
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

#define SE_NAMESPACE		"http://www.modis.ispras.ru/sedna"

using namespace std;

PPOpIn make_pp_op(dynamic_context *cxt, scheme_list *lst);
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

    return se_new UnaryOp(make_calc_op(arr, lst->at(1).internal.list),
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

    get_binary_op_res r = get_binary_op(t, t1, t2);
    if (r.collation)
        return se_new BinaryOpCollation(make_calc_op(arr, lst->at(1).internal.list),
                                     make_calc_op(arr, lst->at(2).internal.list),
                                     r.f.bf_c);
    else
        return se_new BinaryOp(make_calc_op(arr, lst->at(1).internal.list),
                            make_calc_op(arr, lst->at(2).internal.list),
                            r.f.bf);
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

        if (op == "LeafAtomOp") return se_new LeafAtomOp(arr, atoi(lst->at(1).internal.num));
        else if (op == "LeafEffectBoolOp") return se_new LeafEffectBoolOp(arr, atoi(lst->at(1).internal.num));
        else throw USER_EXCEPTION2(SE1004, "102");
    }

    if (   op == "BinaryOpAnd"
        || op == "BinaryOpOr")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST)
        throw USER_EXCEPTION2(SE1004, "103");

        if (op == "BinaryOpAnd") return se_new BinaryOpAnd(make_calc_op(arr, lst->at(1).internal.list), make_calc_op(arr, lst->at(2).internal.list));
        else if (op == "BinaryOpOr") return se_new BinaryOpOr(make_calc_op(arr, lst->at(1).internal.list), make_calc_op(arr, lst->at(2).internal.list));
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
                         dynamic_context *cxt,
                         PPOpIn &child, 
                         NodeTestType &nt_type, 
                         NodeTestData &nt_data,
                         bool persistent)
{
    if (lst->at(3).type != SCM_LIST)
       throw USER_EXCEPTION2(SE1004, "108");

    child = make_pp_op(cxt, lst->at(3).internal.list);

    set_node_test_type_and_data(lst, nt_type, nt_data, persistent);
}

void make_elem_attr_data(scheme_list *lst, sequence_type &st)
{
    if (   lst->size() != 4
        || lst->at(0).type != SCM_SYMBOL
        || lst->at(1).type != SCM_SYMBOL
        || lst->at(2).type != SCM_LIST
        || lst->at(3).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "120");

    string node_name_enum = string(lst->at(0).internal.symb);
    string node_type_enum = string(lst->at(1).internal.symb);

    if (node_name_enum == "wildcard")
        st.type.info.ea.nne = st_nne_wildcard;
    else if (node_name_enum == "name")
        st.type.info.ea.nne = st_nne_name;
    else throw USER_EXCEPTION2(SE1004, "121");

    if (node_type_enum == "nothing")
        st.type.info.ea.tne = st_tne_nothing;
    else if (node_type_enum == "optional")
        st.type.info.ea.tne = st_tne_optional;
    else if (node_type_enum == "present")
        st.type.info.ea.tne = st_tne_present;
    else throw USER_EXCEPTION2(SE1004, "122");


    scheme_list *name = NULL;
    if (st.type.info.ea.nne == st_nne_name)
    {
        name = lst->at(2).internal.list;
        if (   name->size() != 3
            || name->at(0).type != SCM_STRING
            || name->at(1).type != SCM_STRING
            || name->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "123");

        if (*(name->at(0).internal.str))
            st.type.info.ea.node_name_uri = xs_NCName_create(name->at(0).internal.str, PathExpr_malloc_func(false));
        else 
            st.type.info.ea.node_name_uri = NULL;

        st.type.info.ea.node_name_local = xs_NCName_create(name->at(1).internal.str, PathExpr_malloc_func(false));
    }

    if (st.type.info.ea.tne == st_tne_optional || st.type.info.ea.tne == st_tne_present)
    {
        st.type.info.ea.type_name = lr_atomic_type2xmlscm_type(lst->at(3).internal.symb);
    }
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
        st.type.info.single_type = lr_atomic_type2xmlscm_type(lst->at(1).internal.symb);
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
            if (it_lst->size() == 1)
            {
                st.type.type = st_document;
            }
            else if (it_lst->size() == 2)
            {
                if (it_lst->at(1).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "138");

                scheme_list *data_lst = it_lst->at(1).internal.list;
                make_elem_attr_data(data_lst, st);
                st.type.type = st_document_element;
            }
            else throw USER_EXCEPTION2(SE1004, "138.5");
        }
        else if (it_type == "element")
        {
            if (   it_lst->size() != 2
                || it_lst->at(1).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "139");

            scheme_list *data_lst = it_lst->at(1).internal.list;
            make_elem_attr_data(data_lst, st);
            st.type.type = st_element;
        }
        else if (it_type == "attribute")
        {
            if (   it_lst->size() != 2
                || it_lst->at(1).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "140");

            scheme_list *data_lst = it_lst->at(1).internal.list;
            make_elem_attr_data(data_lst, st);
            st.type.type = st_attribute;
        }
        else if (it_type == "pi")
        {
            if (   it_lst->size() < 1
                || it_lst->size() > 2) throw USER_EXCEPTION2(SE1004, "141");
            st.type.type = st_pi;
            if (it_lst->size() == 2)
            {
                if (it_lst->at(1).type != SCM_STRING) throw USER_EXCEPTION2(SE1004, "141.5");
                st.type.info.ncname = xs_NCName_create(it_lst->at(1).internal.str, PathExpr_malloc_func(false));
            }
            else st.type.info.ncname = NULL;
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
    else if (entity == "module") type = dbe_module;
    else throw USER_EXCEPTION2(SE1004, "150");

    return type;
}

db_entity *make_db_entity(scheme_list *ent_lst, bool explicit_name)
{
    db_entity *db_ent = se_new db_entity;

    if (   ent_lst->size() != 2
        || ent_lst->at(0).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "151");

    db_ent->type = make_db_entity_type(ent_lst->at(0).internal.symb);

    if (ent_lst->at(1).type == SCM_STRING)
    {
        string name = string(ent_lst->at(1).internal.str);
        db_ent->name = se_new char[name.length() + 1];
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

    // Abstract base types
    if (strcmp(type, "!xs!anyType") == 0)                 xtype = xs_anyType;
    else if (strcmp(type, "!xs!anySimpleType") == 0)      xtype = xs_anySimpleType;
    else if (strcmp(type, "!xs!anyAtomicType") == 0)      xtype = xs_anyAtomicType;

    // Built-in complex types
    else if (strcmp(type, "!xs!untyped") == 0)            xtype = xs_untyped;

    // Built-in atomic types (Primitive types)
    else if (strcmp(type, "!xs!dateTime") == 0)           xtype = xs_dateTime;
    else if (strcmp(type, "!xs!date") == 0)               xtype = xs_date;
    else if (strcmp(type, "!xs!time") == 0)               xtype = xs_time;
    else if (strcmp(type, "!xs!duration") == 0)           xtype = xs_duration;
    else if (strcmp(type, "!xs!yearMonthDuration") == 0)  xtype = xs_yearMonthDuration;
    else if (strcmp(type, "!xs!dayTimeDuration") == 0)    xtype = xs_dayTimeDuration;
    else if (strcmp(type, "!xs!gYearMonth") == 0)         xtype = xs_gYearMonth;
    else if (strcmp(type, "!xs!gYear") == 0)              xtype = xs_gYear;
    else if (strcmp(type, "!xs!gMonthDay") == 0)          xtype = xs_gMonthDay;
    else if (strcmp(type, "!xs!gDay") == 0)               xtype = xs_gDay;
    else if (strcmp(type, "!xs!gMonth") == 0)             xtype = xs_gMonth;
    else if (strcmp(type, "!xs!float") == 0)              xtype = xs_float;
    else if (strcmp(type, "!xs!double") == 0)             xtype = xs_double;
    else if (strcmp(type, "!xs!decimal") == 0)            xtype = xs_decimal;
    else if (strcmp(type, "!xs!integer") == 0)            xtype = xs_integer;
    else if (strcmp(type, "!xs!boolean") == 0)            xtype = xs_boolean;
    else if (strcmp(type, "!xs!untypedAtomic") == 0)      xtype = xs_untypedAtomic;
    else if (strcmp(type, "!xs!string") == 0)             xtype = xs_string;
    else if (strcmp(type, "!xs!base64Binary") == 0)       xtype = xs_base64Binary;
    else if (strcmp(type, "!xs!hexBinary") == 0)          xtype = xs_hexBinary;
    else if (strcmp(type, "!xs!anyURI") == 0)             xtype = xs_anyURI;
    else if (strcmp(type, "!xs!QName") == 0)              xtype = xs_QName;
    else if (strcmp(type, "!xs!NOTATION") == 0)           xtype = xs_NOTATION;

    // Types derived from xs:string
    else if (strcmp(type, "!xs!normalizedString") == 0)   xtype = xs_normalizedString;
    else if (strcmp(type, "!xs!token") == 0)              xtype = xs_token;
    else if (strcmp(type, "!xs!language") == 0)           xtype = xs_language;
    else if (strcmp(type, "!xs!NMTOKEN") == 0)            xtype = xs_NMTOKEN;
    else if (strcmp(type, "!xs!Name") == 0)               xtype = xs_Name;
    else if (strcmp(type, "!xs!NCName") == 0)             xtype = xs_NCName;
    else if (strcmp(type, "!xs!ID") == 0)                 xtype = xs_ID;
    else if (strcmp(type, "!xs!IDREF") == 0)              xtype = xs_IDREF;
    else if (strcmp(type, "!xs!ENTITY") == 0)             xtype = xs_ENTITY;

    // Types derived from xs:integer
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

    // Special Sedna types
    else if (strcmp(type, "!se!separator") == 0)          xtype = se_separator;
    else if (strcmp(type, "!se!sequence") == 0)           xtype = se_sequence;
    else throw USER_EXCEPTION2(SE1004, "155");

    return xtype;
}

orb_modifier make_order_by_modifier(scheme_list *lst, dynamic_context *cxt)
{
    if (   lst->size() < 2 
        || lst->size() > 3
        || lst->at(0).type != SCM_SYMBOL
        || lst->at(1).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "156");
        
    orb_modifier m;    
        
    string status = string(lst->at(0).internal.symb);
    
    if(status == "greatest") m.status = ORB_EMPTY_GREATEST;
    else if(status == "least") m.status = ORB_EMPTY_LEAST;
    else if(status == "default")
    	m.status = cxt->st_cxt->empty_order == xq_empty_order_least ? ORB_EMPTY_LEAST : ORB_EMPTY_GREATEST;
    else throw USER_EXCEPTION2(SE1004, "157");

    string order = string(lst->at(1).internal.symb);
    
    if(order == "ascending" || order == "default") m.order = ORB_ASCENDING;
    else if(order == "descending") m.order = ORB_DESCENDING;
    else throw USER_EXCEPTION2(SE1004, "158");
    
    if(lst->size() == 2)
        m.collation = cxt->st_cxt->get_default_collation();
    else
    {
        try{
            m.collation = cxt->st_cxt->get_collation(lst->at(2).internal.str);
        }
        catch(SednaUserException &e){
            if(e.get_code() == FOCH0002) throw USER_EXCEPTION2(XQST0076, string(lst->at(2).internal.str).c_str());
            throw;
        }
    } 
    
    return m;
}


PPOpIn make_pp_op(dynamic_context *cxt, scheme_list *lst)
{
    if (   lst->size() != 2
        || (lst->at(0).type != SCM_NUMBER && lst->at(0).type != SCM_LIST)
        || lst->at(1).type != SCM_LIST
        || lst->at(1).internal.list->at(0).type != SCM_SYMBOL) 
        throw USER_EXCEPTION2(SE1004, "01");

    int line, ts;
    
    if (   lst->at(0).type == SCM_LIST )
    {
        scheme_list* lst_int = lst->at(0).internal.list;
        
        if( lst_int->size() != 2 
            ||  lst_int->at(0).type != SCM_NUMBER
            ||  lst_int->at(1).type != SCM_NUMBER)
        throw USER_EXCEPTION2(SE1004, "01.1");
        
        ts   = atoi(lst_int->at(0).internal.num);
        line = atoi(lst_int->at(1).internal.num);
    }
    else
    {
        ts = atoi(lst->at(0).internal.num);
        line = 0;
    }

    string op = string(lst->at(1).internal.list->at(0).internal.symb);
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
   
            opit = se_new PPReturn(cxt,
                                vars, 
                                make_pp_op(cxt, lst->at(2).internal.list),
                                make_pp_op(cxt, lst->at(3).internal.list),
                                pos,
                                make_sequence_type(lst->at(5).internal.list));
 
        }
        else
        {
            opit = se_new PPReturn(cxt,
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

        opit = se_new PPSelect(cxt,
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

            opit = se_new PPLet(cxt,
                             vars,
                             make_pp_op(cxt, lst->at(2).internal.list),
                             make_pp_op(cxt, lst->at(3).internal.list),
                             make_sequence_type(lst->at(4).internal.list));
 
        }
        else
        {
            opit = se_new PPLet(cxt,
                             vars,
                             make_pp_op(cxt, lst->at(2).internal.list),
                             make_pp_op(cxt, lst->at(3).internal.list));
        }
    }
    else if (op == "PPSLet")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "06.2");

        arr_of_var_dsc vars;
        scheme_list *_vars_ = lst->at(1).internal.list;
        for (int i = 0; i != _vars_->size(); i++)
        {
            if (_vars_->at(i).type != SCM_NUMBER)
                throw USER_EXCEPTION2(SE1004, "06.3");

            int var = atoi(_vars_->at(i).internal.num);
            vars.push_back(var);
        }
        
        opit = se_new PPSLet(cxt,
                          vars,
                          make_pp_op(cxt, lst->at(2).internal.list),
                          make_pp_op(cxt, lst->at(3).internal.list));
    }
    else if (op == "PPConst")
    {
        if (lst->size() != 3
           ) throw USER_EXCEPTION2(SE1004, "08");

        tuple_cell tc = make_const(lst->at(2), lst->at(1));

        opit = se_new PPConst(cxt, tc);
    }
    else if (op == "PPVariable")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_NUMBER
           ) throw USER_EXCEPTION2(SE1004, "10");

        int var = atoi(lst->at(1).internal.num);
        opit = se_new PPVariable(cxt, var);
    }
    else if (op == "PPGlobalVariable")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_NUMBER
           ) throw USER_EXCEPTION2(SE1004, "10.5");

        int var = atoi(lst->at(1).internal.num);
        opit = se_new PPGlobalVariable(cxt, var);
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

        opit = se_new PPSequence(cxt, arr);
    }

    else if (op == "PPDmNodeKind")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "13");

        opit = se_new PPDmNodeKind(cxt, 
                                make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnNodeName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "14");

        opit = se_new PPFnNodeName(cxt, 
                                make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnBaseURI")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "15");

        opit = se_new PPFnBaseURI(cxt, 
                               make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnNilled")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "16");

        opit = se_new PPFnNilled(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list));
    }





/* !!! DELETE THIS LATER    !!!*/
    else if (op == "PPDmStringValue")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "15");

        opit = se_new PPDmStringValue(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPDmTypedValue")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "16");

        opit = se_new PPDmTypedValue(cxt, 
                                  make_pp_op(cxt, lst->at(1).internal.list));
    }
/* !!! DELETE THIS LATER    !!!*/




    else if (op == "PPFnEmpty")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "17");

        opit = se_new PPFnEmpty(cxt, 
                             make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnExists")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "18");

        opit = se_new PPFnExists(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPIf")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "19");

        opit = se_new PPIf(cxt,
                        make_pp_op(cxt, lst->at(1).internal.list),
                        make_pp_op(cxt, lst->at(2).internal.list),
                        make_pp_op(cxt, lst->at(3).internal.list));
    }
    else if (op == "PPNil")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "20");

        opit = se_new PPNil(cxt);
    }
    else if (op == "PPStore")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "21");

        opit = se_new PPStore(cxt, 
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

        arr_of_PPOpIn *arr = se_new arr_of_PPOpIn;
        for (i = 2; i < lst->size(); i++)
        {
            arr->push_back(make_pp_op(cxt, lst->at(i).internal.list));
        }

        CalcOp * tree = make_calc_op(arr, lst->at(1).internal.list);

        opit = se_new PPCalculate(cxt,
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
        
        opit = se_new PPAxisChild(cxt,
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
        
        opit = se_new PPAxisAttribute(cxt,
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
        
        opit = se_new PPAxisParent(cxt,
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
        
        opit = se_new PPAxisSelf(cxt,
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
        
        opit = se_new PPAxisDescendant(cxt,
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
        
        opit = se_new PPAxisAncestor(cxt,
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
        
        opit = se_new PPAxisDescendantOrSelf(cxt,
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
        
        opit = se_new PPAxisAncestorOrSelf(cxt,
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
        
        opit = se_new PPAxisFP(cxt,
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
        
        opit = se_new PPAxisSibling(cxt,
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
        
        opit = se_new PPAxisSibling(cxt,
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
        
        opit = se_new PPAxisFP(cxt,
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
        
        opit = se_new PPAxisDescendantAttr(cxt,
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

            opit = se_new PPElementConstructor(cxt,
                                            name.c_str(),
                                            make_pp_op(cxt, lst->at(2).internal.list),
                                            lst->at(3).internal.b,lst->at(4).internal.b);
        }
        else
        {
            opit = se_new PPElementConstructor(cxt,
                                            make_pp_op(cxt, lst->at(1).internal.list),
                                            make_pp_op(cxt, lst->at(2).internal.list),
                                            lst->at(3).internal.b,lst->at(4).internal.b);
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

            opit = se_new PPAttributeConstructor(cxt,
                                              name.c_str(),
                                              make_pp_op(cxt, lst->at(2).internal.list),lst->at(3).internal.b);
        }
        else
        {
            opit = se_new PPAttributeConstructor(cxt,
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

            opit = se_new PPPIConstructor( cxt,
                                        target.c_str(),
                                        make_pp_op(cxt, lst->at(2).internal.list),
                                        lst->at(3).internal.b);
        }
        else
        {
            opit = se_new PPPIConstructor( cxt,
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

        opit = se_new PPCommentConstructor(cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list),
                                        lst->at(2).internal.b);
    }
	else if (op == "PPText")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "32.7");

        opit = se_new PPTextConstructor(cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list),
                                        lst->at(2).internal.b);
    }
	else if (op == "PPDocument")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST            
           ) throw USER_EXCEPTION2(SE1004, "32.7");

        opit = se_new PPDocumentConstructor(cxt,
                                        make_pp_op(cxt, lst->at(1).internal.list) );
    }
    else if (op == "PPFnNot")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "33");

        opit = se_new PPFnNot(cxt, 
                           make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnBoolean")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "33.1");

        opit = se_new PPFnBoolean(cxt, 
                               make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnTrue")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "34");

        opit = se_new PPFnTrue(cxt);
    }
    else if (op == "PPFnFalse")
    {
        if (   lst->size() != 1
           ) throw USER_EXCEPTION2(SE1004, "35");

        opit = se_new PPFnFalse(cxt);
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
            opit = se_new PPAbsPath(cxt, 
                                 path_expr, 
                                 counted_ptr<db_entity>(db_ent));
        }
        else if (ent_lst->at(1).type == SCM_LIST)
        {
            opit = se_new PPAbsPath(cxt, 
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

        opit = se_new PPFnCount(cxt,
                             make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnSum")
    {
        if (   lst->size() < 2
            || lst->size() > 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "43");

        if (lst->size() == 3)
        {
        	if(lst->at(2).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "43.1");
            opit = se_new PPFnSumAvg(cxt,
                                  0,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = se_new PPFnSumAvg(cxt,
                                  0,
                                  make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnAvg")
    {
        if (   lst->size() < 2
            || lst->size() > 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "44");

        if (lst->size() == 3)
        {
        	if(lst->at(2).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "44.1");
            opit = se_new PPFnSumAvg(cxt,
                                  1,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = se_new PPFnSumAvg(cxt,
                                  1,
                                  make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnMax")
    {
        if (   lst->size() < 2
            || lst->size() > 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "45");

        if (lst->size() == 3)
        {
        	if(lst->at(2).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "45.1");
            opit = se_new PPFnMaxMin(cxt,
                                  0,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = se_new PPFnMaxMin(cxt,
                                  0,
                                  make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnMin")
    {
        if (   lst->size() < 2
            || lst->size() > 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46");

        if (lst->size() == 3)
        {
        	if(lst->at(2).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "46.1");
            opit = se_new PPFnMaxMin(cxt,
                                  1,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = se_new PPFnMaxMin(cxt,
                                  1,
                                  make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnEncodeForUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.1");

        opit = se_new PPFnUriEncoding(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   PPFnUriEncoding::ENCODE_FOR_URI);
    }
    else if (op == "PPFnIriToUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.2");

        opit = se_new PPFnUriEncoding(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   PPFnUriEncoding::IRI_TO_URI);
    }
    else if (op == "PPFnEscapeHtmlUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "46.3");

        opit = se_new PPFnUriEncoding(cxt, 
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
            opit = se_new PPFnResolveUri(cxt, 
                                      make_pp_op(cxt, lst->at(1).internal.list),
                                      make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = se_new PPFnResolveUri(cxt, 
                                      make_pp_op(cxt, lst->at(1).internal.list));
        
    }
    else if (op == "PPFnItemAt")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "47");

        opit = se_new PPFnItemAt(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnOneOrMore")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-11");

        opit = se_new PPFnOneOrMore(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list));
                                 
    }
    else if (op == "PPFnExactlyOne")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-10");

        opit = se_new PPFnExactlyOne(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list));
                                 
    }
    else if (op == "PPFnZeroOrOne")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-9");

        opit = se_new PPFnZeroOrOne(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list));
                                 
    }
    else if (op == "PPFnInsertBefore")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-8");

        opit = se_new PPFnInsertBefore(cxt,
                                    make_pp_op(cxt, lst->at(1).internal.list),
                                    make_pp_op(cxt, lst->at(2).internal.list),
                                    make_pp_op(cxt, lst->at(3).internal.list));
    }
    else if (op == "PPFnRemove")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-7");

        opit = se_new PPFnRemove(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnDistinctValues")
    {
        if (   lst->size() < 2
            || lst->size() > 3
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-6");

        if (lst->size() == 3)
        {
        	if(lst->at(2).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "48.-5");
            opit = se_new PPFnDistinctValues(cxt,
                                          make_pp_op(cxt, lst->at(1).internal.list),
                                          make_pp_op(cxt, lst->at(2).internal.list));
        }
        else
            opit = se_new PPFnDistinctValues(cxt,
                                          make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnIndexOf")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-4");

        if (lst->size() == 4)
        {
        	if(lst->at(3).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "48.-3");
            opit = se_new PPFnIndexOf(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   make_pp_op(cxt, lst->at(2).internal.list),
                                   make_pp_op(cxt, lst->at(3).internal.list));
        }
        else
            opit = se_new PPFnIndexOf(cxt,
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnReverse")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "48.-2");

        opit = se_new PPFnReverse(cxt,
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
            opit = se_new PPFnSubsequence(cxt,
                                       make_pp_op(cxt, lst->at(1).internal.list),
                                       make_pp_op(cxt, lst->at(2).internal.list),
                                       make_pp_op(cxt, lst->at(3).internal.list));
        }
        else
            opit = se_new PPFnSubsequence(cxt,
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

        opit = se_new PPTypeswitch(cxt,
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

        opit = se_new PPCast(cxt,
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

        opit = se_new PPCastable(cxt,
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

        opit = se_new PPInstanceOf(cxt,
                                make_pp_op(cxt, lst->at(1).internal.list),
                                make_sequence_type(lst->at(2).internal.list));
    }
    else if (op == "PPTreat")
    {
    	if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "69.1");

        opit = se_new PPTreat(cxt,
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

        opit = se_new PPDDO(cxt,
                         make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPSXptr")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "57.1");

        opit = se_new PPSXptr(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFEL")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "157");

        opit = se_new PPFilterEL(cxt,
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
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_LIST
            || lst->at(4).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "61");

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

		opit = se_new PPIndexScan(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
                                  isc); 
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

        opit = se_new PPFunCall(cxt,
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

        opit = se_new PPNamespaceConstructor(cxt,
                                          lst->at(1).internal.str,
                                          make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = se_new PPFnName(cxt, 
                            make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnLocalName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = se_new PPFnLocalName(cxt, 
                                 make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnNamespaceUri")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = se_new PPFnNamespaceUri(cxt, 
                                    make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnNumber")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = se_new PPFnNumber(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnRoot")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "73");

        opit = se_new PPFnRoot(cxt, 
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

        opit = se_new PPNumericFuncs(cxt, 
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  func);
    }
    else if (op == "PPFnRoundHalfToEven")
    {
        if (   lst->size() == 2
            && lst->at(1).type == SCM_LIST
           ) 
        {
            opit = se_new PPFnRoundHalfToEven(cxt, 
                                           make_pp_op(cxt, lst->at(1).internal.list),
                                           0);
        }
        else if (   lst->size() == 3
                 && lst->at(1).type == SCM_LIST
                 && lst->at(2).type == SCM_LIST
                ) 
        {
            opit = se_new PPFnRoundHalfToEven(cxt, 
                                           make_pp_op(cxt, lst->at(1).internal.list),
                                           make_pp_op(cxt, lst->at(2).internal.list));
        }
        else throw USER_EXCEPTION2(SE1004, "73");
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
        opit = se_new PPSpaceSequence(cxt, arr,lst->at(lst->size()-1).internal.b);
    }


    /////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////
    if (opit) 
    {
        opit->set_xquery_line(line);
        return PPOpIn(opit, ts);
    }
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

        opit = se_new PPFnError(cxt, 
                             child_err, child_descr, child_obj);
    }
	else if (op == "PPFnTrace")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "76");

		opit = se_new PPFnTrace(cxt,
                             make_pp_op(cxt, lst->at(1).internal.list),
                             make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPTest")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "77");

        opit = se_new PPTest(cxt, 
                          make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op=="PPFnTokenize")
	{
		if (   lst->size() < 3
            || lst->size() > 5
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "78");

        __int16 pm = pm_tokenize;


        if (lst->size() == 3)
        {
            opit = se_new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list),
		                          pm);
        }
        else if (lst->size() == 4)
        {
            opit = se_new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
		                          pm);
        }
       
        else throw USER_EXCEPTION2(SE1004, "80");

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
            opit = se_new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
		                          pm);
        }
        else if (lst->size() == 5)
        {
            opit = se_new PPPatMatch(cxt,
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  make_pp_op(cxt, lst->at(3).internal.list),
                                  make_pp_op(cxt, lst->at(4).internal.list),
		                          pm);
        }
        else if (lst->size() == 6)
        {
            opit = se_new PPPatMatch(cxt,
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

        opit = se_new PPDocInCol(cxt, 
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnDocAvailable")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "81.1");

        opit = se_new PPFnDocAvailable(cxt, 
                                    make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPTuple" || op == "PPSTuple")   /// PPSTuple is used only with PPOrderBy
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

        if(op == "PPTuple") opit = se_new PPTuple(cxt, arr);
        else opit = se_new PPSTuple(cxt, arr);
    }
    else if (op == "PPPred1" || op == "PPPred2")	/// PPPred1 and PPPred2 have much in common, hence they are joined in one "if"
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
	
    	        opit = se_new PPPred1(cxt,
        	                       vars,
            	                   make_pp_op(cxt, lst->at(2).internal.list),
                	               _conjuncts_,
                    	           _conditions_,
                        	       make_pp_op(cxt, lst->at(4).internal.list),
                            	   _once_,
	                               pos);
    	    }
	        else
    	        opit = se_new PPPred1(cxt,
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
		
        		opit = se_new PPPred2(cxt,
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
        		opit = se_new PPPred2(cxt,
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
        /// Uncomment it in future!
        /*if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "89");

        opit = se_new PPUnion(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list),
                              lst->at(3).internal.b);*/
        
        /// Remove it in future!
        if (   lst->size() < 3 || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "89");

        bool doc_order = false;  /// By default we use SXptr semantics now
        
        if(4 == lst->size())
        {
            if(lst->at(3).type != SCM_BOOL) throw USER_EXCEPTION2(SE1004, "89.1");
            else doc_order = lst->at(3).internal.b;
        }

        opit = se_new PPUnion(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list),
                              doc_order);

    }
    else if (op == "PPIntersect")
    {
        /// Uncomment it in future!
        /*if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "89");

        opit = se_new PPIntersect(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  lst->at(3).internal.b);*/
        
        /// Remove it in future!
        if (   lst->size() < 3 || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "90");

        bool doc_order = false;  /// By default we use SXptr semantics now
        
        if(4 == lst->size())
        {
            if(lst->at(3).type != SCM_BOOL) throw USER_EXCEPTION2(SE1004, "90.1");
            else doc_order = lst->at(3).internal.b;
        }

        opit = se_new PPIntersect(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list),
                                  doc_order);

    }
    else if (op == "PPExcept")
    {
        /// Uncomment it in future!
        /*if (   lst->size() != 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
            || lst->at(3).type != SCM_BOOL
           ) throw USER_EXCEPTION2(SE1004, "89");

        opit = se_new PPExcept(cxt,
                               make_pp_op(cxt, lst->at(1).internal.list),
                               make_pp_op(cxt, lst->at(2).internal.list),
                               lst->at(3).internal.b);*/
        
        /// Remove it in future!
        if (   lst->size() < 3 || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "91");

        bool doc_order = false;  /// By default we use SXptr semantics now
        
        if(4 == lst->size())
        {
            if(lst->at(3).type != SCM_BOOL) throw USER_EXCEPTION2(SE1004, "91.1");
            else doc_order = lst->at(3).internal.b;
        }

        opit = se_new PPExcept(cxt,
                               make_pp_op(cxt, lst->at(1).internal.list),
                               make_pp_op(cxt, lst->at(2).internal.list),
                               doc_order);

    }
    else if (op == "PPADFilter")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "92");

        opit = se_new PPADFilter(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list),
                              make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPDAFilter")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "93");

        opit = se_new PPDAFilter(cxt,
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

        opit = se_new PPScan(cxt,
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

        opit = se_new PPUp(cxt,
                        make_pp_op(cxt, lst->at(1).internal.list),
                        scm_node);
    }
    else if (op == "PPFnCompare")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "95.1");

        if(lst->size() == 4)
        {
        	if(lst->at(3).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "95.2");
            opit = se_new PPFnCompare(cxt,
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   make_pp_op(cxt, lst->at(2).internal.list),
                                   make_pp_op(cxt, lst->at(3).internal.list));
        }
        else
            opit = se_new PPFnCompare(cxt,
                                   make_pp_op(cxt, lst->at(1).internal.list),
                                   make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnCodepointEqual")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "95.3");

        opit = se_new PPFnCompare(cxt,
                               make_pp_op(cxt, lst->at(1).internal.list),
                               make_pp_op(cxt, lst->at(2).internal.list),
                               true);
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

        opit = se_new PPFnConcat(cxt, 
                              arr);
    }
    else if (op == "PPFnSubstring")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "97.1");

        if(lst->size() == 4)
        {
        	if(lst->at(3).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "97.2");
            opit = se_new PPFnSubstring(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list),
                                     make_pp_op(cxt, lst->at(3).internal.list));
        }
        else
            opit = se_new PPFnSubstring(cxt,
                                     make_pp_op(cxt, lst->at(1).internal.list),
                                     make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnStartsWith" || op == "PPFnEndsWith")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "97.2");

        PPFnStartsEndsWith::FunctionType type = op == "PPFnStartsWith" ? 
                                                PPFnStartsEndsWith::FN_STARTS_WITH :
                                                PPFnStartsEndsWith::FN_ENDS_WITH;
        if(lst->size() == 4)
        {
        	if(lst->at(3).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "97.3");
            opit = se_new PPFnStartsEndsWith(cxt,
                                          make_pp_op(cxt, lst->at(1).internal.list),
                                          make_pp_op(cxt, lst->at(2).internal.list),
                                          make_pp_op(cxt, lst->at(3).internal.list),
                                          type);
        }
        else
            opit = se_new PPFnStartsEndsWith(cxt,
                                          make_pp_op(cxt, lst->at(1).internal.list),
                                          make_pp_op(cxt, lst->at(2).internal.list),
                                          type);
    }
    else if (op == "PPFnSubstringBefore" || op == "PPFnSubstringAfter")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "97.3");

        PPFnSubsBeforeAfter::FunctionType type = op == "PPFnSubstringBefore" ? 
                                                 PPFnSubsBeforeAfter::FN_BEFORE :
                                                 PPFnSubsBeforeAfter::FN_AFTER;
        if(lst->size() == 4)
        {
        	if(lst->at(3).type != SCM_LIST) throw USER_EXCEPTION2(SE1004, "97.4");
            opit = se_new PPFnSubsBeforeAfter(cxt,
                                           make_pp_op(cxt, lst->at(1).internal.list),
                                           make_pp_op(cxt, lst->at(2).internal.list),
                                           make_pp_op(cxt, lst->at(3).internal.list),
                                           type);
        }
        else
            opit = se_new PPFnSubsBeforeAfter(cxt,
                                           make_pp_op(cxt, lst->at(1).internal.list),
                                           make_pp_op(cxt, lst->at(2).internal.list),
                                           type);
    }
    else if (op == "PPFnStringLength")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "98");

        opit = se_new PPFnStringLength(cxt,
                                    make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnStringJoin")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "98.1");

        opit = se_new PPFnStringJoin(cxt,
                                  make_pp_op(cxt, lst->at(1).internal.list),
                                  make_pp_op(cxt, lst->at(2).internal.list));
    }
    else if (op == "PPFnNormalizeSpace")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "98.2");

        opit = se_new PPFnNormalizeSpace(cxt,
                                      make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnStringToCodepoints")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "159");

        opit = se_new PPFnString2CodePoints(cxt,
                                    make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnCodepointsToString")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "159.1");

        opit = se_new PPFnCodePoints2String(cxt,
                                         make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnTranslate")
	{
		if (   lst->size() != 4
			|| lst->at(1).type != SCM_LIST
			) throw USER_EXCEPTION2(SE1004, "98.5");

		opit = se_new PPFnTranslate(cxt,
			make_pp_op(cxt, lst->at(1).internal.list),
			make_pp_op(cxt, lst->at(2).internal.list),
			make_pp_op(cxt, lst->at(3).internal.list));
	}
	else if (op == "PPFnUpperCase")
	{
		if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST
			) throw USER_EXCEPTION2(SE1004, "98.6");

		opit = se_new PPFnChangeCase(cxt,
			make_pp_op(cxt, lst->at(1).internal.list), true);
	}
	else if (op == "PPFnLowerCase")
	{
		if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST
			) throw USER_EXCEPTION2(SE1004, "98.7");

		opit = se_new PPFnChangeCase(cxt,
			make_pp_op(cxt, lst->at(1).internal.list), false);
	}
    else if (op == "PPFnString")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "99");

        opit = se_new PPFnString(cxt,
                              make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnData")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "100");

        opit = se_new PPFnData(cxt,
                            make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnDocumentURI")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "101");

        opit = se_new PPFnDocumentURI(cxt, 
                                   make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPFnStaticBaseUri")
    {
        if (  lst->size() != 1  ) throw USER_EXCEPTION2(SE1004, "101.1");

        opit = se_new PPFnStaticBaseUri(cxt);
    }
    else if (op == "PPFnDefaultCollation")
    {
        if (  lst->size() != 1  ) throw USER_EXCEPTION2(SE1004, "101.2");

        opit = se_new PPFnDefaultCollation(cxt);
    }
    else if (op == "PPRange")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "102");

        opit = se_new PPRange(cxt, 
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

        PPXptr *pp_xptr = se_new PPXptr(cxt, 
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

        opit = se_new PPCheckpoint(cxt);
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

        opit = se_new PPFnDateTimeFuncNoParam(cxt, type);
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
        int ftype;

        if (lst->size() == 2 &&
                lst->at(1).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "103");

        xmlscm_type xtype;

        if (op == "PPFnYearsFromDuration") 	{ ftype = PPFnDateTimeFunc::yearsFromDuration; xtype = xs_duration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMonthsFromDuration")	{ ftype = PPFnDateTimeFunc::monthsFromDuration; xtype = xs_duration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnDaysFromDuration")	{ ftype = PPFnDateTimeFunc::daysFromDuration; xtype = xs_duration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnHoursFromDuration")	{ ftype = PPFnDateTimeFunc::hoursFromDuration; xtype = xs_duration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMinutesFromDuration")	{ ftype = PPFnDateTimeFunc::minutesFromDuration; xtype = xs_duration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnSecondsFromDuration")	{ ftype = PPFnDateTimeFunc::secondsFromDuration; xtype = xs_duration; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnYearFromDateTime")	{ ftype = PPFnDateTimeFunc::yearFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMonthFromDateTime")	{ ftype = PPFnDateTimeFunc::monthFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnDayFromDateTime")	{ ftype = PPFnDateTimeFunc::dayFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnHoursFromDateTime")	{ ftype = PPFnDateTimeFunc::hoursFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMinutesFromDateTime")	{ ftype = PPFnDateTimeFunc::minutesFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnSecondsFromDateTime")	{ ftype = PPFnDateTimeFunc::secondsFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromDateTime")	{ ftype = PPFnDateTimeFunc::timezoneFromDateTime; xtype = xs_dateTime; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnYearFromDate")		{ ftype = PPFnDateTimeFunc::yearFromDate; xtype = xs_date; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMonthFromDate")		{ ftype = PPFnDateTimeFunc::monthFromDate; xtype = xs_date; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnDayFromDate")		{ ftype = PPFnDateTimeFunc::dayFromDate; xtype = xs_date; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromDate")	{ ftype = PPFnDateTimeFunc::timezoneFromDate; xtype = xs_date; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnHoursFromTime")		{ ftype = PPFnDateTimeFunc::hoursFromTime; xtype = xs_time; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnMinutesFromTime")	{ ftype = PPFnDateTimeFunc::minutesFromTime; xtype = xs_time; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnSecondsFromTime")	{ ftype = PPFnDateTimeFunc::secondsFromTime; xtype = xs_time; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromTime")	{ ftype = PPFnDateTimeFunc::timezoneFromTime; xtype = xs_time; goto fn_dt_funcs_correct_type; }
        if (op == "PPFnTimezoneFromTime")	{ ftype = PPFnDateTimeFunc::timezoneFromTime; xtype = xs_time; goto fn_dt_funcs_correct_type; }

	throw USER_EXCEPTION2(SE1004, "invalid date time operator");

fn_dt_funcs_correct_type:

        opit = se_new PPFnDateTimeFunc(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list), ftype, xtype);
    }

    // Date-time functions with at most 2 operands

    else if ( op == "PPFnAdjustDateTimeToTimezone" ||
                op == "PPFnAdjustDateToTimezone" ||
                op == "PPFnAdjustTimeToTimezone" ||
		op == "PPFnDateTime" )
    {
        int ftype;
        xmlscm_type xtype;

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
        	if (op == "PPFnAdjustDateTimeToTimezone") ftype = PPFnDateTimeFunc2Params::adjustDateTimeToTimezone;
		else if (op == "PPFnAdjustDateToTimezone" ) ftype = PPFnDateTimeFunc2Params::adjustDateToTimezone;
        	else if (op == "PPFnAdjustTimeToTimezone") ftype = PPFnDateTimeFunc2Params::adjustTimeToTimezone;
        	else if (op == "PPFnDateTime") ftype = PPFnDateTimeFunc2Params::dateTime;
		else throw USER_EXCEPTION2(SE1004, "Invalid date time function");

                opit = se_new PPFnDateTimeFunc2Params(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list),
                           make_pp_op(cxt, lst->at(2).internal.list), ftype);
	}
	else
	{
        	if (op == "PPFnAdjustDateTimeToTimezone")  { ftype = PPFnDateTimeFunc::adjustDateTimeToTimezone; xtype = xs_dateTime; }
		else if (op == "PPFnAdjustDateToTimezone" )    { ftype = PPFnDateTimeFunc::adjustDateToTimezone; xtype = xs_date; }
        	else if (op == "PPFnAdjustTimeToTimezone") { ftype = PPFnDateTimeFunc::adjustTimeToTimezone; xtype = xs_time; }
		else throw USER_EXCEPTION2(SE1004, "Invalid date time function");
	
                opit = se_new PPFnDateTimeFunc(cxt,
                           make_pp_op(cxt, lst->at(1).internal.list), ftype, xtype); 
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
            orb_modifier om = make_order_by_modifier(modifier, cxt);
            _modifiers_.push_back(om);
		}	        

		opit = se_new PPOrderBy(cxt,							
							 lst->at(1).internal.b,    	
							 make_pp_op(cxt, lst->at(2).internal.list), 
		                     _modifiers_,
							 ts);
    }
	else if (op == "PPFnDeepEqual")
    {
        if (   lst->size()<3
			||lst->size()>4
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "107");
		if (lst->size()==3)
		opit = se_new PPFnDeepEqual(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list),
                                 make_pp_op(cxt, lst->at(2).internal.list));
		else
			opit = se_new PPFnDeepEqual(cxt,
                                 make_pp_op(cxt, lst->at(1).internal.list),
                                 make_pp_op(cxt, lst->at(2).internal.list),
								 make_pp_op(cxt, lst->at(3).internal.list));
    }
	else if (op == "PPFnResolveQName")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "107.5");

		opit = se_new PPFnResolveQName(cxt,
                                    make_pp_op(cxt, lst->at(1).internal.list),
                                    make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPFnQName")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "108");

		opit = se_new PPFnQName(cxt,
                             make_pp_op(cxt, lst->at(1).internal.list),
                             make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPxsQName")
    {
        if (   lst->size() != 4
            || lst->at(1).type != SCM_STRING
            || lst->at(2).type != SCM_STRING
            || lst->at(3).type != SCM_STRING
           ) throw USER_EXCEPTION2(SE1004, "108");

        char *uri = lst->at(1).internal.str;
        char *local = lst->at(2).internal.str;
        char *prefix = lst->at(3).internal.str;
        char *qname = xs_QName_create(uri,
                                      prefix,
                                      local,
                                      malloc,
                                      cxt);

        opit = se_new PPConst(cxt, tuple_cell::atomic(xs_QName, qname));
    }
	else if (op == "PPFnPrefixFromQName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "109");

		opit = se_new PPFnPrefixFromQName(cxt,
                                       make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnLocalNameFromQName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "110");

		opit = se_new PPFnLocalNameFromQName(cxt,
                                          make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnNamespaceUriFromQName")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "111");

		opit = se_new PPFnNamespaceUriFromQName(cxt,
                                             make_pp_op(cxt, lst->at(1).internal.list));
    }
	else if (op == "PPFnNamespaceUriForPrefix")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "112");

		opit = se_new PPFnNamespaceUriForPrefix(cxt,
                                             make_pp_op(cxt, lst->at(1).internal.list),
                                             make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPFnInScopePrefixes")
    {
        if (   lst->size() != 2
            || lst->at(1).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "113");

		opit = se_new PPFnInScopePrefixes(cxt,
                                       make_pp_op(cxt, lst->at(1).internal.list));
    }
    else if (op == "PPDebug")
    {
        if (   lst->size() < 3
            || lst->size() > 4
            || lst->at(1).type != SCM_STRING
           ) throw USER_EXCEPTION2(SE1004, "114");

        char* op_name = se_new char[strlen(lst->at(1).internal.str) + 1];
        strcpy(op_name, lst->at(1).internal.str);
        str_counted_ptr child_name(op_name);
        
        if (lst->size() == 4)
        {
            if(lst->at(2).type != SCM_STRING) throw USER_EXCEPTION2(SE1004, "115");
            
            char* op_info = se_new char[strlen(lst->at(2).internal.str) + 1];
            strcpy(op_info, lst->at(2).internal.str);
            str_counted_ptr child_info(op_info);
  		    
  		    opit = se_new PPDebug(cxt,
                               make_pp_op(cxt, lst->at(3).internal.list),
                               child_name,
                               child_info);

        }
        else
		    opit = se_new PPDebug(cxt,
                               make_pp_op(cxt, lst->at(2).internal.list),
                               child_name);
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


		opit = se_new PPFnSQLConnect(cxt, arr);
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
			opit = se_new PPFnSQLPrepare(cxt,
							make_pp_op(cxt, lst->at(1).internal.list),
							make_pp_op(cxt, lst->at(2).internal.list));
		else
			opit = se_new PPFnSQLPrepare(cxt,
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


		opit = se_new PPFnSQLExecute(cxt, arr, false);
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


		opit = se_new PPFnSQLExecute(cxt, arr, true);
    }

    else if (op == "PPFnSQLClose")
    {
        if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST

           ) throw USER_EXCEPTION(SE1004);

		opit = se_new PPFnSQLClose(cxt, make_pp_op(cxt, lst->at(1).internal.list));
    }

    else if (op == "PPFnSQLCommit")
    {
        if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST

           ) throw USER_EXCEPTION(SE1004);

		opit = se_new PPFnSQLCommit(cxt, make_pp_op(cxt, lst->at(1).internal.list));
    }

    else if (op == "PPFnSQLRollback")
    {
        if (   lst->size() != 2
			|| lst->at(1).type != SCM_LIST

           ) throw USER_EXCEPTION(SE1004);

		opit = se_new PPFnSQLRollback(cxt, make_pp_op(cxt, lst->at(1).internal.list));
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
        if (is_ft_disabled)
       		throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

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
			opit = se_new PPFtScan(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list), 
				make_pp_op(cxt, lst->at(3).internal.list));
		else
			opit = se_new PPFtScan(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list), 
				make_pp_op(cxt, lst->at(3).internal.list),
				make_pp_op(cxt, lst->at(4).internal.list));
    }
	else if (op == "PPFtIndexScan")
    {
        if (is_ft_disabled)
       		throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

        if (   lst->size() != 3
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

		opit = se_new PPFtIndexScan(cxt,
			make_pp_op(cxt, lst->at(1).internal.list), 
			make_pp_op(cxt, lst->at(2).internal.list));
    }
	else if (op == "PPFtIndexScan2")
    {
        if (   lst->size() < 3 || lst->size() > 5
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

		
		if (lst->size() == 3)
			opit = se_new PPFtIndexScan2(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list));
		else if (lst->size() == 4)
			opit = se_new PPFtIndexScan2(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list),
				make_pp_op(cxt, lst->at(3).internal.list));
		else
			opit = se_new PPFtIndexScan2(cxt,
				make_pp_op(cxt, lst->at(1).internal.list), 
				make_pp_op(cxt, lst->at(2).internal.list),
				make_pp_op(cxt, lst->at(3).internal.list),
				make_pp_op(cxt, lst->at(4).internal.list));
    }
	else if (op == "PPFtHighlight")
    {
        if (is_ft_disabled)
       		throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

        if (   lst->size() < 3 || lst->size() > 4
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

	    if (lst->size() == 3)
		{
		    opit = se_new PPFtHighlight(cxt,
		    	make_pp_op(cxt, lst->at(1).internal.list), 
		    	make_pp_op(cxt, lst->at(2).internal.list),
		    	false);
		}
		else
		{
            if (lst->at(3).type != SCM_LIST)
                throw USER_EXCEPTION(SE1004);
		    opit = se_new PPFtHighlight(cxt,
		    	make_pp_op(cxt, lst->at(1).internal.list), 
		    	make_pp_op(cxt, lst->at(2).internal.list),
		    	make_pp_op(cxt, lst->at(3).internal.list),
		    	false);
		}
    }
	else if (op == "PPFtHighlight2")
    {
        if (is_ft_disabled)
       		throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

        if (   lst->size() < 3 || lst->size() > 4
			|| lst->at(1).type != SCM_LIST
			|| lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION(SE1004);

        if (lst->size() == 3)
		{
    		opit = se_new PPFtHighlight(cxt,
    			make_pp_op(cxt, lst->at(1).internal.list), 
    			make_pp_op(cxt, lst->at(2).internal.list),
    			true);
		}
		else
		{
			if (lst->at(3).type != SCM_LIST)
				throw USER_EXCEPTION(SE1004);
    		opit = se_new PPFtHighlight(cxt,
    			make_pp_op(cxt, lst->at(1).internal.list), 
    			make_pp_op(cxt, lst->at(2).internal.list),
    			make_pp_op(cxt, lst->at(3).internal.list),
    			true);
		}
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

        opit = se_new PPAJoin(qp, 
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

        opit = se_new PPASemiJoin(qp, 
                               PPOpOut(),
                               make_pp_op(qp, sqv, lst->at(1).internal.list),
                               make_pp_op(qp, sqv, lst->at(2).internal.list),
                               outer_names,
                               inner_names,
                               make_pp_op(qp, sqv, lst->at(5).internal.list));
    }
*/



    else throw USER_EXCEPTION2(SE1004, ("Wrong plan representation, unknown operation " + op).c_str());

    opit->set_xquery_line(line);
    return PPOpIn(opit, ts);
}

void make_pp_fun(scheme_list *lst, static_context *cxt, function_declaration &fd)
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
    fd.args = se_new sequence_type[fd.num];
    fd.cxt_size = atoi(lst->at(1).internal.num);
    fd.st_cxt = cxt;
    for (i = 0; i < fd.num; i++) 
        fd.args[i] = make_sequence_type(args_list->at(i).internal.list);
    dynamic_context dc(cxt, 0);
    fd.op = make_pp_op(&dc, lst->at(4).internal.list).op;
}

PPQueryEssence *make_pp_qe(scheme_list *qe, static_context *st_cxt, t_print print_mode)
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

        int var_cxt_size = atoi(qe->at(1).internal.num);

        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPQueryRoot(cxt,
                               make_pp_op(cxt, qe->at(2).internal.list),
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

        dynamic_context *cxt1 = se_new dynamic_context(st_cxt, cxt1_size);
        dynamic_context *cxt2 = se_new dynamic_context(st_cxt, cxt2_size);

        return se_new PPInsertTo(make_pp_op(cxt1, qe->at(2).internal.list),
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

        dynamic_context *cxt1 = se_new dynamic_context(st_cxt, cxt1_size);
        dynamic_context *cxt2 = se_new dynamic_context(st_cxt, cxt2_size);

        return se_new PPInsertFollowing(make_pp_op(cxt1, qe->at(2).internal.list),
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

        dynamic_context *cxt1 = se_new dynamic_context(st_cxt, cxt1_size);
        dynamic_context *cxt2 = se_new dynamic_context(st_cxt, cxt2_size);

        return se_new PPInsertBefore(make_pp_op(cxt1, qe->at(2).internal.list),
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

        int var_cxt_size = atoi(qe->at(1).internal.num);

        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPDeleteDeep(make_pp_op(cxt, qe->at(2).internal.list),
                                cxt);
    }
    else if (op == "PPDeleteUndeep")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "307");

        int var_cxt_size = atoi(qe->at(1).internal.num);

        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPDeleteUndeep(make_pp_op(cxt, qe->at(2).internal.list),
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

        int var_cxt_size = atoi(qe->at(1).internal.num);

        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        char *ncname_prefix = xs_NCName_create(qe->at(3).internal.list->at(0).internal.str, PathExpr_malloc_func(false));
        char *ncname_local  = xs_NCName_create(qe->at(3).internal.list->at(1).internal.str, PathExpr_malloc_func(false));

        return se_new PPRename(make_pp_op(cxt, qe->at(2).internal.list),
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

        int var_cxt_size = atoi(qe->at(1).internal.num);

        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPReplace(make_pp_op(cxt, qe->at(2).internal.list),
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

        int var_cxt_size1 = atoi(qe->at(1).internal.num);
        dynamic_context *cxt1 = se_new dynamic_context(st_cxt, var_cxt_size1);

        int var_cxt_size2 = atoi(qe->at(3).internal.num);
        dynamic_context *cxt2 = se_new dynamic_context(st_cxt, var_cxt_size2);

        PPOpIn collection;
        dynamic_context *cxt3 = NULL;
        if (qe->size() == 7)
        {
            if (   qe->at(5).type != SCM_NUMBER
                || qe->at(6).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "311");

            int var_cxt_size3 = atoi(qe->at(5).internal.num);
            cxt3 = se_new dynamic_context(st_cxt, var_cxt_size3);

            collection = make_pp_op(cxt3, qe->at(6).internal.list);
        }


        return se_new PPBulkLoad(make_pp_op(cxt1, qe->at(2).internal.list),
                              cxt1,
                              make_pp_op(cxt2, qe->at(4).internal.list),
                              cxt2,
                              collection,
                              cxt3);
    }
    else if (op == "PPLoadModule")
    {
        if((qe->size() <= 2) || (qe->size() % 2)
            || qe->at(1).type != SCM_BOOL)
        {
           throw USER_EXCEPTION2(SE1004, "310.5");
        }
        int i;
        for (i = 2; i < qe->size(); i += 2)
        {
            if(qe->at(i).type != SCM_NUMBER)
            {
                throw USER_EXCEPTION2(SE1004, "310.6");
            }
            if(qe->at(i + 1).type != SCM_LIST)
            {
                throw USER_EXCEPTION2(SE1004, "310.7");
            }
        }

        int             var_cxt_size;
        dynamic_context *cxt;
        arr_of_PPOpIn   arr;
        for (i = 2; i < qe->size(); i += 2)
        {
            var_cxt_size    = atoi(qe->at(i).internal.num);
            cxt             = se_new dynamic_context(st_cxt, var_cxt_size);
            arr.push_back(make_pp_op(cxt, qe->at(i + 1).internal.list));
        }
        return se_new PPLoadModule(
            arr,
            qe->at(1).internal.b
            );
    }
    //{
    //    if ( qe->size() != 6
    //        || qe->at(1).type != SCM_NUMBER
    //        || qe->at(2).type != SCM_LIST
    //        || qe->at(3).type != SCM_NUMBER
    //        || qe->at(4).type != SCM_LIST
	   // || qe->at(5).type != SCM_BOOL)
    //        throw USER_EXCEPTION2(SE1004, "310.5");

    //    int var_cxt_size1 = atoi(qe->at(1).internal.num);
    //    dynamic_context *cxt1 = se_new dynamic_context(st_cxt, var_cxt_size1);

    //    int var_cxt_size2 = atoi(qe->at(3).internal.num);
    //    dynamic_context *cxt2 = se_new dynamic_context(st_cxt, var_cxt_size2);

    //    return se_new PPLoadModule(make_pp_op(cxt1, qe->at(2).internal.list),
				//				make_pp_op(cxt2, qe->at(4).internal.list),
				//				qe->at(5).internal.b,
				//				s  // is passed to this function
				//				);
    //}
    else if (op == "PPCreateDocument")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "312");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPCreateDocument(make_pp_op(cxt, qe->at(2).internal.list),
                                    cxt);
    }
    else if (op == "PPCreateCollection")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "313");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPCreateCollection(make_pp_op(cxt, qe->at(2).internal.list),
                                      cxt);
    }
    else if (op == "PPCreateDocumentInCollection")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "314");

        int var_cxt_size1 = atoi(qe->at(1).internal.num);
        dynamic_context *cxt1 = se_new dynamic_context(st_cxt, var_cxt_size1);

        int var_cxt_size2 = atoi(qe->at(3).internal.num);
        dynamic_context *cxt2 = se_new dynamic_context(st_cxt, var_cxt_size2);

        return se_new PPCreateDocumentInCollection(make_pp_op(cxt1, qe->at(2).internal.list),
                                                cxt1,
                                                make_pp_op(cxt2, qe->at(4).internal.list),
                                                cxt2);
    }
    else if (op == "PPDropDocument")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "315");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPDropDocument(make_pp_op(cxt, qe->at(2).internal.list),
                                  cxt);
    }
    else if (op == "PPDropModule")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "315.5");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPDropModule(make_pp_op(cxt, qe->at(2).internal.list));
    }
    else if (op == "PPDropCollection")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "316");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPDropCollection(make_pp_op(cxt, qe->at(2).internal.list),
                                    cxt);
    }
    else if (op == "PPDropDocumentInCollection")
    {
        if (   qe->size() != 5
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST
            || qe->at(3).type != SCM_NUMBER
            || qe->at(4).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "317");

        int var_cxt_size1 = atoi(qe->at(1).internal.num);
        dynamic_context *cxt1 = se_new dynamic_context(st_cxt, var_cxt_size1);

        int var_cxt_size2 = atoi(qe->at(3).internal.num);
        dynamic_context *cxt2 = se_new dynamic_context(st_cxt, var_cxt_size2);

        return se_new PPDropDocumentInCollection(make_pp_op(cxt1, qe->at(2).internal.list),
                                              cxt1,
                                              make_pp_op(cxt2, qe->at(4).internal.list),
                                              cxt2);
    }
    else if (op == "PPRetrieveDSForDocument")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "318");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPRetrieveDS(make_pp_op(cxt, qe->at(2).internal.list),
                                cxt,
                                dbe_document);
    }
    else if (op == "PPRetrieveDSForCollection")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "319");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPRetrieveDS(make_pp_op(cxt, qe->at(2).internal.list),
                                cxt,
                                dbe_collection);
    }
    else if (op == "PPRetrieveMetadata")
    {
        PPOpIn collection;
        db_entity_type type;
        bool b;
        dynamic_context *cxt = NULL;

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

            int var_cxt_size = atoi(qe->at(1).internal.num);
            cxt = se_new dynamic_context(st_cxt, var_cxt_size);

            collection = make_pp_op(cxt, qe->at(2).internal.list);
            type = dbe_document;
            b = qe->at(3).internal.b;
        }
        else throw USER_EXCEPTION2(SE1004, "322");

        return se_new PPRetrieveMetadata(type,
                                      collection,
                                      cxt,
                                      b);
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

        int var_cxt_size = atoi(qe->at(5).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPCreateIndex(object_path,
                                 key_path,
                                 key_type,
                                 counted_ptr<db_entity>(db_ent),
                                 make_pp_op(cxt, qe->at(6).internal.list),
                                 cxt);
    }
    else if (op == "PPDropIndex")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "324");

        int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

        return se_new PPDropIndex(make_pp_op(cxt, qe->at(2).internal.list),
                               cxt);
    }
#ifdef SE_ENABLE_FTSEARCH
    else if (op == "PPCreateFtIndex")
    {
        if (is_ft_disabled)
       		throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

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

		int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

		if (qe->size() == 7)
			return se_new PPCreateFtIndex(object_path,
				qe->at(4).internal.str, 
				counted_ptr<db_entity>(db_ent),
				make_pp_op(cxt, qe->at(5).internal.list), //index name
				make_pp_op(cxt, qe->at(6).internal.list), //cust_rules
                cxt);  
		else
			return se_new PPCreateFtIndex(object_path,
				qe->at(4).internal.str,
				counted_ptr<db_entity>(db_ent),
				make_pp_op(cxt, qe->at(5).internal.list), //index name
                cxt);
    }
    else if (op == "PPDropFtIndex")
    {
        if (is_ft_disabled)
       		throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "326");

		int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

		return se_new PPDropFtIndex(make_pp_op(cxt, qe->at(2).internal.list),
                                 cxt);
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

		int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

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
            
            return se_new PPCreateTrigger(qe->at(2).internal.symb, 			// time 
					               qe->at(3).internal.symb,			// event
                                   counted_ptr<db_entity>(db_ent),	// on (db_entity)
                                   trigger_path,					// on            
					               qe->at(6).internal.symb,			// granularity 
                                   lst->at(7).internal.list,		// action
            					   qe->at(9).internal.str,			// inserting name
            					   atoi(qe->at(10).internal.num),	// inserting type
            					   path_to_parent,					// path to parent of the inserted node
								   make_pp_op(cxt, qe->at(8).internal.list), // trigger name
                                   cxt);
        }
        else
            return se_new PPCreateTrigger(qe->at(2).internal.str, 			// time 
					               qe->at(3).internal.str,			// event
                                   counted_ptr<db_entity>(db_ent),	// on (db_entity)
                                   trigger_path,					// on            
					               qe->at(6).internal.str,			// granularity 
                                   lst->at(7).internal.list,		// action
								   make_pp_op(cxt, qe->at(8).internal.list), // trigger name
                                   cxt);
    }
    else if (op == "PPDropTrigger")
    {
        if (   qe->size() != 3
            || qe->at(1).type != SCM_NUMBER
            || qe->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "329");

		int var_cxt_size = atoi(qe->at(1).internal.num);
        dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);

		return se_new PPDropTrigger(make_pp_op(cxt, qe->at(2).internal.list),
                                 cxt);
    }
#else
	else if (   op == "PPCreateTrigger"
		     || op == "PPDropTrigger")
		throw USER_EXCEPTION2(SE1002, "Triggers support disabled. Compile Sedna with ENABLE_TRIGGERS=1 if you want to turn this feature on.");
#endif
    else throw USER_EXCEPTION2(SE1004, "399");

    return NULL;
}

void make_pp_qp(scheme_list *qp, static_context *st_cxt, int &function_counter, int &var_decl_counter)
{
    int i = 0;
    for (i = 1; i < qp->size(); i++) 
    {
        string prolog_decl(qp->at(i).internal.list->at(0).internal.symb);

        // FIXME: Add Version Declaration
        // FIXME: Add Module Declaration
        if (prolog_decl == "PPBoundarySpaceDecl")  
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(1).internal.str, "strip") == 0)
                st_cxt->boundary_space = xq_boundary_space_strip;
            else if (strcmp(qp->at(i).internal.list->at(1).internal.str, "preserve") == 0)
                st_cxt->boundary_space = xq_boundary_space_preserve;
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        else if (prolog_decl == "PPDefaultCollationDecl")
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            st_cxt->set_default_collation_uri(qp->at(i).internal.list->at(1).internal.str);
        }
        else if (prolog_decl == "PPBaseURIDecl")
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            st_cxt->set_base_uri(qp->at(i).internal.list->at(1).internal.str);
        }
        else if (prolog_decl == "PPConstructionDecl")  
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(1).internal.str, "strip") == 0)
                st_cxt->preserve_type = false;
            else if (strcmp(qp->at(i).internal.list->at(1).internal.str, "preserve") == 0)
                st_cxt->preserve_type = true;
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        else if (prolog_decl == "PPOrderingModeDecl")  
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(1).internal.str, "ordered") == 0)
                st_cxt->preserve_type = false;
            else if (strcmp(qp->at(i).internal.list->at(1).internal.str, "unordered") == 0)
                st_cxt->preserve_type = true;
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        else if (prolog_decl == "PPEmptyOrderDecl")
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(1).internal.symb, "greatest") == 0)
                st_cxt->empty_order = xq_empty_order_greatest;
            else if (strcmp(qp->at(i).internal.list->at(1).internal.symb, "least") == 0)
                st_cxt->empty_order = xq_empty_order_least;
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        else if (prolog_decl == "PPCopyNamespacesDecl")  
        {
            if (   qp->at(i).internal.list->size() != 3
                || qp->at(i).internal.list->at(1).type != SCM_STRING
                || qp->at(i).internal.list->at(2).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(1).internal.str, "preserve") == 0)
                st_cxt->cn_preserve = true;
            else if (strcmp(qp->at(i).internal.list->at(1).internal.str, "no-preserve") == 0)
                st_cxt->cn_preserve = false;
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(qp->at(i).internal.list->at(2).internal.str, "inherit") == 0)
                st_cxt->cn_inherit = true;
            else if (strcmp(qp->at(i).internal.list->at(2).internal.str, "no-inherit") == 0)
                st_cxt->cn_inherit = false;
            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
        }
        // FIXME: Add Schema Import
        // FIXME: Add Module Import
        else if (prolog_decl == "PPNSDecl") 
        {
            if (   qp->at(i).internal.list->size() != 3
                || qp->at(i).internal.list->at(1).type != SCM_STRING
                || qp->at(i).internal.list->at(2).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            // FIXME: Check lexical representation and normalize URI and NCName (or should we add this check to add_to_context)
            st_cxt->add_to_context(qp->at(i).internal.list->at(1).internal.str,
                                   qp->at(i).internal.list->at(2).internal.str);
        }
        else if (prolog_decl == "PPDefNSDeclElem")
        {
            if (   qp->at(i).internal.list->size() != 2
                || qp->at(i).internal.list->at(1).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            // FIXME: Check lexical representation and normalize URI (or should we add this check to add_to_context)
            st_cxt->add_to_context(NULL, qp->at(i).internal.list->at(1).internal.str);
        }
        else if (prolog_decl == "PPDefNSDeclFun")  
        {
            // Nothing to do because all function calls are resolved in sqp (static query processing) part
        }
        else if (prolog_decl == "PPVarDecl") 
        { 
            if (   qp->at(i).internal.list->size() < 4
                || qp->at(i).internal.list->size() > 5
                || qp->at(i).internal.list->at(1).type != SCM_NUMBER
                || qp->at(i).internal.list->at(2).type != SCM_NUMBER
                || qp->at(i).internal.list->at(3).type != SCM_LIST
               ) throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            int v_dsc = atoi(qp->at(i).internal.list->at(1).internal.num);
            int var_cxt_size = atoi(qp->at(i).internal.list->at(2).internal.num);

            dynamic_context *cxt = se_new dynamic_context(st_cxt, var_cxt_size);   // it will be freed in global_producer::~global_producer()
            PPVarIterator *opit = NULL;

            if (qp->at(i).internal.list->size() == 5)
            {
                if (qp->at(i).internal.list->at(4).type != SCM_LIST)
                    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

                opit = se_new PPVarDecl(cxt,
                                     v_dsc,
                                     make_pp_op(cxt, qp->at(i).internal.list->at(3).internal.list),
                                     make_sequence_type(qp->at(i).internal.list->at(4).internal.list));
     
            }
            else
            {
                opit = se_new PPVarDecl(cxt,
                                     v_dsc,
                                     make_pp_op(cxt, qp->at(i).internal.list->at(3).internal.list));
            }

            dynamic_context::glb_var_cxt.producers[var_decl_counter].op = opit;
            dynamic_context::glb_var_cxt.producers[var_decl_counter].cxt = cxt;
            var_decl_counter++;
        }
        else if (prolog_decl == "PPFunDecl") 
        { 
            make_pp_fun(qp->at(i).internal.list, st_cxt, dynamic_context::funct_cxt.fun_decls[function_counter]);
            function_counter++; 
        }
        else if (prolog_decl == "PPOptionDecl")  
        {
            if (qp->at(i).internal.list->size() < 2)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            // FIXME: Check lexical representation for QName

            int k = 0, q = 0;
            for (k = 1; k < qp->at(i).internal.list->size(); k++)
            {
                if (qp->at(i).internal.list->at(k).type != SCM_LIST)
                    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

                scheme_list *single = qp->at(i).internal.list->at(k).internal.list;

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
                                /// FIXME: is output method is set by API?
                                dynamic_context::output_method = se_output_method_xml;
                            }
                            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                        }
                        else if (strcmp(name, "indent") == 0)
                        {
                            if (strcmp(value, "yes") == 0) 
                            {
                                st_cxt->output_indent = se_output_indent_yes;
                            }
                            else if (strcmp(value, "no") == 0) 
                            {
                                st_cxt->output_indent = se_output_indent_no;
                            }
                            else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                        }
                        else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                    }
                    else if (strcmp(pr, SE_NAMESPACE) == 0 && strcmp(lp, "character-map") == 0)
                    {
                        // Call add_char_mapping in string matcher. We should substitute 'name' with 
                        // 'value'. Addition check could be needed for name and value
                        dynamic_context::add_char_mapping(name, value);
                    }
                    else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
                }
            }
        }
        else throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
    }
}

PPQueryEssence *scheme_list2qep(scheme_list *lst, se_ostream &s, t_print print_mode)
{
    int i = 0, j = 0;

    if (   lst->size() < 3
        || lst->at(0).type != SCM_SYMBOL 
        || string(lst->at(0).internal.symb) != "query")
        throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    int funcs_num = 0;
    int var_decls_num = 0;

    // check constraints for modules
    for (i = 1; i < lst->size() - 2; i++)
    {
        if (lst->at(i).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

        scheme_list *m = lst->at(i).internal.list;

        if (   m->size() < 1
            || m->at(0).type != SCM_SYMBOL
            || string(m->at(0).internal.symb) != "module"
           )    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

        for (j = 1; j < m->size(); j++)
        {
            if (   m->at(j).type != SCM_LIST
                || m->at(j).internal.list->size() < 1
                || m->at(j).internal.list->at(0).type != SCM_SYMBOL)
                throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

            if (strcmp(m->at(j).internal.list->at(0).internal.symb, "PPFunDecl") == 0) funcs_num++;
            if (strcmp(m->at(j).internal.list->at(0).internal.symb, "PPVarDecl") == 0) var_decls_num++;
        }

    }

    scheme_list *qp = lst->at(lst->size() - 2).internal.list;
    scheme_list *qe = lst->at(lst->size() - 1).internal.list;

    // query-prolog sublist
    if (   qp->size() < 1
        || qp->at(0).type != SCM_SYMBOL
        || string(qp->at(0).internal.symb) != "query-prolog"
       )    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    for (j = 1; j < qp->size(); j++)
    {
        if (   qp->at(j).type != SCM_LIST
            || qp->at(j).internal.list->size() < 1
            || qp->at(j).internal.list->at(0).type != SCM_SYMBOL)
            throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

        if (strcmp(qp->at(j).internal.list->at(0).internal.symb, "PPFunDecl") == 0) funcs_num++;
        if (strcmp(qp->at(j).internal.list->at(0).internal.symb, "PPVarDecl") == 0) var_decls_num++;
    }

    // set static structures of dynamic_context
    dynamic_context::static_set(funcs_num, var_decls_num, lst->size() - 2, s);

    // process all modules and query prolog
    int function_counter = 0;
    int var_decl_counter = 0;
    static_context *st_cxt = NULL;
    for (i = 1; i < lst->size() - 1; i++)
    {
        st_cxt = dynamic_context::create_static_context();

        // function has side effects: cxt, function_counter and var_decl_counter are being changed inside
        make_pp_qp(lst->at(i).internal.list, st_cxt, function_counter, var_decl_counter);
    }

    return make_pp_qe(qe, st_cxt, print_mode);
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

qep_subtree *build_qep(const char* por, int var_cxt_size)
{
    scheme_list *pp_op_in_scheme_lst = NULL;
    pp_op_in_scheme_lst = make_tree_from_scheme_list(por);

    qep_subtree *res = se_new qep_subtree();

    res->cxt = dynamic_context::create_unmanaged(var_cxt_size);
    res->tree = make_pp_op(res->cxt, pp_op_in_scheme_lst);

    delete_scheme_list(pp_op_in_scheme_lst);

    return res;
}

qep_subtree *build_qep(scheme_list* por, int var_cxt_size)
{
    qep_subtree *res = se_new qep_subtree();

    res->cxt = dynamic_context::create_unmanaged(var_cxt_size);
    res->tree = make_pp_op(res->cxt, por);

    return res;
}


void delete_qep(PPQueryEssence *qep)
{
    delete qep;

    dynamic_context::static_clear();
}

void delete_qep(qep_subtree *qep)
{
    dynamic_context::destroy_unmanaged(qep->cxt);
    qep->cxt = NULL;
    delete (qep->tree.op);
    qep->tree.op = NULL;
    delete qep;
    qep = NULL;
}



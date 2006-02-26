/*
 * File:  att.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "att.h"
#include "por2qep.h"

using namespace std;


xp_op *make_xp_op(scheme_list *lst)
{
    if (lst->size() == 0) return NULL;

    if (lst->at(0).type != SCM_SYMBOL) 
        throw USER_EXCEPTION2(SE1004, "501");

    string op_name = string(lst->at(0).internal.symb);

    xp_op *op = new xp_op();

    if (op_name == "path")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "502");

        op->type = xp_op_path;
        op->path_expr = lr2PathExpr(NULL, lst->at(1).internal.list, false);
        op->op1 = make_xp_op(lst->at(2).internal.list);
    }
    else if (op_name == "pred")
    {
        if (   lst->size() != 5
            || lst->at(1).type != SCM_NUMBER
            || lst->at(2).type != SCM_LIST
            || lst->at(2).internal.list->size() != 2
            || lst->at(2).internal.list->at(0).type != SCM_STRING
            || lst->at(2).internal.list->at(1).type != SCM_STRING
            || lst->at(3).type != SCM_LIST
            || lst->at(4).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "503");

        op->type = xp_op_pred;
        op->pred_num = atoi(lst->at(1).internal.num);
        op->qname.Prefix.n = new char[strlen(lst->at(2).internal.list->at(0).internal.str) + 1];
        strcpy(op->qname.Prefix.n, lst->at(2).internal.list->at(0).internal.str);
        op->qname.LocalPart.n = new char[strlen(lst->at(2).internal.list->at(1).internal.str) + 1];
        strcpy(op->qname.LocalPart.n, lst->at(2).internal.list->at(1).internal.str);
        op->op1 = make_xp_op(lst->at(3).internal.list);
        op->op2 = make_xp_op(lst->at(4).internal.list);
    }
    else if (op_name == "and@" || op_name == "or@")
    {
        if (   lst->size() < 2
           ) throw USER_EXCEPTION2(SE1004, "504");

        int i = 0;
        for (i = 1; i < lst->size(); i++)
        {
            if (lst->at(i).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "505");
        }

        op->type = (op_name == "and@" ? xp_op_and : xp_op_or);
        op->ops_size = lst->size() - 1;
        op->ops = new xp_op* [op->ops_size];

        for (i = 1; i < lst->size(); i++)
        {
            op->ops[i - 1] = make_xp_op(lst->at(i).internal.list);
        }
    }
    else if (op_name == "const")
    {
        if (lst->size() != 3
           ) throw USER_EXCEPTION2(SE1004, "506");

        op->type = xp_op_const;
        op->tc = new tuple_cell(make_const(lst->at(1), lst->at(2)));
    }
    else if (   op_name == "=@"
             || op_name == "!=@"
             || op_name == "<@"
             || op_name == ">@"
             || op_name == "<=@"
             || op_name == ">=@"
             || op_name == "eq@"
             || op_name == "ne@"
             || op_name == "lt@"
             || op_name == "le@"
             || op_name == "gt@"
             || op_name == "ge@")
    {
        if (   lst->size() != 3
            || lst->at(1).type != SCM_LIST
            || lst->at(2).type != SCM_LIST
           ) throw USER_EXCEPTION2(SE1004, "507");


        if (op_name == "=@")		op->type = xp_op_gen_eq;
        else if (op_name == "!=@")	op->type = xp_op_gen_ne;
        else if (op_name == "<@")	op->type = xp_op_gen_lt;
        else if (op_name == ">@")	op->type = xp_op_gen_gt;
        else if (op_name == "<=@")	op->type = xp_op_gen_le;
        else if (op_name == ">=@")	op->type = xp_op_gen_ge;
        else if (op_name == "eq@")	op->type = xp_op_val_eq;
        else if (op_name == "ne@")	op->type = xp_op_val_ne;
        else if (op_name == "lt@")	op->type = xp_op_val_lt;
        else if (op_name == "le@")	op->type = xp_op_val_le;
        else if (op_name == "gt@")	op->type = xp_op_val_gt;
        else if (op_name == "ge@")	op->type = xp_op_val_ge;

        op->op1 = make_xp_op(lst->at(1).internal.list);
        op->op2 = make_xp_op(lst->at(2).internal.list);
    }
    else throw USER_EXCEPTION2(SE1004, "600");

    return op;
}

xpath_attr *make_xpath_attr(scheme_list *lst)
{
    if (   lst->size() != 4
        || lst->at(0).type != SCM_SYMBOL 
        || lst->at(1).type != SCM_LIST
        || lst->at(2).type != SCM_NUMBER
        || lst->at(3).type != SCM_LIST
       )    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    xpath_attr *xpath = new xpath_attr();
    xpath->db_ent = counted_ptr<db_entity>(make_db_entity(lst->at(1).internal.list));
    xpath->xp_preds_num = atoi(lst->at(2).internal.num);
    xpath->op = make_xp_op(lst->at(3).internal.list);

    return xpath;
}

att_attr *make_att_attr(scheme_list *lst)
{
    if (   lst->size() < 2
        || lst->at(0).type != SCM_SYMBOL 
       )    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    const char *attr_type = lst->at(0).internal.symb;

    if (strcmp(attr_type, "xpath") == 0)
    {
        att_attr *res = new att_attr();
        res->type = att_xpath;
        res->xpath = make_xpath_attr(lst);
        return res;
    }

    throw USER_EXCEPTION2(SE1004, "Wrong top level representation");
}


void delete_xp_op(xp_op *op)
{
    if (!op) return;

    switch (op->type)
    {
        case xp_op_path		:
            delete_xp_op(op->op1);
            break;
        case xp_op_pred		:
            delete_xp_op(op->op1);
            delete_xp_op(op->op2);
            delete [] op->qname.Prefix.n;
            delete [] op->qname.LocalPart.n;
            break;
        case xp_op_and		:
        case xp_op_or		:
            {
                int i = 0;
                for (i = 0; i < op->ops_size; i++)
                    delete_xp_op(op->ops[i]);
                delete [] (op->ops);
                break;
            }
        case xp_op_const	:
            delete op->tc;
            break;
        case xp_op_gen_eq	:
        case xp_op_gen_ne	:
        case xp_op_gen_lt	:
        case xp_op_gen_gt	:
        case xp_op_gen_le	:
        case xp_op_gen_ge	:
        case xp_op_val_eq	:
        case xp_op_val_ne	:
        case xp_op_val_lt	:
        case xp_op_val_gt	:
        case xp_op_val_le	:
        case xp_op_val_ge	:
            delete_xp_op(op->op1);
            delete_xp_op(op->op2);
            break;
        default				: throw USER_EXCEPTION2(SE1004, "Wrong top level representation");

    }

    delete op;
}

void delete_xpath_attr(xpath_attr *xpath)
{
    if (!xpath) return;

    delete_xp_op(xpath->op);
    delete xpath;
}

void delete_att_attr(att_attr *att)
{
    if (!att) return;

    switch (att->type)
    {
        case att_xpath: delete_xpath_attr(att->xpath);
                        delete att;
                        break;
        default: USER_EXCEPTION2(SE1004, "Wrong top level representation");
    }
}

void xp_op::print()
{
    int i = 0;
    switch (type)
    {
        case xp_op_path		:
            path_expr->print();
            if (op1)
            {
                printf("(");
                op1->print();
                printf(")");
            }
            break;
        case xp_op_pred		:
            printf("pred");
            break;
        case xp_op_and		:
            printf("(");
            ops[0]->print();
            for (i = 1; i < ops_size; i++)
            {
                printf(") and (");
                ops[i]->print();
            }
            printf(")");
            break;
        case xp_op_or		:
            printf("(");
            ops[0]->print();
            for (i = 1; i < ops_size; i++)
            {
                printf(") or (");
                ops[i]->print();
            }
            printf(")");
            break;
        case xp_op_const	:
            tc->print(false);
            break;
        case xp_op_gen_eq	: 
            op1->print(); 
            printf(" = "); 
            op2->print();
            break;
        case xp_op_gen_ne	:
            op1->print(); 
            printf(" != "); 
            op2->print(); 
            break;
        case xp_op_gen_lt	:
            op1->print(); 
            printf(" < "); 
            op2->print(); 
            break;
        case xp_op_gen_gt	:
            op1->print(); 
            printf(" > "); 
            op2->print(); 
            break;
        case xp_op_gen_le	:
            op1->print(); 
            printf(" <= "); 
            op2->print(); 
            break;
        case xp_op_gen_ge	:
            op1->print(); 
            printf(" >= "); 
            op2->print(); 
            break;
        case xp_op_val_eq	:
            op1->print(); 
            printf(" eq "); 
            op2->print(); 
            break;
        case xp_op_val_ne	:
            op1->print(); 
            printf(" ne "); 
            op2->print(); 
            break;
        case xp_op_val_lt	:
            op1->print(); 
            printf(" lt "); 
            op2->print(); 
            break;
        case xp_op_val_gt	:
            op1->print(); 
            printf(" gt "); 
            op2->print(); 
            break;
        case xp_op_val_le	:
            op1->print(); 
            printf(" <= "); 
            op2->print(); 
            break;
        case xp_op_val_ge	:
            op1->print(); 
            printf(" >= "); 
            op2->print(); 
            break;
        default: printf("UNKNOWN");
    }
}


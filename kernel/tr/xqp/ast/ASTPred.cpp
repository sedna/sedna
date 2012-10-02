/*
 * File:  ASTPred.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "tr/xqp/ast/ASTBop.h"
#include "ASTPred.h"

void ASTPred::separateConjuncts(ASTNode *expr)
{
    ASTBop *bop;

    if ((bop = dynamic_cast<ASTBop *>(expr)) && bop->op == ASTBop::AND)
    {
        separateConjuncts(bop->lop);
        separateConjuncts(bop->rop);

        bop->lop = NULL;
        bop->rop = NULL;

        delete expr;
    }
    else
    {
        ASTConjunct c;

        c.expr = expr;
        c.use_cxt = true;   //
        c.use_last = true;  // don't care for now; lreturn will analyze this
        c.use_pos = true;   //

        this->others.push_back(c);
    }
}

ASTPred::ASTPred(const ASTNodeCommonData &loc, ASTNode *expr_) : ASTNode(loc)
{
    // we need to separate conjuncts out from expression
    separateConjuncts(expr_);
}

ASTPred::~ASTPred()
{
    std::vector<ASTConjunct>::iterator it;

    for (it = conjuncts.begin(); it != conjuncts.end(); it++)
        delete it->expr;

    for (it = others.begin(); it != others.end(); it++)
        delete it->expr;
}

void ASTPred::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

bool ASTPred::usePosition() const
{
    std::vector<ASTConjunct>::const_iterator it;

    for (it = conjuncts.begin(); it != conjuncts.end(); it++)
        if (it->use_pos)
            return true;

    for (it = others.begin(); it != others.end(); it++)
        if (it->use_pos)
            return true;

    return false;
}
bool ASTPred::useLast() const
{
    std::vector<ASTConjunct>::const_iterator it;

    for (it = conjuncts.begin(); it != conjuncts.end(); it++)
        if (it->use_last)
            return true;

    for (it = others.begin(); it != others.end(); it++)
        if (it->use_last)
            return true;

    return false;
}

ASTNode *ASTPred::dup()
{
    ASTPred *res = new ASTPred(cd);

    std::vector<ASTConjunct>::iterator it;

    for (it = conjuncts.begin(); it != conjuncts.end(); it++)
    {
        ASTConjunct c;

        c.expr = it->expr->dup();
        c.use_cxt = it->use_cxt;
        c.use_last = it->use_last;
        c.use_pos = it->use_pos;

        res->conjuncts.push_back(c);
    }

    for (it = others.begin(); it != others.end(); it++)
    {
        ASTConjunct c;

        c.expr = it->expr->dup();
        c.use_cxt = it->use_cxt;
        c.use_last = it->use_last;
        c.use_pos = it->use_pos;

        res->others.push_back(c);
    }

    return res;
}

ASTNode *ASTPred::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    unsigned int i;
    ASTPred *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);

    res = new ASTPred(cd);

    for (i = 0; i < sl[2].internal.list->size(); i++)
    {
        U_ASSERT(sl[2].internal.list->at(i).type == SCM_LIST);
        scheme_list &l = *(sl[2].internal.list->at(i).internal.list);
        ASTConjunct c;

        U_ASSERT(l[0].type == SCM_LIST && l[1].type == SCM_NUMBER && l[2].type == SCM_BOOL && l[3].type == SCM_BOOL && l[4].type == SCM_BOOL);

        c.expr = dsGetASTFromSchemeList(*l[0].internal.list);
        c.op = operation_compare_condition(atoi(l[1].internal.num));
        c.use_cxt = l[2].internal.b;
        c.use_last = l[3].internal.b;
        c.use_pos = l[4].internal.b;

        res->conjuncts.push_back(c);
    }

    for (i = 0; i < sl[3].internal.list->size(); i++)
    {
        U_ASSERT(sl[3].internal.list->at(i).type == SCM_LIST);
        scheme_list &l = *(sl[3].internal.list->at(i).internal.list);
        ASTConjunct c;

        U_ASSERT(l[0].type == SCM_LIST && l[1].type == SCM_NUMBER && l[2].type == SCM_BOOL && l[3].type == SCM_BOOL && l[4].type == SCM_BOOL);

        c.expr = dsGetASTFromSchemeList(*l[0].internal.list);
        c.op = operation_compare_condition(atoi(l[1].internal.num));
        c.use_cxt = l[2].internal.b;
        c.use_last = l[3].internal.b;
        c.use_pos = l[4].internal.b;

        res->others.push_back(c);
    }

    return res;
}

void ASTPred::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    std::vector<ASTConjunct>::iterator it;

    for (it = conjuncts.begin(); it != conjuncts.end(); it++)
    {
        if (it->expr == oldc)
        {
            it->expr = newc;
            return;
        }
    }

    for (it = others.begin(); it != others.end(); it++)
    {
        if (it->expr == oldc)
        {
            it->expr = newc;
            return;
        }
    }
}

void ASTPred::seriliazeConjuncts(std::string &str, ASTVisitor &v) const
{
    std::vector<ASTConjunct>::const_iterator it;

    str += "(";
    for (it = conjuncts.begin(); it != conjuncts.end(); it++)
    {
        str += "(";
        it->expr->accept(v);

        str += cast_to_string<operation_compare_condition>(it->op);

        if (it->use_cxt)
            str += " #t";
        else
            str += " #f";
        if (it->use_last)
            str += " #t";
        else
            str += " #f";
        if (it->use_pos)
            str += " #t";
        else
            str += " #f";
        str += ")";
    }
    str += ")";

    str += "(";
    for (it = others.begin(); it != others.end(); it++)
    {
        str += "(";
        it->expr->accept(v);

        str += cast_to_string<operation_compare_condition>(it->op);

        if (it->use_cxt)
            str += " #t";
        else
            str += " #f";
        if (it->use_last)
            str += " #t";
        else
            str += " #f";
        if (it->use_pos)
            str += " #t";
        else
            str += " #f";
        str += ")";
    }
    str += ")";
}

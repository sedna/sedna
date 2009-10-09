/*
 * File:  ASTIf.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTIf.h"

ASTIf::~ASTIf()
{
    delete i_expr;
    delete t_expr;
    delete e_expr;
}

void ASTIf::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode* ASTIf::dup()
{
    return new ASTIf(cd, i_expr->dup(), t_expr->dup(), e_expr->dup());
}

ASTNode *ASTIf::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *i = NULL, *t = NULL, *e = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    i = dsGetASTFromSchemeList(*sl[2].internal.list);
    t = dsGetASTFromSchemeList(*sl[3].internal.list);
    e = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTIf(cd, i, t, e);
}

void ASTIf::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (i_expr == oldc)
    {
        i_expr = newc;
        return;
    }
    if (t_expr == oldc)
    {
        t_expr = newc;
        return;
    }
    if (e_expr == oldc)
    {
        e_expr = newc;
        return;
    }
}

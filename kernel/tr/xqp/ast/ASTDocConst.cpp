/*
 * File:  ASTDocConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDocConst.h"

ASTDocConst::~ASTDocConst()
{
    delete expr;
}

void ASTDocConst::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDocConst::dup()
{
    return new ASTDocConst(cd, expr->dup());
}

ASTNode *ASTDocConst::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *expr = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTDocConst(cd, expr);
}

void ASTDocConst::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}

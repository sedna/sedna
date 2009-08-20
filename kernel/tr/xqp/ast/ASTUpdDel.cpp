/*
 * File:  ASTUpdDel.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdDel.h"

ASTUpdDel::~ASTUpdDel()
{
    delete what;
}

void ASTUpdDel::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTUpdDel::dup()
{
    return new ASTUpdDel(loc, what->dup(), type);
}

ASTNode *ASTUpdDel::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;
    DelType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = DelType(atol(sl[3].internal.num));

    return new ASTUpdDel(loc, expr, type);
}

void ASTUpdDel::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (what == oldc)
    {
        what = newc;
        return;
    }
}

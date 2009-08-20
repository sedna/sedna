/*
 * File:  ASTUpdReplace.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdReplace.h"

ASTUpdReplace::~ASTUpdReplace()
{
    delete what;
    delete new_expr;
}

void ASTUpdReplace::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTUpdReplace::dup()
{
    return new ASTUpdReplace(loc, what->dup(), static_cast<ASTFunDef *>(new_expr->dup()));
}

ASTNode *ASTUpdReplace::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *what = NULL, *newe = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    what = dsGetASTFromSchemeList(*sl[2].internal.list);
    newe = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTUpdReplace(loc, what, newe);
}

void ASTUpdReplace::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (what == oldc)
    {
        what = newc;
        return;
    }
    if (new_expr == oldc)
    {
        new_expr = newc;
        return;
    }
}

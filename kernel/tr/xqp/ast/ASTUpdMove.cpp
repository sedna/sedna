/*
 * File:  ASTUpdMove.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdMove.h"

ASTUpdMove::~ASTUpdMove()
{
    delete what;
    delete where;
}

void ASTUpdMove::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTUpdMove::dup()
{
    return new ASTUpdMove(loc, what->dup(), where->dup(), type);
}

ASTNode *ASTUpdMove::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *what = NULL, *where = NULL;
    UpdType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    what = dsGetASTFromSchemeList(*sl[2].internal.list);
    where = dsGetASTFromSchemeList(*sl[3].internal.list);
    type = UpdType(atol(sl[4].internal.num));

    return new ASTUpdMove(loc, what, where, type);
}

void ASTUpdMove::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (what == oldc)
    {
        what = newc;
        return;
    }
    if (where == oldc)
    {
        where = newc;
        return;
    }
}

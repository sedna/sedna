/*
 * File:  ASTUpdMove.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdMove.h"

ASTUpdMove::~ASTUpdMove()
{
    delete var;
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
    return new ASTUpdMove(cd, var->dup(), what->dup(), where->dup(), type);
}

ASTNode *ASTUpdMove::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *var = NULL, *what = NULL, *where = NULL;
    UpdType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    var = dsGetASTFromSchemeList(*sl[2].internal.list);
    what = dsGetASTFromSchemeList(*sl[3].internal.list);
    where = dsGetASTFromSchemeList(*sl[4].internal.list);
    type = UpdType(atol(sl[5].internal.num));

    return new ASTUpdMove(cd, var, what, where, type);
}

void ASTUpdMove::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (var == oldc)
    {
        what = newc;
        return;
    }
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

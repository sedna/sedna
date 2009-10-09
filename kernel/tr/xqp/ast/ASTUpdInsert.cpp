/*
 * File:  ASTUpdInsert.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdInsert.h"

ASTUpdInsert::~ASTUpdInsert()
{
    delete what;
    delete where;
}

void ASTUpdInsert::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTUpdInsert::dup()
{
    return new ASTUpdInsert(cd, what->dup(), where->dup(), type);
}

ASTNode *ASTUpdInsert::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *what = NULL, *where = NULL;
    UpdType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    what = dsGetASTFromSchemeList(*sl[2].internal.list);
    where = dsGetASTFromSchemeList(*sl[3].internal.list);
    type = UpdType(atol(sl[4].internal.num));

    return new ASTUpdInsert(cd, what, where, type);
}

void ASTUpdInsert::modifyChild(const ASTNode *oldc, ASTNode *newc)
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

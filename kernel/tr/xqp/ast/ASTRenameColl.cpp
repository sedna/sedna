/*
 * File:  ASTRenameColl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRenameColl.h"

ASTRenameColl::~ASTRenameColl()
{
    delete name_old;
    delete name_new;
}

void ASTRenameColl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTRenameColl::dup()
{
    return new ASTRenameColl(cd, name_old->dup(), name_new->dup());
}

ASTNode *ASTRenameColl::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *nold = NULL, *nnew = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    nold = dsGetASTFromSchemeList(*sl[2].internal.list);
    nnew = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTRenameColl(cd, nold, nnew);
}

void ASTRenameColl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (name_old == oldc)
    {
        name_old = newc;
        return;
    }
    if (name_new == oldc)
    {
        name_new = newc;
        return;
    }
}

/*
 * File:  ASTCase.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCase.h"

ASTCase::~ASTCase()
{
    delete type;
    delete fd;
}

void ASTCase::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCase::dup()
{
    return new ASTCase(loc, (type) ? type->dup() : NULL, fd->dup());
}

ASTNode *ASTCase::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *type = NULL, *fd = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    type = dsGetASTFromSchemeList(*sl[2].internal.list);
    fd = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTCase(loc, type, fd);
}

void ASTCase::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (type == oldc)
    {
        type = newc;
        return;
    }
    if (fd == oldc)
    {
        fd = newc;
        return;
    }
}

/*
 * File:  ASTTypeVar.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeVar.h"

ASTTypeVar::~ASTTypeVar()
{
    delete type;
    delete var;
}

void ASTTypeVar::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTTypeVar::dup()
{
    return new ASTTypeVar(loc, type->dup(), var->dup());
}

ASTNode *ASTTypeVar::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *var = NULL, *type = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    type = dsGetASTFromSchemeList(*sl[2].internal.list);
    var = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTTypeVar(loc, type, var);
}

void ASTTypeVar::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (type == oldc)
    {
        type = newc;
        return;
    }
    if (var == oldc)
    {
        var = newc;
        return;
    }
}

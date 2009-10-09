/*
 * File:  ASTTypeSingle.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeSingle.h"

ASTTypeSingle::~ASTTypeSingle()
{
    delete type;
}

void ASTTypeSingle::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTTypeSingle::dup()
{
    return new ASTTypeSingle(cd, type->dup(), mod);
}

ASTNode *ASTTypeSingle::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *type = NULL;
    OccurMod mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    type = dsGetASTFromSchemeList(*sl[2].internal.list);
    mod = OccurMod(atol(sl[3].internal.num));

    return new ASTTypeSingle(cd, type, mod);
}

void ASTTypeSingle::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (type == oldc)
    {
        type = newc;
        return;
    }
}

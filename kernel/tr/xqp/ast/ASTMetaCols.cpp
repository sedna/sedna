/*
 * File:  ASTMetaCols.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaCols.h"

void ASTMetaCols::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTMetaCols::dup()
{
    return new ASTMetaCols(cd, need_stats);
}

ASTNode *ASTMetaCols::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    bool mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_BOOL);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    mod = sl[2].internal.b;

    return new ASTMetaCols(cd, mod);
}

void ASTMetaCols::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

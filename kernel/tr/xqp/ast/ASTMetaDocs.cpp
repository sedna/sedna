/*
 * File:  ASTMetaDocs.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaDocs.h"

ASTMetaDocs::~ASTMetaDocs()
{
    delete coll;
}

void ASTMetaDocs::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTMetaDocs::dup()
{
    return new ASTMetaDocs(loc, (coll) ? coll->dup() : NULL, need_stats);
}

ASTNode *ASTMetaDocs::createNode(scheme_list &sl)
{
    ASTLocation loc;
    bool mod;
    ASTNode *col;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_BOOL && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    mod = sl[2].internal.b;
    col = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTMetaDocs(loc, col, mod);
}

void ASTMetaDocs::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (coll == oldc)
    {
        coll = newc;
        return;
    }
}

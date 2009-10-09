/*
 * File:  ASTTypeSeq.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeSeq.h"

ASTTypeSeq::~ASTTypeSeq()
{
    delete type_test;
}

void ASTTypeSeq::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTTypeSeq::dup()
{
    return new ASTTypeSeq(cd, type_test->dup(), mod);
}

ASTNode *ASTTypeSeq::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *type = NULL;
    OccurMod mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    type = dsGetASTFromSchemeList(*sl[2].internal.list);
    mod = OccurMod(atol(sl[3].internal.num));

    return new ASTTypeSeq(cd, type, mod);
}

void ASTTypeSeq::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (type_test == oldc)
    {
        type_test = newc;
        return;
    }
}

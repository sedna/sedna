/*
 * File:  ASTBop.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBop.h"

ASTBop::~ASTBop()
{
    delete lop;
    delete rop;
}

void ASTBop::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode* ASTBop::dup()
{
    return new ASTBop(cd, op, lop->dup(), rop->dup());
}

ASTNode *ASTBop::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *lop = NULL, *rop = NULL;
    Oper op;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    op = Oper(atoi(sl[2].internal.num));

    lop = dsGetASTFromSchemeList(*sl[3].internal.list);
    rop = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTBop(cd, op, lop, rop);
}

void ASTBop::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (lop == oldc)
    {
        lop = newc;
        return;
    }
    if (rop == oldc)
    {
        rop = newc;
        return;
    }
}

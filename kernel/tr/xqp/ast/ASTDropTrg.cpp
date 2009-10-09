/*
 * File:  ASTDropTrg.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropTrg.h"

ASTDropTrg::~ASTDropTrg()
{
    delete trg;
}

void ASTDropTrg::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropTrg::dup()
{
    return new ASTDropTrg(cd, new std::string(*trg));
}

ASTNode *ASTDropTrg::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *trg = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    trg = new std::string(sl[2].internal.str);

    return new ASTDropTrg(cd, trg);
}

void ASTDropTrg::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

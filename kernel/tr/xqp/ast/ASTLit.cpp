/*
 * File:  ASTLit.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLit.h"

ASTLit::~ASTLit()
{
    delete lit;
}

void ASTLit::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTLit::dup()
{
    return new ASTLit(cd, type, new std::string(*lit));
}

ASTNode *ASTLit::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *lit = NULL;
    LitType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    lit = new std::string(sl[2].internal.str);
    type = LitType(atol(sl[3].internal.num));

    return new ASTLit(cd, type, lit);
}

void ASTLit::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

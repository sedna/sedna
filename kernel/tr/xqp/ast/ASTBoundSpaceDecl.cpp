/*
 * File:  ASTBoundSpaceDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBoundSpaceDecl.h"

void ASTBoundSpaceDecl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTBoundSpaceDecl::dup()
{
    return new ASTBoundSpaceDecl(loc, mod);
}

ASTNode *ASTBoundSpaceDecl::createNode(scheme_list &sl)
{
    ASTLocation loc;
    opt mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    mod = opt(atoi(sl[2].internal.num));

    return new ASTBoundSpaceDecl(loc, mod);
}

void ASTBoundSpaceDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

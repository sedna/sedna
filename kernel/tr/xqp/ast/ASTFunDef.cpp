/*
 * File:  ASTFunDef.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFunDef.h"

ASTFunDef::~ASTFunDef()
{
    destroyASTNodesVector(vars);
    delete fun;
}

void ASTFunDef::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTFunDef::dup()
{
    return new ASTFunDef(loc, duplicateASTNodes(vars), fun->dup());
}

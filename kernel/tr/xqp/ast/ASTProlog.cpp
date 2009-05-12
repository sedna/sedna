/*
 * File:  ASTProlog.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTProlog.h"

ASTProlog::~ASTProlog()
{
    destroyASTNodesVector(decls);
}

void ASTProlog::addPrologDecl(ASTNode *decl)
{
    decls->push_back(decl);
}

void ASTProlog::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTProlog::dup()
{
    return new ASTProlog(loc, duplicateASTNodes(decls));
}

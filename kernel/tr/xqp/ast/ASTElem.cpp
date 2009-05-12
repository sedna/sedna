/*
 * File:  ASTElem.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTElem.h"

ASTElem::~ASTElem()
{
    delete pref;
    delete local;
    destroyASTNodesVector(attrs);
    destroyASTNodesVector(cont);
}

void ASTElem::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTElem::dup()
{
    return new ASTElem(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(attrs), duplicateASTNodes(cont));
}

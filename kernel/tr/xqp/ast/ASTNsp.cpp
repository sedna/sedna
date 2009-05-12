/*
 * File:  ASTNsp.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNsp.h"

ASTNsp::~ASTNsp()
{
    delete name;
    destroyASTNodesVector(cont);
}

void ASTNsp::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTNsp::dup()
{
    return new ASTNsp(loc, new std::string(*name), duplicateASTNodes(cont));
}

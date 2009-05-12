/*
 * File:  ASTScript.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTScript.h"

ASTScript::~ASTScript()
{
    destroyASTNodesVector(modules);
}

void ASTScript::addModule(ASTNode *mod)
{
    modules->push_back(mod);
}

void ASTScript::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTScript::dup()
{
    return new ASTScript(loc, duplicateASTNodes(modules));
}

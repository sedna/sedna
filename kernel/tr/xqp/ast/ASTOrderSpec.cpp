/*
 * File:  ASTOrderSpec.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderSpec.h"

ASTOrderSpec::~ASTOrderSpec()
{
    delete expr;
    delete mod;
}

void ASTOrderSpec::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderSpec::dup()
{
    return new ASTOrderSpec(loc, expr->dup(), (mod) ? static_cast<ASTOrderMod *>(mod->dup()) : NULL);
}

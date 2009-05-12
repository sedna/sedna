/*
 * File:  ASTDropMod.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropMod.h"

ASTDropMod::~ASTDropMod()
{
    delete module;
}

void ASTDropMod::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropMod::dup()
{
    return new ASTDropMod(loc, new std::string(*module));
}

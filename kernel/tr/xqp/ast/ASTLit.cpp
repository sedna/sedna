/*
 * File:  ASTLit.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLit.h"

ASTLit::~ASTLit()
{
    delete lit;
}

void ASTLit::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTLit::dup()
{
    return new ASTLit(loc, type, new std::string(*lit));
}

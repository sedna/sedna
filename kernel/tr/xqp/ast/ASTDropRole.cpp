/*
 * File:  ASTDropRole.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropRole.h"

ASTDropRole::~ASTDropRole()
{
    delete role;
}

void ASTDropRole::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropRole::dup()
{
    return new ASTDropRole(loc, new std::string(*role));
}

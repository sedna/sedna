/*
 * File:  ASTRevokeRole.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRevokeRole.h"

ASTRevokeRole::~ASTRevokeRole()
{
    delete role;
    delete role_from;
}

void ASTRevokeRole::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTRevokeRole::dup()
{
    return new ASTRevokeRole(loc, new std::string(*role), new std::string(*role_from));
}

/*
 * File:  ASTAlterUser.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAlterUser.h"

ASTAlterUser::~ASTAlterUser()
{
    delete user;
    delete psw;
}

void ASTAlterUser::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAlterUser::dup()
{
    return new ASTAlterUser(loc, new std::string(*user), new std::string(*psw));
}

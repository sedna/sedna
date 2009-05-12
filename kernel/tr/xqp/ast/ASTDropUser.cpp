/*
 * File:  ASTDropUser.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropUser.h"

ASTDropUser::~ASTDropUser()
{
    delete user;
}

void ASTDropUser::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropUser::dup()
{
    return new ASTDropUser(loc, new std::string(*user));
}

/*
 * File:  ASTOption.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOption.h"

ASTOption::~ASTOption()
{
    delete pref;
    delete local;
    delete opt;
}

void ASTOption::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOption::dup()
{
    return new ASTOption(loc, new std::string(*pref), new std::string(*local), new std::string(*opt));
}

/*
 * File:  ASTVar.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVar.h"

ASTVar::~ASTVar()
{
    delete pref;
    delete local;
}

void ASTVar::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTVar::dup()
{
    return new ASTVar(loc, new std::string(*pref), new std::string(*local));
}

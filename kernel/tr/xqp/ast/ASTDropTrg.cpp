/*
 * File:  ASTDropTrg.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropTrg.h"

ASTDropTrg::~ASTDropTrg()
{
    delete trg;
}

void ASTDropTrg::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropTrg::dup()
{
    return new ASTDropTrg(loc, new std::string(*trg));
}

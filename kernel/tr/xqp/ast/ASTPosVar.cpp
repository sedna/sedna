/*
 * File:  ASTPosVar.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPosVar.h"

ASTPosVar::~ASTPosVar()
{
    delete var;
}

void ASTPosVar::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTPosVar::dup()
{
    return new ASTPosVar(loc, static_cast<ASTVar *>(var->dup()));
}

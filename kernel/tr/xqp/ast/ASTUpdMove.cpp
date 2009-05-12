/*
 * File:  ASTUpdMove.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdMove.h"

ASTUpdMove::~ASTUpdMove()
{
    delete what;
    delete where;
}

void ASTUpdMove::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTUpdMove::dup()
{
    return new ASTUpdMove(loc, what->dup(), static_cast<ASTFunDef *>(where->dup()), type);
}

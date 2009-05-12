/*
 * File:  ASTVarDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVarDecl.h"

ASTVarDecl::~ASTVarDecl()
{
    delete var;
    delete type;
    delete expr;
}

void ASTVarDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTVarDecl::dup()
{
    return new ASTVarDecl(loc, static_cast<ASTVar *>(var->dup()), (type == NULL) ? NULL: static_cast<ASTTypeSeq *>(type->dup()), (expr == NULL) ? NULL: expr->dup());
}

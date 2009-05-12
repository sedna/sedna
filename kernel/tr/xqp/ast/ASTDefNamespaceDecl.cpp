/*
 * File:  ASTDefNamespaceDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDefNamespaceDecl.h"

ASTDefNamespaceDecl::~ASTDefNamespaceDecl()
{
    delete uri;
}

void ASTDefNamespaceDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDefNamespaceDecl::dup()
{
    return new ASTDefNamespaceDecl(loc, new std::string(*uri), type);
}

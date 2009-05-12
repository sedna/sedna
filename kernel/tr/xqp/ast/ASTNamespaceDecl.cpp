/*
 * File:  ASTNamespaceDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNamespaceDecl.h"

ASTNamespaceDecl::~ASTNamespaceDecl()
{
    delete name;
    delete uri;
}

void ASTNamespaceDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTNamespaceDecl::dup()
{
    return new ASTNamespaceDecl(loc, new std::string(*name), new std::string(*uri));
}

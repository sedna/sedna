/*
 * File:  ASTBaseURI.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBaseURI.h"

ASTBaseURI::~ASTBaseURI()
{
    delete uri;
}

void ASTBaseURI::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTBaseURI::dup()
{
    return new ASTBaseURI(loc, new std::string(*uri));
}

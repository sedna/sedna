/*
 * File:  ASTPi.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPi.h"

ASTPi::~ASTPi()
{
    delete name;
    delete cont;
}

void ASTPi::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTPi::dup()
{
    return new ASTPi(loc, new std::string(*name), (cont) ? new std::string(*cont) : NULL);
}

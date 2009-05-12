/*
 * File:  ASTElemConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTElemConst.h"

ASTElemConst::~ASTElemConst()
{
    delete name;
    delete pref;
    delete local;
    delete expr;
}

void ASTElemConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTElemConst::dup()
{
    if (pref)
        return new ASTElemConst(loc, new std::string(*pref), new std::string(*local), (expr) ? expr->dup() : NULL);

    return new ASTElemConst(loc, name->dup(), (expr) ? expr->dup() : NULL);
}

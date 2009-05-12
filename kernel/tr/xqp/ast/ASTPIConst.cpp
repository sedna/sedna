/*
 * File:  ASTPIConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPIConst.h"

ASTPIConst::~ASTPIConst()
{
    delete name;
    delete ncname;
    delete expr;
}

void ASTPIConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTPIConst::dup()
{
    if (ncname)
        return new ASTPIConst(loc, new std::string(*ncname), (expr) ? expr->dup() : NULL);

    return new ASTPIConst(loc, name->dup(), (expr) ? expr->dup() : NULL);
}

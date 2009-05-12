/*
 * File:  ASTAttrConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAttrConst.h"

ASTAttrConst::~ASTAttrConst()
{
    delete name;
    delete pref;
    delete local;
    delete expr;
}

void ASTAttrConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAttrConst::dup()
{
    if (pref)
        return new ASTAttrConst(loc, new std::string(*pref), new std::string(*local), (expr) ? expr->dup() : NULL);

    return new ASTAttrConst(loc, name->dup(), (expr) ? expr->dup() : NULL);
}

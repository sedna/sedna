/*
 * File:  ASTDropMod.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropMod.h"

ASTDropMod::~ASTDropMod()
{
    delete module;
}

void ASTDropMod::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropMod::dup()
{
    return new ASTDropMod(loc, new std::string(*module));
}

ASTNode *ASTDropMod::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *mod = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    mod = new std::string(sl[2].internal.str);

    return new ASTDropMod(loc, mod);
}

void ASTDropMod::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

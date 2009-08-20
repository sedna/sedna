/*
 * File:  ASTDropRole.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropRole.h"

ASTDropRole::~ASTDropRole()
{
    delete role;
}

void ASTDropRole::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropRole::dup()
{
    return new ASTDropRole(loc, new std::string(*role));
}

ASTNode *ASTDropRole::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *role = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    role = new std::string(sl[2].internal.str);

    return new ASTDropRole(loc, role);
}

void ASTDropRole::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

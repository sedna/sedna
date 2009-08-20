/*
 * File:  ASTGrantRole.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTGrantRole.h"

ASTGrantRole::~ASTGrantRole()
{
    delete role;
    delete role_to;
}

void ASTGrantRole::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTGrantRole::dup()
{
    return new ASTGrantRole(loc, new std::string(*role), new std::string(*role_to));
}

ASTNode *ASTGrantRole::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *role_f = NULL, *role_to = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    role_f = new std::string(sl[2].internal.str);
    role_to = new std::string(sl[3].internal.str);

    return new ASTGrantRole(loc, role_f, role_to);
}

void ASTGrantRole::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

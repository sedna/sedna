/*
 * File:  ASTRevokeRole.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRevokeRole.h"

ASTRevokeRole::~ASTRevokeRole()
{
    delete role;
    delete role_from;
}

void ASTRevokeRole::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTRevokeRole::dup()
{
    return new ASTRevokeRole(loc, new std::string(*role), new std::string(*role_from));
}

ASTNode *ASTRevokeRole::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *role_f = NULL, *role = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    role = new std::string(sl[2].internal.str);
    role_f = new std::string(sl[3].internal.str);

    return new ASTRevokeRole(loc, role, role_f);
}

void ASTRevokeRole::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

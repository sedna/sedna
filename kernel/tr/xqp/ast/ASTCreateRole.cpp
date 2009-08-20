/*
 * File:  ASTCreateRole.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateRole.h"

ASTCreateRole::~ASTCreateRole()
{
    delete role;
}

void ASTCreateRole::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCreateRole::dup()
{
    return new ASTCreateRole(loc, new std::string(*role));
}

ASTNode *ASTCreateRole::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *role = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    role = new std::string(sl[2].internal.str);

    return new ASTCreateRole(loc, role);
}

void ASTCreateRole::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

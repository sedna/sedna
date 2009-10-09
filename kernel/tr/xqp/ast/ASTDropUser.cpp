/*
 * File:  ASTDropUser.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropUser.h"

ASTDropUser::~ASTDropUser()
{
    delete user;
}

void ASTDropUser::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropUser::dup()
{
    return new ASTDropUser(cd, new std::string(*user));
}

ASTNode *ASTDropUser::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *user = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    user = new std::string(sl[2].internal.str);

    return new ASTDropUser(cd, user);
}

void ASTDropUser::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

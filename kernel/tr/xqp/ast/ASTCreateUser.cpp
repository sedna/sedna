/*
 * File:  ASTCreateUser.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateUser.h"

ASTCreateUser::~ASTCreateUser()
{
    delete user;
    delete psw;
}

void ASTCreateUser::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCreateUser::dup()
{
    return new ASTCreateUser(cd, new std::string(*user), new std::string(*psw));
}

ASTNode *ASTCreateUser::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *user = NULL, *psw = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    user = new std::string(sl[2].internal.str);
    psw = new std::string(sl[3].internal.str);

    return new ASTCreateUser(cd, user, psw);
}

void ASTCreateUser::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

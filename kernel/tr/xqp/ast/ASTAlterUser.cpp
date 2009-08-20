/*
 * File:  ASTAlterUser.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAlterUser.h"

ASTAlterUser::~ASTAlterUser()
{
    delete user;
    delete psw;
}

void ASTAlterUser::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTAlterUser::dup()
{
    return new ASTAlterUser(loc, new std::string(*user), new std::string(*psw));
}

ASTNode *ASTAlterUser::createNode(scheme_list &sl)
{
    std::string *user, *psw;
    ASTLocation loc;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    user = new std::string(sl[2].internal.str);
    psw = new std::string(sl[3].internal.str);

    return new ASTAlterUser(loc, user, psw);
}

void ASTAlterUser::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

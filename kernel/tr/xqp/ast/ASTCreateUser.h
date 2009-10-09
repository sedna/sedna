/*
 * File:  ASTCreateUser.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_USER_H_
#define _AST_CREATE_USER_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTCreateUser : public ASTNode
{
public:
    std::string *user, *psw;

public:
    ASTCreateUser(const ASTNodeCommonData &loc, std::string *user_, std::string *psw_) : ASTNode(loc), user(user_), psw(psw_) {}

    ~ASTCreateUser();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

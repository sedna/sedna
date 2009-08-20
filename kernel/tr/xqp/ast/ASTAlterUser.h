/*
 * File:  ASTAlterUser.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ALTER_USER_H_
#define _AST_ALTER_USER_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTAlterUser : public ASTNode
{
public:
    std::string *user, *psw;

public:
    ASTAlterUser(ASTLocation &loc, std::string *user_, std::string *psw_) : ASTNode(loc), user(user_), psw(psw_) {}

    ~ASTAlterUser();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTCreateRole.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_ROLE_H_
#define _AST_CREATE_ROLE_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTCreateRole : public ASTNode
{
public:
    std::string *role;

public:
    ASTCreateRole(ASTLocation &loc, std::string *role_) : ASTNode(loc), role(role_) {}

    ~ASTCreateRole();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

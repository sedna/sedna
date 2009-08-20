/*
 * File:  ASTGrantRole.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_GRANT_ROLE_H_
#define _AST_GRANT_ROLE_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTGrantRole : public ASTNode
{
public:
    std::string *role, *role_to;

public:
    ASTGrantRole(ASTLocation &loc, std::string *r, std::string *rt) : ASTNode(loc), role(r), role_to(rt) {}

    ~ASTGrantRole();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

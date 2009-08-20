/*
 * File:  ASTDropRole.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DROP_ROLE_H_
#define _AST_DROP_ROLE_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTDropRole : public ASTNode
{
public:
    std::string *role;

public:
    ASTDropRole(ASTLocation &loc, std::string *role_) : ASTNode(loc), role(role_) {}

    ~ASTDropRole();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTRevokeRole.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_REVOKE_ROLE_H_
#define _AST_REVOKE_ROLE_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTRevokeRole : public ASTNode
{
public:
    std::string *role, *role_from;

public:
    ASTRevokeRole(const ASTNodeCommonData &loc, std::string *r, std::string *rf) : ASTNode(loc), role(r), role_from(rf) {}

    ~ASTRevokeRole();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

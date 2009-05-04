#ifndef _AST_REVOKE_ROLE_H_
#define _AST_REVOKE_ROLE_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTRevokeRole : public ASTNode
{
public:
    std::string *role, *role_from;

public:
    ASTRevokeRole(ASTLocation &loc, std::string *r, std::string *rf) : ASTNode(loc), role(r), role_from(rf) {}

    ~ASTRevokeRole();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

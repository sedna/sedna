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
};

#endif

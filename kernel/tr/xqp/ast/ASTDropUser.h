#ifndef _AST_DROP_USER_H_
#define _AST_DROP_USER_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTDropUser : public ASTNode
{
public:
    std::string *user;

public:
    ASTDropUser(ASTLocation &loc, std::string *user_) : ASTNode(loc), user(user_) {}

    ~ASTDropUser();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

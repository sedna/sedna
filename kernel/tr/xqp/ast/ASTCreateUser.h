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
    ASTCreateUser(ASTLocation &loc, std::string *user_, std::string *psw_) : ASTNode(loc), user(user_), psw(psw_) {}

    ~ASTCreateUser();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

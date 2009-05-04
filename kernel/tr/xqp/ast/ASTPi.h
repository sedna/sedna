#ifndef _AST_PI_H_
#define _AST_PI_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTPi : public ASTNode
{
public:
    std::string *name; // pi name
    std::string *cont; // pi content

public:
    ASTPi(ASTLocation &loc, std::string *name_, std::string *cont_) : ASTNode(loc), name(name_), cont(cont_) {}

    ~ASTPi();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

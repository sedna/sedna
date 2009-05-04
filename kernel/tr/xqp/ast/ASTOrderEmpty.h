#ifndef _AST_ORDER_EMPTY_H_
#define _AST_ORDER_EMPTY_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrderEmpty : public ASTNode
{
public:
    enum opt
    {
        EMPTY_LEAST,
        EMPTY_GREATEST
    };

    ASTOrderEmpty::opt mod; // ordered/unordered modificator

public:
    ASTOrderEmpty(ASTLocation &loc, ASTOrderEmpty::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTOrderEmpty() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

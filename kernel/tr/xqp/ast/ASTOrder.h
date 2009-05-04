#ifndef _AST_ORDER_H_
#define _AST_ORDER_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrder : public ASTNode
{
public:
    enum opt
    {
        ORDERED,
        UNORDERED
    };

    ASTOrder::opt mod; // ordered/unordered modificator

public:
    ASTOrder(ASTLocation &loc, ASTOrder::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTOrder() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
};


#endif

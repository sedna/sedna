#ifndef _AST_UOP_H_
#define _AST_UOP_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUop : public ASTNode
{
public:
    enum Oper
    {
        PLUS,
        MINUS
    };

    ASTNode *expr;
    Oper op;

public:
    ASTUop(ASTLocation &loc, ASTUop::Oper oper, ASTNode *uexpr) : ASTNode(loc), expr(uexpr), op(oper) {}
    ~ASTUop();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

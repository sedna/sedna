#ifndef _AST_ORD_EXPR_H_
#define _AST_ORD_EXPR_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrdExpr : public ASTNode
{
public:
    enum OrdType
    {
        ORDERED,
        UNORDERED
    };

    OrdType type;
    ASTNode *expr;

public:
    ASTOrdExpr(ASTLocation &loc, OrdType type_, ASTNode *expr_) : ASTNode(loc), type(type_), expr(expr_) {}

    ~ASTOrdExpr();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

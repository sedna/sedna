#ifndef _AST_PRED_H_
#define _AST_PRED_H_

#include "ASTNode.h"
#include "AST.h"

class ASTPred : public ASTNode
{
public:
    ASTNode *iter_expr, *pred_expr;

public:
    ASTPred(ASTLocation &loc, ASTNode *iter_expr_, ASTNode *pred_expr_) : ASTNode(loc), iter_expr(iter_expr_), pred_expr(pred_expr_) {}

    ~ASTPred();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

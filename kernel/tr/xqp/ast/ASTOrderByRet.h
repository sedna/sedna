#ifndef _AST_ORDER_BY_RET_H_
#define _AST_ORDER_BY_RET_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrderByRet : public ASTNode
{
public:
    ASTNode *iter_expr, *ret_expr;

public:
    ASTOrderByRet(ASTLocation &loc, ASTNode *iter_expr_, ASTNode *ret_expr_) : ASTNode(loc), iter_expr(iter_expr_), ret_expr(ret_expr_) {}

    ~ASTOrderByRet();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

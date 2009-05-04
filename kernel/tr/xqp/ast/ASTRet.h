#ifndef _AST_RET_H_
#define _AST_RET_H_

#include "ASTNode.h"
#include "AST.h"

class ASTRet : public ASTNode
{
public:
    ASTNode *iter_expr, *ret_expr;

public:
    ASTRet(ASTLocation &loc, ASTNode *iter_expr_, ASTNode *ret_expr_) : ASTNode(loc), iter_expr(iter_expr_), ret_expr(ret_expr_) {}

    ~ASTRet();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

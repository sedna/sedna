#ifndef _AST_EXT_EXPR_H_
#define _AST_EXT_EXPR_H_

#include "ASTNode.h"
#include "AST.h"

class ASTExtExpr : public ASTNode
{

public:
    ASTNodesVector *pragmas;
    ASTNode *expr; // may be NULL

public:
    ASTExtExpr(ASTLocation &loc, ASTNodesVector *pragmas_, ASTNode *expr_) : ASTNode(loc), pragmas(pragmas_), expr(expr_) {}
    ~ASTExtExpr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

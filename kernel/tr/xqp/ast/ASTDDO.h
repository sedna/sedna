#ifndef _AST_DDO_H_
#define _AST_DDO_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDDO : public ASTNode
{
public:
    ASTNode *expr;

public:
    ASTDDO(ASTLocation &loc, ASTNode *expr_) : ASTNode(loc), expr(expr_) {}

    ~ASTDDO();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

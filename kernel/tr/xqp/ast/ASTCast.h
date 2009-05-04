#ifndef _AST_CAST_H_
#define _AST_CAST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeSingle;

class ASTCast : public ASTNode
{
public:
    ASTNode *expr;
    ASTTypeSingle *type;

public:
    ASTCast(ASTLocation &loc, ASTNode *cast_expr, ASTTypeSingle *cast_type) : ASTNode(loc), expr(cast_expr), type(cast_type) {}
    ~ASTCast();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

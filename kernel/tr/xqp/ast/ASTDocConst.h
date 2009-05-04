#ifndef _AST_DOC_CONST_H_
#define _AST_DOC_CONST_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTDocConst : public ASTNode
{
public:
    ASTNode *expr; // computed construction expression

public:
    ASTDocConst(ASTLocation &loc, ASTNode *expr_) : ASTNode(loc), expr(expr_) {}

    ~ASTDocConst();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

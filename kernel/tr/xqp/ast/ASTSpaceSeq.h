#ifndef _AST_SPACE_SEQ_H_
#define _AST_SPACE_SEQ_H_

#include "ASTNode.h"
#include "AST.h"

class ASTSpaceSeq : public ASTNode
{
public:
    ASTNode *expr;

public:
    ASTSpaceSeq(ASTLocation &loc, ASTNode *expr_) : ASTNode(loc), expr(expr_) {}

    ~ASTSpaceSeq();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

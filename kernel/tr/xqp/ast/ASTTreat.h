#ifndef _AST_TREAT_H_
#define _AST_TREAT_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeSeq;

class ASTTreat : public ASTNode
{
public:
    ASTNode *expr;
    ASTTypeSeq *type;

public:
    ASTTreat(ASTLocation &loc, ASTNode *tr_expr, ASTTypeSeq *tr_type) : ASTNode(loc), expr(tr_expr), type(tr_type) {}
    ~ASTTreat();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

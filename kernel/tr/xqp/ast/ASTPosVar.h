#ifndef _AST_POS_VAR_H_
#define _AST_POS_VAR_H_

#include "ASTNode.h"
#include "AST.h"

class ASTVar;

class ASTPosVar : public ASTNode
{
public:
    ASTVar *var;

public:
    ASTPosVar(ASTLocation &loc, ASTVar *pos_var) : ASTNode(loc), var(pos_var) {}

    ~ASTPosVar();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

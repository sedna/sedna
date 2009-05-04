#ifndef _AST_UNIO_H_
#define _AST_UNIO_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUnio : public ASTNode
{
public:
    ASTNodesVector *vars;

public:
    ASTUnio(ASTLocation &loc, ASTNodesVector *vars_) : ASTNode(loc), vars(vars_) {}

    ~ASTUnio();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

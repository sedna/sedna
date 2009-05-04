#ifndef _AST_ERROR_NODE_H_
#define _AST_ERROR_NODE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTError : public ASTNode
{
public:
    ASTError(ASTLocation &loc) : ASTNode(loc) {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

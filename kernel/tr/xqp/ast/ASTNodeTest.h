#ifndef _AST_NODE_TEST_H_
#define _AST_NODE_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTNodeTest : public ASTNode
{
public:
    ASTNodeTest(ASTLocation &loc) : ASTNode(loc) {}

    ~ASTNodeTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

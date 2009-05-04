#ifndef _AST_COMM_TEST_H_
#define _AST_COMM_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTCommTest : public ASTNode
{
public:
    ASTCommTest(ASTLocation &loc) : ASTNode(loc) {}

    ~ASTCommTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

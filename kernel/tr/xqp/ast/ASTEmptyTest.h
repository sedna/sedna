#ifndef _AST_EMPTY_TEST_H_
#define _AST_EMPTY_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTEmptyTest : public ASTNode
{
public:
    ASTEmptyTest(ASTLocation &loc) : ASTNode(loc) {}

    ~ASTEmptyTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

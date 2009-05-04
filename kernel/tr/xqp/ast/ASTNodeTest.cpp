#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNodeTest.h"

void ASTNodeTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTNodeTest::dup()
{
    return new ASTNodeTest(loc);
}

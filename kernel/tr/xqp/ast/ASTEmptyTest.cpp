#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTEmptyTest.h"

void ASTEmptyTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTEmptyTest::dup()
{
    return new ASTEmptyTest(loc);
}

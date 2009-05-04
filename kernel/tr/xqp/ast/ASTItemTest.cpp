#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTItemTest.h"

void ASTItemTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTItemTest::dup()
{
    return new ASTItemTest(loc);
}

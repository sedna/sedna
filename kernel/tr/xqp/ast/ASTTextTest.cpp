#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTextTest.h"

void ASTTextTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTextTest::dup()
{
    return new ASTTextTest(loc);
}

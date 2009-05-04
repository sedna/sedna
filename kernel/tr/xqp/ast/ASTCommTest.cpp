#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCommTest.h"

void ASTCommTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCommTest::dup()
{
    return new ASTCommTest(loc);
}

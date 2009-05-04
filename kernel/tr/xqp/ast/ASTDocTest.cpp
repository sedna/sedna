#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDocTest.h"

ASTDocTest::~ASTDocTest()
{
    delete elem_test;
}

void ASTDocTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDocTest::dup()
{
    return new ASTDocTest(loc, (elem_test) ? elem_test->dup() : NULL);
}

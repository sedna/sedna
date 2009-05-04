#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPiTest.h"

ASTPiTest::~ASTPiTest()
{
    delete test;
}

void ASTPiTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTPiTest::dup()
{
    return new ASTPiTest(loc, (test) ? new std::string(*test) : NULL, type);
}

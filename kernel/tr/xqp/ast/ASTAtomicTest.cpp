#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAtomicTest.h"

ASTAtomicTest::~ASTAtomicTest()
{
    delete name;
}

void ASTAtomicTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAtomicTest::dup()
{
    return new ASTAtomicTest(loc, new std::string(*name));
}

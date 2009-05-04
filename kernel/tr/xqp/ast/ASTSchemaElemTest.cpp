#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSchemaElemTest.h"

ASTSchemaElemTest::~ASTSchemaElemTest()
{
    delete npref;
    delete nloc;
}

void ASTSchemaElemTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTSchemaElemTest::dup()
{
    return new ASTSchemaElemTest(loc,
                             (npref) ? new std::string(*npref) : NULL,
                             (nloc) ? new std::string(*nloc) : NULL
                            );
}

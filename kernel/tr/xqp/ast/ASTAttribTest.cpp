#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAttribTest.h"

ASTAttribTest::~ASTAttribTest()
{
    delete npref;
    delete nloc;

    delete tpref;
    delete tloc;
}

void ASTAttribTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAttribTest::dup()
{
    return new ASTAttribTest(loc,
                             (npref) ? new std::string(*npref) : NULL,
                             (nloc) ? new std::string(*nloc) : NULL,
                             (tpref) ? new std::string(*tpref) : NULL,
                             (tloc) ? new std::string(*tloc) : NULL
                            );
}

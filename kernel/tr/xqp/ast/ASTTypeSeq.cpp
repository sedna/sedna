#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeSeq.h"

ASTTypeSeq::~ASTTypeSeq()
{
    delete type_name;
    delete type_test;
}

void ASTTypeSeq::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTypeSeq::dup()
{
    if (type_name)
        return new ASTTypeSeq(loc, new std::string(*type_name), mod);

    return new ASTTypeSeq(loc, type_test->dup(), mod);
}

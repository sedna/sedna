#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuery.h"

ASTQuery::~ASTQuery()
{
    delete query;
}

void ASTQuery::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTQuery::dup()
{
    return new ASTQuery(loc, query->dup(), type);
}

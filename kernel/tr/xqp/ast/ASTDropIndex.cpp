#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropIndex.h"

ASTDropIndex::~ASTDropIndex()
{
    delete index;
}

void ASTDropIndex::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropIndex::dup()
{
    return new ASTDropIndex(loc, index->dup());
}

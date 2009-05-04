#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropFtIndex.h"

ASTDropFtIndex::~ASTDropFtIndex()
{
    delete index;
}

void ASTDropFtIndex::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropFtIndex::dup()
{
    return new ASTDropFtIndex(loc, index->dup());
}

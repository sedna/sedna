#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaDocs.h"

ASTMetaDocs::~ASTMetaDocs()
{
    delete coll;
}

void ASTMetaDocs::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTMetaDocs::dup()
{
    return new ASTMetaDocs(loc, (coll) ? coll->dup() : NULL, need_stats);
}

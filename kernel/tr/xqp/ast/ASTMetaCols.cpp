#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaCols.h"

void ASTMetaCols::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTMetaCols::dup()
{
    return new ASTMetaCols(loc, need_stats);
}

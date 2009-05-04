#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrder.h"

void ASTOrder::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrder::dup()
{
    return new ASTOrder(loc, mod);
}

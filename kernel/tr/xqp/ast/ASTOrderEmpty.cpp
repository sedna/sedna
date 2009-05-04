#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderEmpty.h"

void ASTOrderEmpty::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderEmpty::dup()
{
    return new ASTOrderEmpty(loc, mod);
}

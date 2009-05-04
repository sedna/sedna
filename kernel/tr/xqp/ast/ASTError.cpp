#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTError.h"

void ASTError::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTError::dup()
{
    return new ASTError(loc);
}

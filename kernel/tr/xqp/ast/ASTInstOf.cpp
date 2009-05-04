#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTInstOf.h"

ASTInstOf::~ASTInstOf()
{
    delete expr;
    delete type;
}

void ASTInstOf::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTInstOf::dup()
{
    return new ASTInstOf(loc, expr->dup(), static_cast<ASTTypeSeq *>(type->dup()));
}

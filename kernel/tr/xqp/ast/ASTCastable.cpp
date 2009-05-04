#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCastable.h"

ASTCastable::~ASTCastable()
{
    delete expr;
    delete type;
}

void ASTCastable::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCastable::dup()
{
    return new ASTCastable(loc, expr->dup(), static_cast<ASTTypeSingle *>(type->dup()));
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCast.h"

ASTCast::~ASTCast()
{
    delete expr;
    delete type;
}

void ASTCast::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCast::dup()
{
    return new ASTCast(loc, expr->dup(), static_cast<ASTTypeSingle *>(type->dup()));
}

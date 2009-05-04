#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTreat.h"

ASTTreat::~ASTTreat()
{
    delete expr;
    delete type;
}

void ASTTreat::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTreat::dup()
{
    return new ASTTreat(loc, expr->dup(), static_cast<ASTTypeSeq *>(type->dup()));
}

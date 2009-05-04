#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrdExpr.h"

ASTOrdExpr::~ASTOrdExpr()
{
    delete expr;
}

void ASTOrdExpr::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrdExpr::dup()
{
    return new ASTOrdExpr(loc, type, expr->dup());
}

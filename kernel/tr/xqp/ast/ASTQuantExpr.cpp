#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuantExpr.h"

ASTQuantExpr::~ASTQuantExpr()
{
    delete expr;
    delete fd;
}

void ASTQuantExpr::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTQuantExpr::dup()
{
    return new ASTQuantExpr(loc, expr->dup(), static_cast<ASTFunDef *>(fd->dup()), type);
}

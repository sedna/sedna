#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPred.h"

ASTPred::~ASTPred()
{
    delete iter_expr;
    delete pred_expr;
}

void ASTPred::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTPred::dup()
{
    return new ASTPred(loc, iter_expr->dup(), pred_expr->dup());
}

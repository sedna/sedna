#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderByRet.h"

ASTOrderByRet::~ASTOrderByRet()
{
    delete iter_expr;
    delete ret_expr;
}

void ASTOrderByRet::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderByRet::dup()
{
    return new ASTOrderByRet(loc, iter_expr->dup(), ret_expr->dup());
}

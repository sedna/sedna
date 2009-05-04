#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTIf.h"

ASTIf::~ASTIf()
{
    delete i_expr;
    delete t_expr;
    delete e_expr;
}

void ASTIf::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode* ASTIf::dup()
{
    return new ASTIf(loc, i_expr->dup(), t_expr->dup(), e_expr->dup());
}

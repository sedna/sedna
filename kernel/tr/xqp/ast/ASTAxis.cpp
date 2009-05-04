#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAxis.h"

ASTAxis::~ASTAxis()
{
    delete expr;
    delete test;
}

void ASTAxis::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAxis::dup()
{
    return new ASTAxis(loc, axis, expr->dup(), test->dup());
}

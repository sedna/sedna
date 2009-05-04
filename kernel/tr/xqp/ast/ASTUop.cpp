#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUop.h"

ASTUop::~ASTUop()
{
    delete expr;
}

void ASTUop::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode* ASTUop::dup()
{
    return new ASTUop(loc, op, expr->dup());
}

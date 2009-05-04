#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDDO.h"

ASTDDO::~ASTDDO()
{
    delete expr;
}

void ASTDDO::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDDO::dup()
{
    return new ASTDDO(loc, expr->dup());
}

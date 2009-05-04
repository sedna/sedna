#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTextConst.h"

ASTTextConst::~ASTTextConst()
{
    delete expr;
}

void ASTTextConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTextConst::dup()
{
    return new ASTTextConst(loc, expr->dup());
}

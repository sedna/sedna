#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSpaceSeq.h"

ASTSpaceSeq::~ASTSpaceSeq()
{
    delete expr;
}

void ASTSpaceSeq::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTSpaceSeq::dup()
{
    return new ASTSpaceSeq(loc, expr->dup());
}

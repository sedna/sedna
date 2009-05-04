#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCommentConst.h"

ASTCommentConst::~ASTCommentConst()
{
    delete expr;
}

void ASTCommentConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCommentConst::dup()
{
    return new ASTCommentConst(loc, expr->dup());
}

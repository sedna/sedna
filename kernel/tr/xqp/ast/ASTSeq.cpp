#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSeq.h"

ASTSeq::~ASTSeq()
{
    destroyASTNodesVector(exprs);
}

void ASTSeq::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTSeq::dup()
{
    return new ASTSeq(loc, duplicateASTNodes(exprs));
}

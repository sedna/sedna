#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFilterStep.h"

ASTFilterStep::~ASTFilterStep()
{
    delete expr;
    destroyASTNodesVector(preds);
}

void ASTFilterStep::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTFilterStep::dup()
{
    return new ASTFilterStep(loc, expr->dup(), duplicateASTNodes(preds));
}

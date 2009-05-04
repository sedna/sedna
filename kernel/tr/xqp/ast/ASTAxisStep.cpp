#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAxisStep.h"

ASTAxisStep::~ASTAxisStep()
{
    delete test;
    destroyASTNodesVector(preds);
}

void ASTAxisStep::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAxisStep::dup()
{
    return new ASTAxisStep(loc, axis, test->dup(), duplicateASTNodes(preds));
}

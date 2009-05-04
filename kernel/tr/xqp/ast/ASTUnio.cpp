#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUnio.h"

ASTUnio::~ASTUnio()
{
    destroyASTNodesVector(vars);
}

void ASTUnio::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTUnio::dup()
{
    return new ASTUnio(loc, duplicateASTNodes(vars));
}

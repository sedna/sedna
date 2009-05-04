#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFunDef.h"

ASTFunDef::~ASTFunDef()
{
    destroyASTNodesVector(vars);
    delete fun;
}

void ASTFunDef::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTFunDef::dup()
{
    return new ASTFunDef(loc, duplicateASTNodes(vars), fun->dup());
}

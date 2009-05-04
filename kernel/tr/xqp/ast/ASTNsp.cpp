#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNsp.h"

ASTNsp::~ASTNsp()
{
    delete name;
    destroyASTNodesVector(cont);
}

void ASTNsp::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTNsp::dup()
{
    return new ASTNsp(loc, new std::string(*name), duplicateASTNodes(cont));
}

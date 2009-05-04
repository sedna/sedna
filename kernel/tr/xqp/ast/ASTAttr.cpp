#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAttr.h"

ASTAttr::~ASTAttr()
{
    delete pref;
    delete local;
    destroyASTNodesVector(cont);
}

void ASTAttr::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAttr::dup()
{
    return new ASTAttr(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(cont));
}

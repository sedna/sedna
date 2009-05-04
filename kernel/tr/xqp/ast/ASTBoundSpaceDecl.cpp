#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBoundSpaceDecl.h"

void ASTBoundSpaceDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTBoundSpaceDecl::dup()
{
    return new ASTBoundSpaceDecl(loc, mod);
}

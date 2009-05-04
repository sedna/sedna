#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTConstDecl.h"

void ASTConstDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTConstDecl::dup()
{
    return new ASTConstDecl(loc, mod);
}

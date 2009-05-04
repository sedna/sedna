#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderModInt.h"

ASTOrderModInt::~ASTOrderModInt()
{
    delete uri;
}

void ASTOrderModInt::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderModInt::dup()
{
    return new ASTOrderModInt(loc, mod, (uri) ? new std::string(*uri) : NULL);
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBaseURI.h"

ASTBaseURI::~ASTBaseURI()
{
    delete uri;
}

void ASTBaseURI::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTBaseURI::dup()
{
    return new ASTBaseURI(loc, new std::string(*uri));
}

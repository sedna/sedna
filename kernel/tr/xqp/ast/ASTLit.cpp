#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLit.h"

ASTLit::~ASTLit()
{
    delete lit;
}

void ASTLit::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTLit::dup()
{
    return new ASTLit(loc, type, new std::string(*lit));
}

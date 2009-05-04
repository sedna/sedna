#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdDel.h"

ASTUpdDel::~ASTUpdDel()
{
    delete what;
}

void ASTUpdDel::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTUpdDel::dup()
{
    return new ASTUpdDel(loc, what->dup(), type);
}

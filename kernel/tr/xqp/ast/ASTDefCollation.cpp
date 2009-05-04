#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDefCollation.h"

ASTDefCollation::~ASTDefCollation()
{
    delete uri;
}

void ASTDefCollation::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDefCollation::dup()
{
    return new ASTDefCollation(loc, new std::string(*uri));
}

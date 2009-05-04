#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropRole.h"

ASTDropRole::~ASTDropRole()
{
    delete role;
}

void ASTDropRole::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropRole::dup()
{
    return new ASTDropRole(loc, new std::string(*role));
}

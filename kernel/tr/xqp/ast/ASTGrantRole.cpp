#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTGrantRole.h"

ASTGrantRole::~ASTGrantRole()
{
    delete role;
    delete role_to;
}

void ASTGrantRole::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTGrantRole::dup()
{
    return new ASTGrantRole(loc, new std::string(*role), new std::string(*role_to));
}

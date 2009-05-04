#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateRole.h"

ASTCreateRole::~ASTCreateRole()
{
    delete role;
}

void ASTCreateRole::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateRole::dup()
{
    return new ASTCreateRole(loc, new std::string(*role));
}

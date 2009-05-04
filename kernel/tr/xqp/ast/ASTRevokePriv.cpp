#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRevokePriv.h"

ASTRevokePriv::~ASTRevokePriv()
{
    delete priv;
    delete obj;
    delete user;
}

void ASTRevokePriv::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTRevokePriv::dup()
{
    return new ASTRevokePriv(loc, new std::string(*priv), (obj) ? new std::string(*obj) : NULL, new std::string(*user), mod);
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTGrantPriv.h"

ASTGrantPriv::~ASTGrantPriv()
{
    delete priv;
    delete obj;
    delete user;
}

void ASTGrantPriv::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTGrantPriv::dup()
{
    return new ASTGrantPriv(loc, new std::string(*priv), (obj) ? new std::string(*obj) : NULL, new std::string(*user), mod);
}

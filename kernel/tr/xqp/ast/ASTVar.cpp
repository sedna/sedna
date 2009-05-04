#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVar.h"

ASTVar::~ASTVar()
{
    delete pref;
    delete local;
}

void ASTVar::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTVar::dup()
{
    return new ASTVar(loc, new std::string(*pref), new std::string(*local));
}

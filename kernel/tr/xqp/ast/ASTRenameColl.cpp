#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRenameColl.h"

ASTRenameColl::~ASTRenameColl()
{
    delete name_old;
    delete name_new;
}

void ASTRenameColl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTRenameColl::dup()
{
    return new ASTRenameColl(loc, name_old->dup(), name_new->dup());
}

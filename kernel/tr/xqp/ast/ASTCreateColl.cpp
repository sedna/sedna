#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateColl.h"

ASTCreateColl::~ASTCreateColl()
{
    delete coll;
}

void ASTCreateColl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateColl::dup()
{
    return new ASTCreateColl(loc, coll->dup());
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdRename.h"

ASTUpdRename::~ASTUpdRename()
{
    delete what;
    delete pref;
    delete local;
}

void ASTUpdRename::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTUpdRename::dup()
{
    return new ASTUpdRename(loc, what->dup(), new std::string(*pref), new std::string(*local));
}

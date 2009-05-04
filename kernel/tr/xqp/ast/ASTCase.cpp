#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCase.h"

ASTCase::~ASTCase()
{
    delete type;
    delete fd;
}

void ASTCase::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCase::dup()
{
    return new ASTCase(loc, (type) ? static_cast<ASTTypeSeq *>(type->dup()) : NULL, static_cast<ASTFunDef *>(fd->dup()));
}

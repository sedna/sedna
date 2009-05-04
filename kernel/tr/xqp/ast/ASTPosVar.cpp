#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPosVar.h"

ASTPosVar::~ASTPosVar()
{
    delete var;
}

void ASTPosVar::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTPosVar::dup()
{
    return new ASTPosVar(loc, static_cast<ASTVar *>(var->dup()));
}

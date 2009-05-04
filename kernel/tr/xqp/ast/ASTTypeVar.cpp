#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeVar.h"

ASTTypeVar::~ASTTypeVar()
{
    delete type_name;
    delete type_seq;
    delete var;
}

void ASTTypeVar::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTypeVar::dup()
{
    if (type_name)
        return new ASTTypeVar(loc, new std::string(*type_name), static_cast<ASTVar *>(var->dup()));

    return new ASTTypeVar(loc, static_cast<ASTTypeSeq *>(type_seq->dup()), static_cast<ASTVar *>(var->dup()));
}

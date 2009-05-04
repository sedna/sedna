#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLet.h"

ASTLet::~ASTLet()
{
    delete tv;
    delete expr;
    delete fd;
}

void ASTLet::accept(ASTVisitor &v)
{
    v.visit(*this);
}

void ASTLet::setFunDef(ASTFunDef *funDef)
{
    delete fd;

    fd = funDef;
}

ASTNodesVector *ASTLet::getVarList()
{
    ASTNodesVector *res;

    res = new ASTNodesVector();

    res->push_back(tv->dup());

    return res;
}

ASTNode *ASTLet::dup()
{
    ASTLet *res;

    res = new ASTLet(loc, static_cast<ASTTypeVar *>(tv->dup()), expr->dup());

    if (fd)
        res->setFunDef(static_cast<ASTFunDef *>(fd->dup()));

    return res;
}

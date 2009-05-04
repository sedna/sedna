#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFor.h"

ASTFor::~ASTFor()
{
    delete tv;
    delete pv;
    delete expr;
    delete fd;
}

void ASTFor::accept(ASTVisitor &v)
{
    v.visit(*this);
}

void ASTFor::setFunDef(ASTFunDef *funDef)
{
    delete fd;

    fd = funDef;
}

ASTNodesVector *ASTFor::getVarList()
{
    ASTNodesVector *res;

    res = new ASTNodesVector();

    res->push_back(tv->dup());

    if (pv)
        res->push_back(pv->dup());

    return res;
}

ASTNode *ASTFor::dup()
{
    ASTFor *res;

    res = new ASTFor(loc, static_cast<ASTTypeVar *>(tv->dup()), (pv) ? static_cast<ASTPosVar *>(pv->dup()) : NULL, expr->dup());

    if (fd)
        res->setFunDef(static_cast<ASTFunDef *>(fd->dup()));

    return res;
}

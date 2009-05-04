#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLibModule.h"

ASTLibModule::~ASTLibModule()
{
    delete prolog;
    delete moduleDecl;
}

void ASTLibModule::setVersionDecl(ASTNode *vd)
{
    prolog->addVersionDecl(vd);
}

void ASTLibModule::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTLibModule::dup()
{
    return new ASTLibModule(loc, static_cast<ASTModuleDecl *>(moduleDecl->dup()), static_cast<ASTProlog *>(prolog->dup()));
}

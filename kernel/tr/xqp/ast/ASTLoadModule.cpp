#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLoadModule.h"

ASTLoadModule::~ASTLoadModule()
{
    destroyASTStringVector(modules);
}

void ASTLoadModule::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTLoadModule::dup()
{
    return new ASTLoadModule(loc, duplicateASTStringVector(modules), mod);
}

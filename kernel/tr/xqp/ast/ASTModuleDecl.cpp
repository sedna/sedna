#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTModuleDecl.h"

ASTModuleDecl::~ASTModuleDecl()
{
    delete name;
    delete uri;
}

void ASTModuleDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTModuleDecl::dup()
{
    return new ASTModuleDecl(loc, new std::string(*name), new std::string(*uri));
}

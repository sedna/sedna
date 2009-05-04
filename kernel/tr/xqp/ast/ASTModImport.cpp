#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTModImport.h"

ASTModImport::~ASTModImport()
{
    delete name;
    delete uri;

    destroyASTStringVector(hints);
}

void ASTModImport::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTModImport::dup()
{
    ASTStringVector *vec;

    vec = duplicateASTStringVector(hints);

    return new ASTModImport(loc, (name == NULL) ? NULL : new std::string(*name), (uri == NULL) ? NULL : new std::string(*uri), vec);
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVersionDecl.h"

ASTVersionDecl::~ASTVersionDecl()
{
    delete xq_version;
    delete encoding;
}

void ASTVersionDecl::accept(ASTVisitor &v)
{
   v.visit(*this);
}

ASTNode *ASTVersionDecl::dup()
{
    return new ASTVersionDecl(loc, new std::string(*xq_version), (encoding == NULL) ? NULL : new std::string(*encoding));
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTXMLComm.h"

ASTXMLComm::~ASTXMLComm()
{
    delete cont;
}

void ASTXMLComm::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTXMLComm::dup()
{
    return new ASTXMLComm(loc, new std::string(*cont));
}

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropDoc.h"

ASTDropDoc::~ASTDropDoc()
{
    delete doc;
    delete coll;
}

void ASTDropDoc::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropDoc::dup()
{
    return new ASTDropDoc(loc, doc->dup(), (coll) ? coll->dup() : NULL);
}

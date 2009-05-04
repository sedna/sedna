#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaSchemaDoc.h"

ASTMetaSchemaDoc::~ASTMetaSchemaDoc()
{
    delete doc;
    delete coll;
}

void ASTMetaSchemaDoc::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTMetaSchemaDoc::dup()
{
    return new ASTMetaSchemaDoc(loc, doc->dup(), (coll) ? coll->dup() : NULL);
}

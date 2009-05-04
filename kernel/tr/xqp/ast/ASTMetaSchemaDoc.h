#ifndef _AST_META_SCHEMA_DOC_H_
#define _AST_META_SCHEMA_DOC_H_

#include "ASTNode.h"
#include "AST.h"

class ASTMetaSchemaDoc : public ASTNode
{
public:
    ASTNode *doc, *coll;


public:
    ASTMetaSchemaDoc(ASTLocation &loc, ASTNode *doc_, ASTNode *coll_ = NULL) : ASTNode(loc), doc(doc_), coll(coll_) {}

    ~ASTMetaSchemaDoc();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

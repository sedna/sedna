#ifndef _AST_META_SCHEMA_COLL_H_
#define _AST_META_SCHEMA_COLL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTMetaSchemaCol : public ASTNode
{
public:
    ASTNode *coll;

public:
    ASTMetaSchemaCol(ASTLocation &loc, ASTNode *coll_) : ASTNode(loc), coll(coll_) {}

    ~ASTMetaSchemaCol();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

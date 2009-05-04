#ifndef _AST_DROP_DOC_H_
#define _AST_DROP_DOC_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDropDoc : public ASTNode
{
public:
    ASTNode *doc, *coll;

public:
    ASTDropDoc(ASTLocation &loc, ASTNode *doc_, ASTNode *coll_ = NULL) : ASTNode(loc), doc(doc_), coll(coll_) {}

    ~ASTDropDoc();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

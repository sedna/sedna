#ifndef _AST_DROP_COLL_H_
#define _AST_DROP_COLL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDropColl : public ASTNode
{
public:
    ASTNode *coll;

public:
    ASTDropColl(ASTLocation &loc, ASTNode *coll_) : ASTNode(loc), coll(coll_) {}

    ~ASTDropColl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

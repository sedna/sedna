#ifndef _AST_META_DOCS_H_
#define _AST_META_DOCS_H_

#include "ASTNode.h"
#include "AST.h"

class ASTMetaDocs : public ASTNode
{
public:
    ASTNode *coll;
    bool need_stats;

public:
    ASTMetaDocs(ASTLocation &loc, ASTNode *coll_, bool stats) : ASTNode(loc), coll(coll_), need_stats(stats) {}

    ~ASTMetaDocs();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

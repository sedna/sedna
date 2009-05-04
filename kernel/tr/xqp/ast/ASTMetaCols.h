#ifndef _AST_META_COLS_H_
#define _AST_META_COLS_H_

#include "ASTNode.h"
#include "AST.h"

class ASTMetaCols : public ASTNode
{
public:
    bool need_stats;

public:
    ASTMetaCols(ASTLocation &loc, bool stats) : ASTNode(loc), need_stats(stats) {}

    ~ASTMetaCols() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

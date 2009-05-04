#ifndef _AST_DROP_FT_INDEX_H_
#define _AST_DROP_FT_INDEX_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTDropFtIndex : public ASTNode
{
public:
    ASTNode *index;

public:
    ASTDropFtIndex(ASTLocation &loc, ASTNode *index_) : ASTNode(loc), index(index_) {}

    ~ASTDropFtIndex();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

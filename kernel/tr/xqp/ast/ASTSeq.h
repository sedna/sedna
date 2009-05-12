/*
 * File:  ASTSeq.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_SEQ_H_
#define _AST_SEQ_H_

#include "ASTNode.h"
#include "AST.h"

#include <vector>

class ASTSeq : public ASTNode
{
public:
    ASTNodesVector *exprs;

public:
    ASTSeq(ASTLocation &loc, ASTNodesVector *exprs_) : ASTNode(loc), exprs(exprs_) {}

    ASTSeq(ASTLocation &loc) : ASTNode(loc)
    {
        exprs = new ASTNodesVector();
    }

    ~ASTSeq();

    void addExpr(ASTNode *expr)
    {
        exprs->push_back(expr);
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

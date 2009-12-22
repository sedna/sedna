/*
 * File:  ASTSpaceSeq.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_SPACE_SEQ_H_
#define _AST_SPACE_SEQ_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTSpaceSeq : public ASTNode
{
public:
    ASTNode *expr;

    bool atomize; // space any nodes; false - only atomics

public:
    ASTSpaceSeq(const ASTNodeCommonData &loc, ASTNode *expr_) : ASTNode(loc), expr(expr_) {}

    ~ASTSpaceSeq();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

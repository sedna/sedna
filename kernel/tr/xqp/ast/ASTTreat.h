/*
 * File:  ASTTreat.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TREAT_H_
#define _AST_TREAT_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeSeq.h"

class ASTTreat : public ASTNode
{
public:
    ASTNode *expr;
    ASTNode *type; // ASTTypeSeq

public:
    ASTTreat(const ASTNodeCommonData &loc, ASTNode *tr_expr, ASTNode *tr_type) : ASTNode(loc), expr(tr_expr), type(tr_type) {}
    ~ASTTreat();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

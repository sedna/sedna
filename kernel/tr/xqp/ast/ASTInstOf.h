/*
 * File:  ASTInstOf.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_INST_OF_H_
#define _AST_INST_OF_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeSeq.h"

class ASTInstOf : public ASTNode
{
public:
    ASTNode *expr;
    ASTNode *type; // ASTTypeSeq

public:
    ASTInstOf(const ASTNodeCommonData &loc, ASTNode *inst_expr, ASTNode *inst_type) : ASTNode(loc), expr(inst_expr), type(inst_type) {}
    ~ASTInstOf();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

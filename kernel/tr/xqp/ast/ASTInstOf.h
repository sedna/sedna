/*
 * File:  ASTInstOf.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_INST_OF_H_
#define _AST_INST_OF_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeSeq;

class ASTInstOf : public ASTNode
{
public:
    ASTNode *expr;
    ASTTypeSeq *type;

public:
    ASTInstOf(ASTLocation &loc, ASTNode *inst_expr, ASTTypeSeq *inst_type) : ASTNode(loc), expr(inst_expr), type(inst_type) {}
    ~ASTInstOf();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

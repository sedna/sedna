/*
 * File:  ASTCase.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CASE_H_
#define _AST_CASE_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeSeq.h"

class ASTCase : public ASTNode
{
public:
    ASTNode *type; // NULL if this is a default case; ASTTypeSeq
    ASTNode *var; // ASTVar; case variable or NULL
    ASTNode *expr; // expression for this case

public:
    ASTCase(ASTLocation &loc, ASTNode *var_, ASTNode *seqtype, ASTNode *expr_) : ASTNode(loc), type(seqtype), var(var_), expr(expr_) {}

    ~ASTCase();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTCase.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CASE_H_
#define _AST_CASE_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeSeq.h"
#include "ASTFunDef.h"

class ASTCase : public ASTNode
{
public:
    ASTNode *type; // NULL if this is a default case; ASTTypeSeq
    ASTNode *fd; // ASTFunDef

public:
    ASTCase(ASTLocation &loc, ASTNode *seqtype, ASTNode *expr) : ASTNode(loc), type(seqtype), fd(expr) {}

    ~ASTCase();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

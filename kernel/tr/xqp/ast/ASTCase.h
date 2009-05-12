/*
 * File:  ASTCase.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CASE_H_
#define _AST_CASE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeSeq;
class ASTFunDef;

class ASTCase : public ASTNode
{
public:
    ASTTypeSeq *type; // NULL if this is a default case
    ASTFunDef *fd;

public:
    ASTCase(ASTLocation &loc, ASTTypeSeq *seqtype, ASTFunDef *expr) : ASTNode(loc), type(seqtype), fd(expr) {}

    ~ASTCase();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

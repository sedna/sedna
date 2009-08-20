/*
 * File:  ASTPosVar.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_POS_VAR_H_
#define _AST_POS_VAR_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTVar.h"

class ASTPosVar : public ASTNode
{
public:
    ASTNode *var;

public:
    ASTPosVar(ASTLocation &loc, ASTNode *pos_var) : ASTNode(loc), var(pos_var) {}

    ~ASTPosVar();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

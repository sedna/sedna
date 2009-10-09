/*
 * File:  ASTUnio.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_UNIO_H_
#define _AST_UNIO_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUnio : public ASTNode
{
public:
    ASTNodesVector *vars;

public:
    ASTUnio(const ASTNodeCommonData &loc, ASTNodesVector *vars_) : ASTNode(loc), vars(vars_) {}

    ~ASTUnio();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTDDO.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DDO_H_
#define _AST_DDO_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDDO : public ASTNode
{
public:
    ASTNode *expr;
    bool distinct_only;

public:
    ASTDDO(const ASTNodeCommonData &loc, ASTNode *expr_, bool dist = false) : ASTNode(loc), expr(expr_), distinct_only(dist) {}

    ~ASTDDO();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

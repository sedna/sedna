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
    bool true_ddo;

public:
    ASTDDO(const ASTNodeCommonData &loc, ASTNode *expr_, bool true_ddo_ = true) : ASTNode(loc), expr(expr_), true_ddo(true_ddo_) {}

    ~ASTDDO();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

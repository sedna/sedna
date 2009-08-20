/*
 * File:  ASTTypeSwitch.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TYPESWITCH_H_
#define _AST_TYPESWITCH_H_

#include "ASTNode.h"
#include "AST.h"

#include <vector>

#include "ASTCase.h"

class ASTTypeSwitch : public ASTNode
{
public:
    ASTNode *expr; // operand expression
    ASTNodesVector *cases;
    ASTNode *def_case; // ASTCase

public:
    ASTTypeSwitch(ASTLocation &loc, ASTNode *op_expr, ASTNodesVector *cs, ASTNode *dc) : ASTNode(loc), expr(op_expr), cases(cs), def_case(dc) {}

    ~ASTTypeSwitch();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

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
#include "tr/xqp/XQCommon.h"

class ASTTypeSwitch : public ASTNode
{
public:
    ASTNode *expr; // operand expression
    ASTNodesVector *cases;
    ASTNode *def_case; // ASTCase

    // stored by lreturn analysis to send to case-statements
    sedna::xqExprInfo expr_info;

public:
    ASTTypeSwitch(const ASTNodeCommonData &loc, ASTNode *op_expr, ASTNodesVector *cs, ASTNode *dc) : ASTNode(loc), expr(op_expr), cases(cs), def_case(dc) {}

    ~ASTTypeSwitch();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    sedna::xqExprInfo getExprProperties() const
    {
        return expr_info;
    }

    void setExprProperties(const sedna::xqExprInfo ex)
    {
        expr_info = ex;
    }

    static ASTNode *createNode(scheme_list &sl);
};

#endif

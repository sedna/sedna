/*
 * File:  ASTExtExpr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_EXT_EXPR_H_
#define _AST_EXT_EXPR_H_

#include "ASTNode.h"
#include "AST.h"

class ASTExtExpr : public ASTNode
{

public:
    ASTNodesVector *pragmas;
    ASTNode *expr; // may be NULL

public:
    ASTExtExpr(ASTLocation &loc, ASTNodesVector *pragmas_, ASTNode *expr_) : ASTNode(loc), pragmas(pragmas_), expr(expr_) {}
    ~ASTExtExpr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

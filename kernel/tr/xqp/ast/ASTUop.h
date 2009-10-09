/*
 * File:  ASTUop.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_UOP_H_
#define _AST_UOP_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUop : public ASTNode
{
public:
    enum Oper
    {
        PLUS,
        MINUS
    };

    ASTNode *expr;
    Oper op;

public:
    ASTUop(const ASTNodeCommonData &loc, ASTUop::Oper oper, ASTNode *uexpr) : ASTNode(loc), expr(uexpr), op(oper) {}
    ~ASTUop();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

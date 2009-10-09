/*
 * File:  ASTOrder.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ORDER_H_
#define _AST_ORDER_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrder : public ASTNode
{
public:
    enum opt
    {
        ORDERED,
        UNORDERED
    };

    ASTOrder::opt mod; // ordered/unordered modificator

public:
    ASTOrder(const ASTNodeCommonData &loc, ASTOrder::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTOrder() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};


#endif

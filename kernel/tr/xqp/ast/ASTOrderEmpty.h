/*
 * File:  ASTOrderEmpty.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ORDER_EMPTY_H_
#define _AST_ORDER_EMPTY_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrderEmpty : public ASTNode
{
public:
    enum opt
    {
        EMPTY_LEAST,
        EMPTY_GREATEST
    };

    ASTOrderEmpty::opt mod; // ordered/unordered modificator

public:
    ASTOrderEmpty(ASTLocation &loc, ASTOrderEmpty::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTOrderEmpty() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

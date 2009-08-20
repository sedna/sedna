/*
 * File:  ASTOrderMod.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ORDER_MOD_H_
#define _AST_ORDER_MOD_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTOrderModInt.h"

class ASTOrderMod : public ASTNode
{
public:
    ASTNode *ad_mod; // may be NULL; ASTOrderModInt
    ASTNode *em_mod; // may be NULL; ASTOrderModInt
    ASTNode *col_mod; // may be NULL; ASTOrderModInt


public:
    ASTOrderMod(ASTLocation &loc, ASTNode *ad = NULL, ASTNode *em = NULL, ASTNode *cm = NULL)
        : ASTNode(loc),
          ad_mod(ad),
          em_mod(em),
          col_mod(cm) {}

    ~ASTOrderMod();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

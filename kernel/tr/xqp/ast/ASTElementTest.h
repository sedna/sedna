/*
 * File:  ASTElementTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ELEM_TEST_H_
#define _AST_ELEM_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTElementTest : public ASTNode
{
public:
    enum Mod
    {
        ANY_NIL,
        NON_NIL
    };

    ASTNode *name;
    ASTNode *type;

    Mod mod;

public:
    ASTElementTest(ASTLocation &loc, ASTNode *name_ = NULL, ASTNode *type_ = NULL, Mod mod_ = NON_NIL) : ASTNode(loc), name(name_), type(type_), mod(mod_) {}

    ~ASTElementTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

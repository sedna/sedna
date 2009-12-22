/*
 * File:  ASTTypeSingle.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TYPE_SINGLE_H_
#define _AST_TYPE_SINGLE_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTAtomicTest;

class ASTTypeSingle : public ASTNode
{
public:
    enum OccurMod
    {
        ONE = 0,
        OPT,
    };

    ASTNode *type;

    OccurMod mod; // '?' or nothing (ONE)

public:
    ASTTypeSingle(const ASTNodeCommonData &loc, ASTNode *type_, ASTTypeSingle::OccurMod omod = ONE) : ASTNode(loc), type(type_), mod(omod) {}

    ~ASTTypeSingle();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTTypeSeq.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TYPE_SEQ_H_
#define _AST_TYPE_SEQ_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTTypeSeq : public ASTNode
{
public:
    enum OccurMod
    {
        ONE = 0,
        OPT,
        ZERO_OR_MORE,
        ONE_OR_MORE,
        EMPTY,
    };

    ASTNode *type_test;

    OccurMod mod; // '?', '*', '+'

public:
    ASTTypeSeq(const ASTNodeCommonData &loc, ASTNode *type, ASTTypeSeq::OccurMod omod = ONE) : ASTNode(loc), type_test(type), mod(omod) {}

    ~ASTTypeSeq();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

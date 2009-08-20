/*
 * File:  ASTTypeSeq.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TYPE_SEQ_H_
#define _AST_TYPE_SEQ_H_

#include "ASTNode.h"
#include "AST.h"

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

        // this is for our Schema quirks such as (xs:anyType) since it is analyzed via se:sa-analyze-seq-type anyway.
        // Since i don't know what sa will do with it on the next stages, i'll stay with NONE
        // Probably, it should be get ridden all together. For example, ((xs:anyType (var "" "i"))) --> ((zero-or-more xs:anyType) (var "" "i")))
        // For now, though, some tests from Sedna Tests suite emanate errors on ((zero-or-more xs:anyType) (var "" "i")))
        NONE
    };

    ASTNode *type_test;

    OccurMod mod; // '?', '*', '+'

public:
    ASTTypeSeq(ASTLocation &loc, ASTNode *type, ASTTypeSeq::OccurMod omod = ONE) : ASTNode(loc), type_test(type), mod(omod) {}

    ~ASTTypeSeq();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

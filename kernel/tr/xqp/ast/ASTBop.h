/*
 * File:  ASTBop.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_BOP_H_
#define _AST_BOP_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTBop : public ASTNode
{
public:
    // NOTICE: lreturn::ASTBop optimization depends on the order of operations in this enum
    enum Oper
    {
        // logical ops
        OR = 0,
        AND,

        // arithmetic ops
        TO,
        PLUS,
        MINUS,
        MULT,
        DIV,
        IDIV,
        MOD,

        // value comparisons
        EQ_V,
        NE_V,
        LT_V,
        LE_V,
        GT_V,
        GE_V,

        // node comparisons
        IS,
        PREC,
        FOLLOW,

        // general comparisons
        EQ_G,
        NE_G,
        LT_G,
        LE_G,
        GT_G,
        GE_G,

        // combine ops
        UNION,
        INTERSECT,
        EXCEPT,
    };

public:
    ASTNode *lop, *rop;
    Oper op;

    bool doc_order; // for combine ops: are children in doc-order (true) or in xptr-order (false, via distinct)

public:
    ASTBop(const ASTNodeCommonData &loc, ASTBop::Oper oper, ASTNode *LExpr = NULL, ASTNode *RExpr = NULL) : ASTNode(loc), lop(LExpr), rop(RExpr), op(oper)
    {
        doc_order = false;
    }

    ~ASTBop();

    void setLExpr(ASTNode *expr)
    {
        delete lop;
        lop = expr;
    }

    void setRExpr(ASTNode *expr)
    {
        delete rop;
        rop = expr;
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

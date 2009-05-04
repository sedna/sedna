#ifndef _AST_BOP_H_
#define _AST_BOP_H_

#include "ASTNode.h"
#include "AST.h"

class ASTBop : public ASTNode
{
public:
    enum Oper
    {
        OR = 0,
        AND,
        TO,
        PLUS,
        MINUS,
        MULT,
        DIV,
        IDIV,
        MOD,
        UNION,
        INTERSECT,
        EXCEPT,

        // general comparison
        EQ_G,
        NE_G,
        LT_G,
        LE_G,
        GT_G,
        GE_G,

        // value comparison
        EQ_V,
        NE_V,
        LT_V,
        LE_V,
        GT_V,
        GE_V,

        IS,
        PREC,
        FOLLOW
    };

public:
    ASTNode *lop, *rop;
    Oper op;

public:
    ASTBop(ASTLocation &loc, ASTBop::Oper oper, ASTNode *LExpr = NULL, ASTNode *RExpr = NULL) : ASTNode(loc), lop(LExpr), rop(RExpr), op(oper) {}
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
};

#endif

/*
 * File:  ASTFor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FOR_H_
#define _AST_FOR_H_

#include "ASTNode.h"
class ASTVisitor;

#include "ASTTypeVar.h"
#include "ASTPosVar.h"

class ASTFor : public ASTNode
{
public:
    ASTNode *tv; // main for variable; ASTTypeVar
    ASTNode *pv; // positional variable; may be NULL; ASTPosVar
    ASTNode *expr; // for expression

public:
    ASTFor(const ASTNodeCommonData &loc, ASTNode *var, ASTNode *pos_var, ASTNode *for_expr) : ASTNode(loc), tv(var), pv(pos_var), expr(for_expr) {}

    ~ASTFor();

    void accept(ASTVisitor &v);

    ASTTypeVar *getVar()
    {
        return static_cast<ASTTypeVar *>(tv);
    }

    ASTPosVar *getPosVar()
    {
        return static_cast<ASTPosVar *>(pv);
    }

    bool usesPosVar() const
    {
        return (pv != NULL);
    }

    // returns list containing COPY of variables (usual and pos)
    ASTNodesVector *getVarList();

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

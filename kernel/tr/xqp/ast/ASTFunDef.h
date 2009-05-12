/*
 * File:  ASTFunDef.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FUNDEF_H_
#define _AST_FUNDEF_H_

#include "ASTNode.h"
#include "AST.h"

#include "tr/xqp/ast/ASTTypeVar.h"

class ASTFunDef : public ASTNode
{
public:
    ASTNodesVector *vars;
    ASTNode *fun;

public:
    ASTFunDef(ASTLocation &loc, ASTNodesVector *varlist, ASTNode *expr) : ASTNode(loc), vars(varlist), fun(expr) {}
    ASTFunDef(ASTLocation &loc, ASTTypeVar *var, ASTNode *expr) : ASTNode(loc), fun(expr)
    {
        vars = new ASTNodesVector();
        vars->push_back(var);
    }

    ~ASTFunDef();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

/*
 * File:  ASTVarDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_VAR_DECL_H_
#define _AST_VAR_DECL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTVarDecl : public ASTNode
{
public:
    ASTNode *var; // ASTVar
    ASTNode *type; // NULL if no type is specified; ASTTypeSeq
    ASTNode *expr; // NULL means that variable is external

public:
    ASTVarDecl(ASTLocation &loc, ASTNode *vard, ASTNode *var_type = NULL, ASTNode *var_expr = NULL) : ASTNode(loc), var(vard), type(var_type), expr(var_expr) {}

    ~ASTVarDecl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

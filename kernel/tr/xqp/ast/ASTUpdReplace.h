/*
 * File:  ASTUpdReplace.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_UPDATE_REPLACE_H_
#define _AST_UPDATE_REPLACE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUpdReplace : public ASTNode
{
public:
    ASTNode *var; // ASTTypeVar
    ASTNode *what;
    ASTNode *new_expr;

public:
    ASTUpdReplace(const ASTNodeCommonData &loc, ASTNode *var_, ASTNode *what_, ASTNode *new_expr_) : ASTNode(loc), var(var_), what(what_), new_expr(new_expr_) {}

    ~ASTUpdReplace();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

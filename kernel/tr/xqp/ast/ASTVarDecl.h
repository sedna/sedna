/*
 * File:  ASTVarDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_VAR_DECL_H_
#define _AST_VAR_DECL_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTVarDecl : public ASTNode
{
public:
    ASTNode *var; // ASTVar
    ASTNode *type; // NULL if no type is specified; ASTTypeSeq
    ASTNode *expr; // NULL means that variable is external

    // cached by lr2por
    int id; // id for this variable to use for consumers

public:
    ASTVarDecl(const ASTNodeCommonData &loc, ASTNode *vard, ASTNode *var_type = NULL, ASTNode *var_expr = NULL) : ASTNode(loc), var(vard), type(var_type), expr(var_expr)
    {
        id = -1;
    }

    ~ASTVarDecl();

    int getId() const
    {
        return id;
    }

    void setId(int id_)
    {
        id = id_;
    }

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

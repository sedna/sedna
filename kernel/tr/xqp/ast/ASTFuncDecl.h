/*
 * File:  ASTFuncDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FUNC_DECL_H_
#define _AST_FUNC_DECL_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTTypeSeq;

class ASTFuncDecl : public ASTNode
{
public:
    std::string *pref, *local; // function name
    ASTNodesVector *params; // parameters (ASTTypeVar); NULL for ()
    ASTTypeSeq *ret; // return type
    ASTNode *body; // function body; NULL means external

public:
    ASTFuncDecl(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params = NULL,
                ASTTypeSeq *ret_type = NULL, ASTNode *func_body = NULL);

    ASTFuncDecl(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params = NULL,
            ASTTypeSeq *ret_type = NULL, ASTNode *func_body = NULL);

    ~ASTFuncDecl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

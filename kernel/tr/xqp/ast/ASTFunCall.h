/*
 * File:  ASTFunCall.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FUN_CALL_H_
#define _AST_FUN_CALL_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTFunCall : public ASTNode
{
public:
    std::string *pref, *local; // function name
    ASTNodesVector *params; // parameters (ASTTypeVar); NULL for ()

public:
    ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params = NULL);

    ASTFunCall(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params = NULL);

    ~ASTFunCall();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

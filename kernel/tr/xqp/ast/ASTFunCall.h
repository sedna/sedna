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

    std::string *uri; // added by sema
    std::string *int_name; // internal name (for standard functions)

public:
    ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params = NULL);

    ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNode *func_param);

    ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNode *func_param1, ASTNode *func_param2);

    ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNode *func_param1, ASTNode *func_param2, ASTNode *func_param3);

    ASTFunCall(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params = NULL);

    ~ASTFunCall();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTFuncDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FUNC_DECL_H_
#define _AST_FUNC_DECL_H_

#include "ASTNode.h"
class ASTVisitor;
#include <string>

class ASTFuncDecl : public ASTNode
{
public:
    std::string *pref, *local; // function name
    ASTNodesVector *params; // parameters (ASTTypeVar); NULL for ()
    ASTNode *ret; // return type; ASTTypeSeq
    ASTNode *body; // function body; NULL means external

    // added by semantic analysis
    std::string *func_uri;

    // cached by lr2por
    int id;

public:
    ASTFuncDecl(const ASTNodeCommonData &loc, std::string *func_name, ASTNodesVector *func_params = NULL,
                ASTNode *ret_type = NULL, ASTNode *func_body = NULL);

    ASTFuncDecl(const ASTNodeCommonData &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params = NULL,
            ASTNode *ret_type = NULL, ASTNode *func_body = NULL);

    ~ASTFuncDecl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    int getId() const
    {
        return id;
    }

    void setId(int id_)
    {
        id = id_;
    }

    static ASTNode *createNode(scheme_list &sl);
};

#endif

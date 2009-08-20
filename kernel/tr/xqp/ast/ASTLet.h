/*
 * File:  ASTLet.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_LET_H_
#define _AST_LET_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeVar.h"
#include "ASTFunDef.h"

class ASTLet : public ASTNode
{
public:
    ASTNode *tv; // main for variable; ASTTypeVar
    ASTNode *expr; // for expression

    ASTNode *fd;  // FunDef expression for the final for-clause representation; ASTFunDef

public:
    ASTLet(ASTLocation &loc, ASTNode *var, ASTNode *let_expr) : ASTNode(loc), tv(var), expr(let_expr), fd(NULL) {}
    ASTLet(ASTLocation &loc, ASTNode *var, ASTNode *let_expr, ASTNode *fd_expr) : ASTNode(loc), tv(var), expr(let_expr), fd(fd_expr)
    {
        if (var == NULL)
            tv = new ASTVar(loc, new std::string("dummy-var"));
    }

    ~ASTLet();

    void accept(ASTVisitor &v);

    ASTTypeVar *getVar()
    {
        return static_cast<ASTTypeVar*>(tv);
    }

    // returns list containing COPY of variables (usual and pos)
    ASTNodesVector *getVarList();

    // we set funDef when we cough up the final For-clause representation
    void setFunDef(ASTNode *funDef);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

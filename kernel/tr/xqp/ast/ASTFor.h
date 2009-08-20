/*
 * File:  ASTFor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FOR_H_
#define _AST_FOR_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTTypeVar.h"
#include "ASTPosVar.h"
#include "ASTFunDef.h"

class ASTFor : public ASTNode
{
public:
    ASTNode *tv; // main for variable; ASTTypeVar
    ASTNode *pv; // positional variable; may be NULL; ASTPosVar
    ASTNode *expr; // for expression

    ASTNode *fd;  // FunDef expression for the final for-clause representation; ASTFunDef

public:
    ASTFor(ASTLocation &loc, ASTNode *var, ASTNode *pos_var, ASTNode *for_expr) : ASTNode(loc), tv(var), pv(pos_var), expr(for_expr), fd(NULL) {}

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

    // returns list containing COPY of variables (usual and pos)
    ASTNodesVector *getVarList();

    // we set funDef when we cough up the final For-clause representation
    void setFunDef(ASTNode *funDef);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

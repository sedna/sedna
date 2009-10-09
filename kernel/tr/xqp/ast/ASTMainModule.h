/*
 * File:  ASTMainModule.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_MAIN_MODULE_H_
#define _AST_MAIN_MODULE_H_

#include "ASTNode.h"
#include "AST.h"

#include "ASTProlog.h"
#include "ASTQuery.h"

class ASTMainModule : public ASTNode
{
public:
    ASTNode *prolog; // query prolog; not NULL
    ASTNode *query; // query itself (including Sedna update expressions); not NULL

public:
    ASTMainModule(const ASTNodeCommonData &loc, ASTNode *prol, ASTNode *quer) : ASTNode(loc), prolog(prol), query(quer) {}

    void setVersionDecl(ASTNode *vd);

    ~ASTMainModule();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

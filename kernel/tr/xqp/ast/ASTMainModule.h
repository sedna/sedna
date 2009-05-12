/*
 * File:  ASTMainModule.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_MAIN_MODULE_H_
#define _AST_MAIN_MODULE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTProlog;
class ASTQuery;

class ASTMainModule : public ASTNode
{
public:
    ASTProlog *prolog; // query prolog; not NULL
    ASTQuery *query; // query itself (including Sedna update expressions); not NULL

public:
    ASTMainModule(ASTLocation &loc, ASTProlog *prol, ASTQuery *quer) : ASTNode(loc), prolog(prol), query(quer) {}

    void setVersionDecl(ASTNode *vd);

    ~ASTMainModule();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

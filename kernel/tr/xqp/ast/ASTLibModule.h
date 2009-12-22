/*
 * File:  ASTLibModule.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_LIB_MODULE_H_
#define _AST_LIB_MODULE_H_

#include "ASTNode.h"
class ASTVisitor;

#include "ASTModuleDecl.h"
#include "ASTProlog.h"

class ASTLibModule : public ASTNode
{
public:
    ASTNode *moduleDecl; // module declaration; ASTModuleDecl
    ASTNode *prolog; // query prolog; not NULL; ASTProlog

    bool is_internal; // true, if this module was deserialized from internals (reset form createNode)

public:
    ASTLibModule(const ASTNodeCommonData &loc, ASTNode *md, ASTNode *prol) : ASTNode(loc), moduleDecl(md), prolog(prol)
    {
        is_internal = false;
    }

    void setVersionDecl(ASTNode *vd);

    ~ASTLibModule();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

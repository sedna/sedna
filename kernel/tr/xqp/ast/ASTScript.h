/*
 * File:  ASTScript.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_SCRIPT_H_
#define _AST_SCRIPT_H_

#include "ASTNode.h"
#include "AST.h"

class ASTScript : public ASTNode
{
public:
    ASTNodesVector *modules;

public:
    ASTScript(ASTLocation &loc) : ASTNode(loc)
    {
        modules = new ASTNodesVector();
    }

    ASTScript(ASTLocation &loc, ASTNodesVector *mods) : ASTNode(loc), modules(mods) {}

    ~ASTScript();

    void addModule(ASTNode *mod);
    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

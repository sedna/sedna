/*
 * File:  ASTLoadModule.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_LOAD_MODULE_H_
#define _AST_LOAD_MODULE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTLoadModule : public ASTNode
{
public:
    enum LoadMod
    {
        LOAD,
        REPLACE // load or replace actually
    };

    ASTStringVector *modules;
    LoadMod mod;

public:
    ASTLoadModule(ASTLocation &loc, ASTStringVector *mods, LoadMod mod_) : ASTNode(loc), modules(mods), mod(mod_) {}

    ~ASTLoadModule();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

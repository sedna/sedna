/*
 * File:  ASTDropMod.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DROP_MODULE_H_
#define _AST_DROP_MODULE_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTDropMod : public ASTNode
{
public:
    std::string *module;

public:
    ASTDropMod(ASTLocation &loc, std::string *module_) : ASTNode(loc), module(module_) {}

    ~ASTDropMod();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

/*
 * File:  ASTModuleDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_MODULE_DECL_H_
#define _AST_MODULE_DECL_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTModuleDecl : public ASTNode
{
public:
    std::string *name, *uri;

public:
    ASTModuleDecl(ASTLocation &loc, std::string *mod_name, std::string *mod_uri) : ASTNode(loc), name(mod_name), uri(mod_uri) {}

    ~ASTModuleDecl();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

/*
 * File:  ASTCreateTrg.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_TRIGGER_H_
#define _AST_CREATE_TRIGGER_H_

#include "ASTNode.h"
#include "AST.h"

class ASTCreateTrg : public ASTNode
{
public:
    enum TrgMod
    {
        BEFORE = 0,
        AFTER,

        INSERT,
        DEL,
        REPLACE,

        NODE,
        STATEMENT
    };

    ASTNode *path;
    ASTNodesVector *do_exprs;
    std::string *name;
    TrgMod t_mod, a_mod, g_mod;

public:
    ASTCreateTrg(ASTLocation &loc, std::string *name_, TrgMod ba, TrgMod idr, ASTNode *path_, TrgMod ns, ASTNodesVector *expr) :
        ASTNode(loc),
        path(path_),
        do_exprs(expr),
        name(name_),
        t_mod(ba),
        a_mod(idr),
        g_mod(ns)
    {}

    ~ASTCreateTrg();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

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

    // added by sema
    std::string *leaf_name; // leaf test in path   |
    int leaf_type; // 0 - element, 1 - otherwise   | all of this only for before-insert-node triggers
    ASTNode *trimmed_path; // path without leaf    |

public:
    ASTCreateTrg(const ASTNodeCommonData &loc, std::string *name_, TrgMod ba, TrgMod idr, ASTNode *path_, TrgMod ns, ASTNodesVector *expr) :
        ASTNode(loc),
        path(path_),
        do_exprs(expr),
        name(name_),
        t_mod(ba),
        a_mod(idr),
        g_mod(ns)
    {
        leaf_name = NULL;
        leaf_type = -1;
        trimmed_path = NULL;
    }

    ~ASTCreateTrg();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

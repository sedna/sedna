/*
 * File:  ASTUpdRename.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_UPDATE_RENAME_H_
#define _AST_UPDATE_RENAME_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUpdRename : public ASTNode
{
public:
    ASTNode *what;
    std::string *pref, *local;

public:
    ASTUpdRename(ASTLocation &loc, ASTNode *what_, std::string *name) : ASTNode(loc), what(what_)
    {
        ASTParseQName(name, &pref, &local);
    }

    ASTUpdRename(ASTLocation &loc, ASTNode *what_, std::string *pref_, std::string *loc_) : ASTNode(loc), what(what_), pref(pref_), local(loc_) {}

    ~ASTUpdRename();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

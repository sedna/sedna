/*
 * File:  ASTCreateIndex.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_INDEX_H_
#define _AST_CREATE_INDEX_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTTypeSingle;

class ASTCreateIndex : public ASTNode
{
public:
    ASTNode *name, *on_path, *by_path;
    ASTTypeSingle *type;

public:
    ASTCreateIndex(ASTLocation &loc, ASTNode *name_, ASTNode *on_path_, ASTNode *by_path_, ASTTypeSingle *type_) :
        ASTNode(loc),
        name(name_),
        on_path(on_path_),
        by_path(by_path_),
        type(type_)
    {}

    ~ASTCreateIndex();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

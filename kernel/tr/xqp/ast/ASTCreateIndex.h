/*
 * File:  ASTCreateIndex.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_INDEX_H_
#define _AST_CREATE_INDEX_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

#include "ASTTypeSingle.h"

class ASTCreateIndex : public ASTNode
{
public:
    ASTNode *name, *on_path, *by_path;
    ASTNode *type; // ASTTypeSingle
    std::string *tree_type; // type of index tree

public:
    ASTCreateIndex(const ASTNodeCommonData &loc, ASTNode *name_, ASTNode *on_path_, ASTNode *by_path_, ASTNode *type_, std::string *tree_type_) :
        ASTNode(loc),
        name(name_),
        on_path(on_path_),
        by_path(by_path_),
        type(type_),
        tree_type(tree_type_)
    {}

    ~ASTCreateIndex();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTCreateFtIndex.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_FT_INDEX_H_
#define _AST_CREATE_FT_INDEX_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTCreateFtIndex : public ASTNode
{
public:
    ASTNode *name, *path, *cust_expr;
    std::string *type;

public:
    ASTCreateFtIndex(ASTLocation &loc, ASTNode *name_, ASTNode *path_, std::string *type_, ASTNode *cust_expr_ = NULL) :
        ASTNode(loc),
        name(name_),
        path(path_),
        cust_expr(cust_expr_),
        type(type_)
    {}

    ~ASTCreateFtIndex();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

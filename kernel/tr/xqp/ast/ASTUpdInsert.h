/*
 * File:  ASTUpdInsert.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_UPDATE_INSERT_H_
#define _AST_UPDATE_INSERT_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUpdInsert : public ASTNode
{
public:
    enum UpdType
    {
        PRECEDING,
        INTO,
        FOLLOWING
    };

    ASTNode *what, *where;
    UpdType type;

public:
    ASTUpdInsert(ASTLocation &loc, ASTNode *what_, ASTNode *where_, UpdType type_) : ASTNode(loc), what(what_), where(where_), type(type_) {}

    ~ASTUpdInsert();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

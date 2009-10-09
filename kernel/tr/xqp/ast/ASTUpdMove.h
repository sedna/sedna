/*
 * File:  ASTUpdMove.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_UPDATE_MOVE_H_
#define _AST_UPDATE_MOVE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUpdMove : public ASTNode
{
public:
    enum UpdType
    {
        PRECEDING,
        INTO,
        FOLLOWING
    };

    ASTNode *var; // ASTTypeVar
    ASTNode *what;
    ASTNode *where;
    UpdType type;

public:
    ASTUpdMove(ASTLocation &loc, ASTNode *var_, ASTNode *what_, ASTNode *where_, UpdType type_) : ASTNode(loc), var(var_), what(what_), where(where_), type(type_) {}

    ~ASTUpdMove();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

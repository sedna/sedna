/*
 * File:  ASTPIConst.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_PI_CONST_H_
#define _AST_PI_CONST_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTPIConst : public ASTNode
{
public:
    ASTNode *name; // computed name, or
    std::string *ncname; // direct NCName

    ASTNode *expr; // computed construction expression

public:
    ASTPIConst(ASTLocation &loc, ASTNode *name_, ASTNode *expr_ = NULL) : ASTNode(loc), name(name_), ncname(NULL), expr(expr_) {}
    ASTPIConst(ASTLocation &loc, std::string *ncname_, ASTNode *expr_ = NULL) : ASTNode(loc), name(NULL), ncname(ncname_), expr(expr_) {}

    ~ASTPIConst();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

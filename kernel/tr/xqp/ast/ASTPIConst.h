/*
 * File:  ASTPIConst.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_PI_CONST_H_
#define _AST_PI_CONST_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTPIConst : public ASTNode
{
public:
    ASTNode *name; // computed name, or
    std::string *ncname; // direct NCName
    ASTNode *expr; // computed construction expression

    bool deep_copy; // pi will be attached to virtual_root and copied on demand

public:
    ASTPIConst(const ASTNodeCommonData &loc, ASTNode *name_, ASTNode *expr_ = NULL) : ASTNode(loc), name(name_), ncname(NULL), expr(expr_) {}
    ASTPIConst(const ASTNodeCommonData &loc, std::string *ncname_, ASTNode *expr_ = NULL) : ASTNode(loc), name(NULL), ncname(ncname_), expr(expr_) {}

    ~ASTPIConst();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

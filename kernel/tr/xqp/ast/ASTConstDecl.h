/*
 * File:  ASTConstDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CONST_DECL_H_
#define _AST_CONST_DECL_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTConstDecl : public ASTNode
{
public:
    enum opt
    {
        STRIP,
        PRESERVE
    };

    ASTConstDecl::opt mod; // strip/preserve modificator

public:
    ASTConstDecl(const ASTNodeCommonData &loc, ASTConstDecl::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTConstDecl() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

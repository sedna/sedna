/*
 * File:  ASTError.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ERROR_NODE_H_
#define _AST_ERROR_NODE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTError : public ASTNode
{
public:
    ASTError(ASTLocation &loc) : ASTNode(loc) {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTNodeTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_NODE_TEST_H_
#define _AST_NODE_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTNodeTest : public ASTNode
{
public:
    ASTNodeTest(const ASTNodeCommonData &loc) : ASTNode(loc) {}

    ~ASTNodeTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

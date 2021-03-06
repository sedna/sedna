/*
 * File:  ASTCommTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_COMM_TEST_H_
#define _AST_COMM_TEST_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTCommTest : public ASTNode
{
public:
    ASTCommTest(const ASTNodeCommonData &loc) : ASTNode(loc) {}

    ~ASTCommTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTTextTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TEXT_TEST_H_
#define _AST_TEXT_TEST_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTTextTest : public ASTNode
{
public:
    ASTTextTest(const ASTNodeCommonData &loc) : ASTNode(loc) {}

    ~ASTTextTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

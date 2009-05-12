/*
 * File:  ASTTextTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TEXT_TEST_H_
#define _AST_TEXT_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTextTest : public ASTNode
{
public:
    ASTTextTest(ASTLocation &loc) : ASTNode(loc) {}

    ~ASTTextTest() {}

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

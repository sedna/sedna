/*
 * File:  ASTPiTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_PI_TEST_H_
#define _AST_PI_TEST_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTPiTest : public ASTNode
{
public:
    enum TypeTest
    {
        NCNAME,
        STRING,
        NONE
    };

    std::string *test; // will be NULL if type==NONE
    TypeTest type;

public:
    ASTPiTest(const ASTNodeCommonData &loc, std::string *test_, TypeTest type_) : ASTNode(loc), test(test_), type(type_) {}
    ASTPiTest(const ASTNodeCommonData &loc) : ASTNode(loc), test(NULL), type(NONE) {}

    ~ASTPiTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

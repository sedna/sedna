/*
 * File:  ASTSchemaAttrTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_SCHEMA_ATTR_TEST_H_
#define _AST_SCHEMA_ATTR_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTSchemaAttrTest : public ASTNode
{
public:
    ASTNode *name; // ASTNameTest

public:
    ASTSchemaAttrTest(const ASTNodeCommonData &loc, ASTNode *name_) : ASTNode(loc), name(name_)
    {
    }

    ~ASTSchemaAttrTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

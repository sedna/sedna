/*
 * File:  ASTAttribTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ATTRIB_TEST_H_
#define _AST_ATTRIB_TEST_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTAttribTest : public ASTNode
{
public:
    ASTNode *name; // ASTNameTest;  may be NULL; attribute()
    ASTNode *type; // may be NULL; attribute(abubu)

public:
    ASTAttribTest(const ASTNodeCommonData &loc, ASTNode *name_ = NULL, ASTNode *type_ = NULL) : ASTNode(loc), name(name_), type(type_) {}

    ~ASTAttribTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

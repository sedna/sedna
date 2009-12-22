/*
 * File:  ASTNameTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_NAME_TEST_H_
#define _AST_NAME_TEST_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTNameTest : public ASTNode
{
public:
    std::string *pref, *local;

    std::string *uri; // added by sema

public:
    ASTNameTest(const ASTNodeCommonData &loc, std::string *name) : ASTNode(loc)
    {
        ASTParseQName(name, &pref, &local);
        uri = NULL;

        delete name;
    }

    ASTNameTest(const ASTNodeCommonData &loc, std::string *pref_, std::string *local_) : ASTNode(loc), pref(pref_), local(local_)
    {
        uri = NULL;
    }

    ~ASTNameTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

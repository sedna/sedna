/*
 * File:  ASTSchemaElemTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_SCHEMA_ELEM_TEST_H_
#define _AST_SCHEMA_ELEM_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTSchemaElemTest : public ASTNode
{
public:
    std::string *npref, *nloc;

public:
    ASTSchemaElemTest(ASTLocation &loc, std::string *name_ = NULL) : ASTNode(loc)
    {
        ASTParseQName(name_, &npref, &nloc);
    }
    ASTSchemaElemTest(ASTLocation &loc, std::string *npref_, std::string *nloc_) :
            ASTNode(loc),
            npref(npref_),
            nloc(nloc_)
    {}

    ~ASTSchemaElemTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

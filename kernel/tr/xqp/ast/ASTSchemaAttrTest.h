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
    std::string *npref, *nloc;

public:
    ASTSchemaAttrTest(ASTLocation &loc, std::string *name_ = NULL) : ASTNode(loc)
    {
        ASTParseQName(name_, &npref, &nloc);
    }
    ASTSchemaAttrTest(ASTLocation &loc, std::string *npref_, std::string *nloc_) :
            ASTNode(loc),
            npref(npref_),
            nloc(nloc_)
    {}

    ~ASTSchemaAttrTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

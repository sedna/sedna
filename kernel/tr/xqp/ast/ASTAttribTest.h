/*
 * File:  ASTAttribTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ATTRIB_TEST_H_
#define _AST_ATTRIB_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTAttribTest : public ASTNode
{
public:
    std::string *npref, *nloc;
    std::string *tpref, *tloc;

public:
    ASTAttribTest(ASTLocation &loc, std::string *name_ = NULL, std::string *type_ = NULL) : ASTNode(loc)
    {
        ASTParseQName(name_, &npref, &nloc);
        ASTParseQName(type_, &tpref, &tloc);

        delete name_;
        delete type_;
    }
    ASTAttribTest(ASTLocation &loc, std::string *npref_, std::string *nloc_, std::string *tpref_, std::string *tloc_) :
            ASTNode(loc),
            npref(npref_),
            nloc(nloc_),
            tpref(tpref_),
            tloc(tloc_)
    {}

    ~ASTAttribTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

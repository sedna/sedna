/*
 * File:  ASTElementTest.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ELEM_TEST_H_
#define _AST_ELEM_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTElementTest : public ASTNode
{
public:
    enum Mod
    {
        ANY_NIL,
        NON_NIL
    };

    std::string *npref, *nloc;
    std::string *tpref, *tloc;

    Mod mod;

public:
    ASTElementTest(ASTLocation &loc, std::string *name_ = NULL, std::string *type_ = NULL, Mod mod_ = NON_NIL) : ASTNode(loc), mod(mod_)
    {
        ASTParseQName(name_, &npref, &nloc);
        ASTParseQName(type_, &tpref, &tloc);

        delete name_;
        delete type_;
    }
    ASTElementTest(ASTLocation &loc, std::string *npref_, std::string *nloc_, std::string *tpref_, std::string *tloc_, Mod mod_ = NON_NIL) :
            ASTNode(loc),
            npref(npref_),
            nloc(nloc_),
            tpref(tpref_),
            tloc(tloc_),
            mod(mod_)
    {}

    ~ASTElementTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

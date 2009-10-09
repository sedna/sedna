/*
 * File:  ASTQName.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_QNAME_H_
#define _AST_QNAME_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTQName : public ASTNode
{
public:
    std::string *uri, *pref, *local;

public:
    ASTQName(const ASTNodeCommonData &loc, std::string *uri_, std::string *pref_, std::string *loc_) : ASTNode(loc), uri(uri_), pref(pref_), local(loc_) {}

    ~ASTQName();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTBaseURI.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_BASE_URI_H_
#define _AST_BASE_URI_H_

#include "ASTNode.h"
#include "AST.h"

class ASTBaseURI : public ASTNode
{
public:
    std::string *uri;

public:
    ASTBaseURI(const ASTNodeCommonData &loc, std::string *base_uri) : ASTNode(loc), uri(base_uri) {}

    ~ASTBaseURI();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

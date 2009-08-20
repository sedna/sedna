/*
 * File:  ASTNamespaceDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_NAMESPACE_DECL_H_
#define _AST_NAMESPACE_DECL_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTNamespaceDecl : public ASTNode
{
public:
    std::string *name, *uri;

public:
    ASTNamespaceDecl(ASTLocation &loc, std::string *nsp_name, std::string *nsp_uri) : ASTNode(loc), name(nsp_name), uri(nsp_uri) {}

    ~ASTNamespaceDecl();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

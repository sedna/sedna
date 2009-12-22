/*
 * File:  ASTDefNamespaceDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DEF_NAMESPACE_DECL_H_
#define _AST_DEF_NAMESPACE_DECL_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTDefNamespaceDecl : public ASTNode
{
public:
    enum Type
    {
        ELEMENT,
        FUNCTION
    };

    std::string *uri;
    Type type;

public:
    ASTDefNamespaceDecl(const ASTNodeCommonData &loc, std::string *nsp_uri, ASTDefNamespaceDecl::Type t) : ASTNode(loc), uri(nsp_uri), type(t) {}

    ~ASTDefNamespaceDecl();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

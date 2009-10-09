/*
 * File:  ASTNamespaceDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNamespaceDecl.h"

ASTNamespaceDecl::~ASTNamespaceDecl()
{
    delete name;
    delete uri;
}

void ASTNamespaceDecl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTNamespaceDecl::dup()
{
    return new ASTNamespaceDecl(cd, new std::string(*name), new std::string(*uri));
}

ASTNode *ASTNamespaceDecl::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *name = NULL, *uri = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    name = new std::string(sl[2].internal.str);
    uri = new std::string(sl[3].internal.str);

    return new ASTNamespaceDecl(cd, name, uri);
}

void ASTNamespaceDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

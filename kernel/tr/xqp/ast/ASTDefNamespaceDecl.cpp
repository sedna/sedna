/*
 * File:  ASTDefNamespaceDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDefNamespaceDecl.h"

ASTDefNamespaceDecl::~ASTDefNamespaceDecl()
{
    delete uri;
}

void ASTDefNamespaceDecl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDefNamespaceDecl::dup()
{
    return new ASTDefNamespaceDecl(cd, new std::string(*uri), type);
}

ASTNode *ASTDefNamespaceDecl::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *uri = NULL;
    Type type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    uri = new std::string(sl[2].internal.str);
    type = Type(atol(sl[3].internal.num));

    return new ASTDefNamespaceDecl(cd, uri, type);
}

void ASTDefNamespaceDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

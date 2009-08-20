/*
 * File:  ASTModuleDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTModuleDecl.h"

ASTModuleDecl::~ASTModuleDecl()
{
    delete name;
    delete uri;
}

void ASTModuleDecl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTModuleDecl::dup()
{
    return new ASTModuleDecl(loc, new std::string(*name), new std::string(*uri));
}

ASTNode *ASTModuleDecl::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *name = NULL, *uri = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    name = new std::string(sl[2].internal.str);
    uri = new std::string(sl[3].internal.str);

    return new ASTModuleDecl(loc, name, uri);
}

void ASTModuleDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

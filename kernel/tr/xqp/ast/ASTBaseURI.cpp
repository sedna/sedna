/*
 * File:  ASTBaseURI.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBaseURI.h"

ASTBaseURI::~ASTBaseURI()
{
    delete uri;
}

void ASTBaseURI::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTBaseURI::dup()
{
    return new ASTBaseURI(cd, new std::string(*uri));
}

ASTNode *ASTBaseURI::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *uri;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    uri = new std::string(sl[2].internal.str);

    return new ASTBaseURI(cd, uri);
}

void ASTBaseURI::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

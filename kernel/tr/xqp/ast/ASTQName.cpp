/*
 * File:  ASTQName.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQName.h"

ASTQName::~ASTQName()
{
    delete uri;
    delete pref;
    delete local;
}

void ASTQName::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTQName::dup()
{
    return new ASTQName(loc, new std::string(*uri), new std::string(*pref), new std::string(*local));
}

ASTNode *ASTQName::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *uri = NULL, *pref = NULL, *local = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    uri = new std::string(sl[2].internal.str);
    pref = new std::string(sl[3].internal.str);
    local = new std::string(sl[4].internal.str);

    return new ASTQName(loc, uri, pref, local);
}

void ASTQName::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

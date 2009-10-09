/*
 * File:  ASTVar.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVar.h"

ASTVar::~ASTVar()
{
    delete pref;
    delete local;
    delete uri;
}

void ASTVar::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTVar::dup()
{
    ASTVar *res;

    res = new ASTVar(cd, new std::string(*pref), new std::string(*local));

    if (uri)
        res->uri = new std::string(uri->c_str());

    return res;
}

ASTNode *ASTVar::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *pref = NULL, *local = NULL;
    ASTVar *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);
    res = new ASTVar(cd, pref, local);

    if (sl.size() > 4)
    {
        U_ASSERT(sl[4].type == SCM_STRING);
        res->uri = new std::string(sl[4].internal.str);
    }

    return res;
}

void ASTVar::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

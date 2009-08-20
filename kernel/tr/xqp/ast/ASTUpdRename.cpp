/*
 * File:  ASTUpdRename.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdRename.h"

ASTUpdRename::~ASTUpdRename()
{
    delete what;
    delete pref;
    delete local;
}

void ASTUpdRename::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTUpdRename::dup()
{
    return new ASTUpdRename(loc, what->dup(), new std::string(*pref), new std::string(*local));
}

ASTNode *ASTUpdRename::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *what = NULL;
    std::string *pref = NULL, *local = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_STRING && sl[4].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    what = dsGetASTFromSchemeList(*sl[2].internal.list);
    pref = new std::string(sl[3].internal.str);
    local = new std::string(sl[4].internal.str);

    return new ASTUpdRename(loc, what, pref,local);
}

void ASTUpdRename::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (what == oldc)
    {
        what = newc;
        return;
    }
}

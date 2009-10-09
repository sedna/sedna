/*
 * File:  ASTCreateDoc.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateDoc.h"

ASTCreateDoc::~ASTCreateDoc()
{
    delete doc;
    delete coll;
}

void ASTCreateDoc::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCreateDoc::dup()
{
    return new ASTCreateDoc(cd, doc->dup(), (coll) ? coll->dup() : NULL);
}

ASTNode *ASTCreateDoc::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *coll = NULL, *doc = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    doc = dsGetASTFromSchemeList(*sl[2].internal.list);
    coll = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTCreateDoc(cd, doc, coll);
}

void ASTCreateDoc::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (doc == oldc)
    {
        doc = newc;
        return;
    }
    if (coll == oldc)
    {
        coll = newc;
        return;
    }
}

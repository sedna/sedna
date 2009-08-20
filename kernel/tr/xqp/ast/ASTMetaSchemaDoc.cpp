/*
 * File:  ASTMetaSchemaDoc.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaSchemaDoc.h"

ASTMetaSchemaDoc::~ASTMetaSchemaDoc()
{
    delete doc;
    delete coll;
}

void ASTMetaSchemaDoc::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTMetaSchemaDoc::dup()
{
    return new ASTMetaSchemaDoc(loc, doc->dup(), (coll) ? coll->dup() : NULL);
}

ASTNode *ASTMetaSchemaDoc::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *coll = NULL, *doc = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    doc = dsGetASTFromSchemeList(*sl[2].internal.list);
    coll = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTMetaSchemaDoc(loc, doc, coll);
}

void ASTMetaSchemaDoc::modifyChild(const ASTNode *oldc, ASTNode *newc)
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

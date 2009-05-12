/*
 * File:  ASTDropDoc.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropDoc.h"

ASTDropDoc::~ASTDropDoc()
{
    delete doc;
    delete coll;
}

void ASTDropDoc::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropDoc::dup()
{
    return new ASTDropDoc(loc, doc->dup(), (coll) ? coll->dup() : NULL);
}

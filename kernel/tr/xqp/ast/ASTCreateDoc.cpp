/*
 * File:  ASTCreateDoc.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateDoc.h"

ASTCreateDoc::~ASTCreateDoc()
{
    delete doc;
    delete coll;
}

void ASTCreateDoc::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateDoc::dup()
{
    return new ASTCreateDoc(loc, doc->dup(), (coll) ? coll->dup() : NULL);
}

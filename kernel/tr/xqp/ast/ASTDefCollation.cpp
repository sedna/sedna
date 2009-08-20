/*
 * File:  ASTDefCollation.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDefCollation.h"

ASTDefCollation::~ASTDefCollation()
{
    delete uri;
}

void ASTDefCollation::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDefCollation::dup()
{
    return new ASTDefCollation(loc, new std::string(*uri));
}

ASTNode *ASTDefCollation::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *uri = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    uri = new std::string(sl[2].internal.str);

    return new ASTDefCollation(loc, uri);
}

void ASTDefCollation::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

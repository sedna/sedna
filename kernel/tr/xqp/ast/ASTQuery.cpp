/*
 * File:  ASTQuery.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuery.h"

ASTQuery::~ASTQuery()
{
    delete query;
}

void ASTQuery::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTQuery::dup()
{
    return new ASTQuery(cd, query->dup(), type);
}

ASTNode *ASTQuery::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *expr = NULL;
    QueryType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = QueryType(atol(sl[3].internal.num));

    return new ASTQuery(cd, expr, type);
}

void ASTQuery::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (query == oldc)
    {
        query = newc;
        return;
    }
}

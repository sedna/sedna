/*
 * File:  ASTAxis.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAxis.h"

ASTAxis::~ASTAxis()
{
    delete expr;
    delete test;
}

void ASTAxis::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTAxis::dup()
{
    return new ASTAxis(loc, axis, expr->dup(), test->dup());
}

ASTNode *ASTAxis::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *test = NULL, *expr = NULL;
    ASTAxisStep::AxisType axis;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    axis = ASTAxisStep::AxisType(atoi(sl[2].internal.num));

    expr = dsGetASTFromSchemeList(*sl[3].internal.list);
    test = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTAxis(loc, axis, expr, test);
}

void ASTAxis::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (test == oldc)
    {
        test = newc;
        return;
    }
}

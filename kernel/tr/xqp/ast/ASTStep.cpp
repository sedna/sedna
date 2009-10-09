/*
 * File:  ASTStep.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTStep.h"

ASTStep::~ASTStep()
{
    delete cont;
    destroyASTNodesVector(preds);
}

void ASTStep::accept(ASTVisitor &v)
{
}

ASTNode *ASTStep::dup()
{
    ASTStep *res;

    res = new ASTStep(loc, duplicateASTNodes(preds));

    if (cont)
        res->setContext(cont->dup());

    return res;
}

void ASTStep::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (cont == oldc)
    {
        cont = newc;
        return;
    }
    if (preds)
    {
        for (unsigned int i = 0; i < preds->size(); i++)
        {
            if ((*preds)[i] == oldc)
            {
                (*preds)[i] = newc;
                return;
            }
        }
    }
}

ASTAxisStep::~ASTAxisStep()
{
    delete test;
}

void ASTAxisStep::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTAxisStep::dup()
{
    ASTAxisStep *res;

    res = new ASTAxisStep(loc, axis, test->dup(), duplicateASTNodes(preds));

    if (cont)
        res->setContext(cont->dup());

    if (isLast)
        res->setAsLast();

    return res;
}

ASTNode *ASTAxisStep::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *test = NULL, *cont = NULL;
    ASTNodesVector *preds = NULL;
    AxisType axis;
    ASTAxisStep *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER && sl[4].type == SCM_LIST && sl[5].type == SCM_LIST && sl[6].type == SCM_BOOL);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    cont = dsGetASTFromSchemeList(*sl[2].internal.list);
    axis = AxisType(atoi(sl[3].internal.num));
    test = dsGetASTFromSchemeList(*sl[4].internal.list);
    preds = dsGetASTNodesFromSList(*sl[5].internal.list);

    res = new ASTAxisStep(loc, axis, test, preds);

    res->setContext(cont);

    if (sl[6].internal.b)
        res->setAsLast();

    return res;
}

void ASTAxisStep::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    ASTStep::modifyChild(oldc, newc);

    if (test == oldc)
    {
        test = newc;
        return;
    }
}

ASTFilterStep::~ASTFilterStep()
{
    delete expr;
}

void ASTFilterStep::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTFilterStep::dup()
{
    ASTFilterStep *res;

    res = new ASTFilterStep(loc, (expr) ? expr->dup() : NULL, duplicateASTNodes(preds));

    if (cont)
        res->setContext(cont->dup());

    if (isLast)
        res->setAsLast();

    return res;
}

ASTNode *ASTFilterStep::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL, *cont = NULL;
    ASTNodesVector *preds = NULL;
    ASTFilterStep *res;
    bool isLast;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_BOOL );

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    cont = dsGetASTFromSchemeList(*sl[2].internal.list);
    expr = dsGetASTFromSchemeList(*sl[3].internal.list);
    preds = dsGetASTNodesFromSList(*sl[4].internal.list);
    isLast = sl[5].internal.b;

    res = new ASTFilterStep(loc, expr, preds);

    if (isLast)
        res->setAsLast();

    res->setContext(cont);

    return res;
}

void ASTFilterStep::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    ASTStep::modifyChild(oldc, newc);

    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}

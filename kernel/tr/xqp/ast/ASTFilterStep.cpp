/*
 * File:  ASTFilterStep.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFilterStep.h"

ASTFilterStep::~ASTFilterStep()
{
    delete expr;
    destroyASTNodesVector(preds);
}

void ASTFilterStep::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTFilterStep::dup()
{
    return new ASTFilterStep(loc, expr->dup(), duplicateASTNodes(preds));
}

ASTNode *ASTFilterStep::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;
    ASTNodesVector *preds = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    preds = dsGetASTNodesFromSList(*sl[3].internal.list);

    return new ASTFilterStep(loc, expr, preds);
}

void ASTFilterStep::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
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

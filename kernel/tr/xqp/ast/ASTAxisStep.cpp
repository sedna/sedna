/*
 * File:  ASTAxisStep.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAxisStep.h"

ASTAxisStep::~ASTAxisStep()
{
    delete test;
    destroyASTNodesVector(preds);
}

void ASTAxisStep::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTAxisStep::dup()
{
    return new ASTAxisStep(loc, axis, test->dup(), duplicateASTNodes(preds));
}

ASTNode *ASTAxisStep::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *test = NULL;
    ASTNodesVector *preds = NULL;
    AxisType axis;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    axis = AxisType(atoi(sl[2].internal.num));
    test = dsGetASTFromSchemeList(*sl[3].internal.list);
    preds = dsGetASTNodesFromSList(*sl[4].internal.list);

    return new ASTAxisStep(loc, axis, test, preds);
}

void ASTAxisStep::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (test == oldc)
    {
        test = newc;
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

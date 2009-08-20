/*
 * File:  ASTSeq.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSeq.h"

ASTSeq::~ASTSeq()
{
    destroyASTNodesVector(exprs);
}

void ASTSeq::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTSeq::dup()
{
    return new ASTSeq(loc, duplicateASTNodes(exprs));
}

ASTNode *ASTSeq::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNodesVector *exprs = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    exprs = dsGetASTNodesFromSList(*sl[2].internal.list);

    if (exprs == NULL)
        exprs = new ASTNodesVector();

    return new ASTSeq(loc, exprs);
}

void ASTSeq::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (exprs)
    {
        for (unsigned int i = 0; i < exprs->size(); i++)
        {
            if ((*exprs)[i] == oldc)
            {
                (*exprs)[i] = newc;
                return;
            }
        }
    }
}

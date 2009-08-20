/*
 * File:  ASTVisitor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "ASTVisitor.h"

void ASTVisitor::addToPath(ASTNode *nod)
{
    vis_path.push_back(nod);
}

void ASTVisitor::removeFromPath(ASTNode *nod)
{
    U_ASSERT(vis_path.back() == nod);
    vis_path.pop_back();
}

void ASTVisitor::VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v)
{
    ASTNodesVector::iterator it;
    ASTNode *node;

    if (nodes == NULL) return;

    for (unsigned int i = 0; i < nodes->size(); i++)
    {
        node = (*nodes)[i];
        node->accept(v);
    }
}

void ASTVisitor::modifyParent(ASTNode *newc, bool toAccept, bool toGarbage)
{
    ASTNode *oldc;

    if (vis_path.size() < 2)
        return;

    oldc = vis_path.back();

    (vis_path[vis_path.size() - 2])->modifyChild(oldc, newc);

    if (toGarbage)
        putToGarbage(oldc);

    if (toAccept)
    {
        vis_path.pop_back();
        newc->accept(*this);
        vis_path.push_back(oldc);
    }
}

void ASTVisitor::putToGarbage(ASTNode *nod)
{
    garbNodes.push_back(nod);
}

void ASTVisitor::freeGarbage()
{
    ASTNodesVector::iterator it;

    for (it = garbNodes.begin(); it != garbNodes.end(); it++)
        delete *it;
}

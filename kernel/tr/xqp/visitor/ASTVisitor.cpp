/*
 * File:  ASTVisitor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "ASTVisitor.h"

void ASTVisitor::VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v)
{
    ASTNodesVector::iterator it;

    if (nodes == NULL) return;

    for (it = nodes->begin(); it != nodes->end(); it++)
        (*it)->accept(v);
}

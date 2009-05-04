#include "ASTVisitor.h"

void ASTVisitor::VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v)
{
    ASTNodesVector::iterator it;

    if (nodes == NULL) return;

    for (it = nodes->begin(); it != nodes->end(); it++)
        (*it)->accept(v);
}

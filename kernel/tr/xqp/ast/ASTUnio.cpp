/*
 * File:  ASTUnio.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUnio.h"

ASTUnio::~ASTUnio()
{
    destroyASTNodesVector(vars);
}

void ASTUnio::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTUnio::dup()
{
    return new ASTUnio(loc, duplicateASTNodes(vars));
}

ASTNode *ASTUnio::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNodesVector *vars = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    vars = dsGetASTNodesFromSList(*sl[2].internal.list);

    return new ASTUnio(loc, vars);
}

void ASTUnio::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (vars)
    {
        for (unsigned int i = 0; i < vars->size(); i++)
        {
            if ((*vars)[i] == oldc)
            {
                (*vars)[i] = newc;
                return;
            }
        }
    }
}

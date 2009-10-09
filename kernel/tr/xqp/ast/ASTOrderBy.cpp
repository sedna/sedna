/*
 * File:  ASTOrderBy.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderBy.h"

ASTOrderBy::~ASTOrderBy()
{
    destroyASTNodesVector(specs);
}

void ASTOrderBy::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOrderBy::dup()
{
    return new ASTOrderBy(cd, type, duplicateASTNodes(specs));
}

ASTNode *ASTOrderBy::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNodesVector *specs = NULL;
    OrdType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    specs = dsGetASTNodesFromSList(*sl[2].internal.list);
    type = OrdType(atol(sl[3].internal.num));

    return new ASTOrderBy(cd, type, specs);
}

void ASTOrderBy::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (specs)
    {
        for (unsigned int i = 0; i < specs->size(); i++)
        {
            if ((*specs)[i] == oldc)
            {
                (*specs)[i] = newc;
                return;
            }
        }
    }
}

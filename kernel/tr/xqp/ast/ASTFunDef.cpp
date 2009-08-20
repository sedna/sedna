/*
 * File:  ASTFunDef.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFunDef.h"

ASTFunDef::~ASTFunDef()
{
    destroyASTNodesVector(vars);
    delete fun;
}

void ASTFunDef::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTFunDef::dup()
{
    return new ASTFunDef(loc, duplicateASTNodes(vars), fun->dup());
}

ASTNode *ASTFunDef::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNodesVector *params = NULL;
    ASTNode *fun = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    params = dsGetASTNodesFromSList(*sl[2].internal.list);
    fun = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTFunDef(loc, params, fun);
}

void ASTFunDef::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
    if (fun == oldc)
    {
        fun = newc;
        return;
    }
}

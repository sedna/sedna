/*
 * File:  ASTTypeSwitch.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeSwitch.h"

ASTTypeSwitch::~ASTTypeSwitch()
{
    delete expr;
    destroyASTNodesVector(cases);
    delete def_case;
}

void ASTTypeSwitch::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTTypeSwitch::dup()
{
    return new ASTTypeSwitch(cd, expr->dup(), duplicateASTNodes(cases), def_case->dup());
}

ASTNode *ASTTypeSwitch::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNodesVector *cases = NULL;
    ASTNode *expr, *def;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    cases = dsGetASTNodesFromSList(*sl[3].internal.list);
    def = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTTypeSwitch(cd, expr, cases, def);
}

void ASTTypeSwitch::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (cases)
    {
        for (unsigned int i = 0; i < cases->size(); i++)
        {
            if ((*cases)[i] == oldc)
            {
                (*cases)[i] = newc;
                return;
            }
        }
    }
    if (def_case == oldc)
    {
        def_case = newc;
        return;
    }
}

/*
 * File:  ASTExtExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTExtExpr.h"

ASTExtExpr::~ASTExtExpr()
{
    destroyASTNodesVector(pragmas);
    delete expr;
}

void ASTExtExpr::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode* ASTExtExpr::dup()
{
    return new ASTExtExpr(cd, duplicateASTNodes(pragmas), (expr) ? expr->dup() : NULL);
}

ASTNode *ASTExtExpr::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNodesVector *pragmas = NULL;
    ASTNode *expr = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);

    pragmas = dsGetASTNodesFromSList(*sl[2].internal.list);
    expr = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTExtExpr(cd, pragmas, expr);
}

void ASTExtExpr::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (pragmas)
    {
        for (unsigned int i = 0; i < pragmas->size(); i++)
        {
            if ((*pragmas)[i] == oldc)
            {
                (*pragmas)[i] = newc;
                return;
            }
        }
    }
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}

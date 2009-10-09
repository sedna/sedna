/*
 * File:  ASTOrderByRet.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderByRet.h"

ASTOrderByRet::~ASTOrderByRet()
{
    delete iter_expr;
    delete ord_expr;
    delete ret_expr;
    destroyASTNodesVector(vars);
}

void ASTOrderByRet::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOrderByRet::dup()
{
    return new ASTOrderByRet(cd, iter_expr->dup(), ord_expr->dup(), ret_expr->dup(), duplicateASTNodes(vars));
}

ASTNode *ASTOrderByRet::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *iter = NULL, *ord = NULL, *ret = NULL;
    ASTNodesVector *vars = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    iter = dsGetASTFromSchemeList(*sl[2].internal.list);
    ord = dsGetASTFromSchemeList(*sl[3].internal.list);
    ret = dsGetASTFromSchemeList(*sl[4].internal.list);
    vars = dsGetASTNodesFromSList(*sl[5].internal.list);

    return new ASTOrderByRet(cd, iter, ord, ret, vars);
}

void ASTOrderByRet::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (iter_expr == oldc)
    {
        iter_expr = newc;
        return;
    }
    if (ord_expr == oldc)
    {
        ord_expr = newc;
        return;
    }
    if (ret_expr == oldc)
    {
        ret_expr = newc;
        return;
    }
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

/*
 * File:  ASTFor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFor.h"

ASTFor::~ASTFor()
{
    delete tv;
    delete pv;
    delete expr;
    delete fd;
}

void ASTFor::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

void ASTFor::setNextExpr(ASTNode *fd_)
{
    delete fd;

    fd = fd_;
}

ASTNodesVector *ASTFor::getVarList()
{
    ASTNodesVector *res;

    res = new ASTNodesVector();

    res->push_back(tv->dup());

    if (pv)
        res->push_back(pv->dup());

    return res;
}

ASTNode *ASTFor::dup()
{
    ASTFor *res;

    res = new ASTFor(loc, tv->dup(), (pv) ? pv->dup() : NULL, expr->dup());

    if (fd)
        res->setNextExpr(fd->dup());

    return res;
}

ASTNode *ASTFor::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *tv = NULL, *pv = NULL, *expr = NULL, *fd = NULL;
    ASTFor *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    tv = dsGetASTFromSchemeList(*sl[2].internal.list);
    pv = dsGetASTFromSchemeList(*sl[3].internal.list);
    expr = dsGetASTFromSchemeList(*sl[4].internal.list);
    fd = dsGetASTFromSchemeList(*sl[5].internal.list);

    res = new ASTFor(loc, tv, pv, expr);

    res->setNextExpr(fd);

    return res;
}

void ASTFor::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (tv == oldc)
    {
        tv = newc;
        return;
    }
    if (pv == oldc)
    {
        pv = newc;
        return;
    }
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (fd == oldc)
    {
        fd = newc;
        return;
    }
}

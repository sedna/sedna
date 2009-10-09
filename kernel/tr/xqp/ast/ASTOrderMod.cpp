/*
 * File:  ASTOrderMod.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderMod.h"

ASTOrderMod::~ASTOrderMod()
{
    delete ad_mod;
    delete em_mod;
    delete col_mod;
}

void ASTOrderMod::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOrderMod::dup()
{
    return new ASTOrderMod(cd, (ad_mod) ? ad_mod->dup() : NULL,
                           (em_mod) ? em_mod->dup() : NULL,
                            (col_mod) ? col_mod->dup() : NULL);
}

ASTNode *ASTOrderMod::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *ad = NULL, *em = NULL, *col = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    ad = dsGetASTFromSchemeList(*sl[2].internal.list);
    em = dsGetASTFromSchemeList(*sl[3].internal.list);
    col = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTOrderMod(cd, ad, em, col);
}

void ASTOrderMod::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (ad_mod == oldc)
    {
        ad_mod = newc;
        return;
    }
    if (em_mod == oldc)
    {
        em_mod = newc;
        return;
    }
    if (col_mod == oldc)
    {
        col_mod = newc;
        return;
    }
}

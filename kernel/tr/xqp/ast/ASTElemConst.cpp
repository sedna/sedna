/*
 * File:  ASTElemConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTElemConst.h"

ASTElemConst::~ASTElemConst()
{
    delete name;
    delete pref;
    delete local;
    delete expr;
}

void ASTElemConst::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTElemConst::dup()
{
    if (pref)
        return new ASTElemConst(cd, new std::string(*pref), new std::string(*local), (expr) ? expr->dup() : NULL);

    return new ASTElemConst(cd, name->dup(), (expr) ? expr->dup() : NULL);
}

ASTNode *ASTElemConst::createNode(scheme_list &sl)
{
    std::string *pref = NULL, *local = NULL;
    ASTNodeCommonData cd;
    ASTNode *name = NULL, *expr = NULL;
    ASTElemConst *res;

    U_ASSERT(sl[1].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);

    if (sl[2].type == SCM_LIST) // computed name
    {
        name = dsGetASTFromSchemeList(*sl[2].internal.list);

        U_ASSERT(sl[3].type == SCM_LIST && sl[4].type == SCM_BOOL);
        expr = dsGetASTFromSchemeList(*sl[3].internal.list);

        res = new ASTElemConst(cd, name, expr);

        res->deep_copy = sl[4].internal.b;
    }
    else
    {
        U_ASSERT(sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

        pref = new std::string(sl[2].internal.str);
        local = new std::string(sl[3].internal.str);

        U_ASSERT(sl[4].type == SCM_LIST && sl[5].type == SCM_BOOL);
        expr = dsGetASTFromSchemeList(*sl[4].internal.list);

        res = new ASTElemConst(cd, pref, local, expr);

        res->deep_copy = sl[5].internal.b;
    }

    return res;
}

void ASTElemConst::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (name == oldc)
    {
        name = newc;
        return;
    }
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}

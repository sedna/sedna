/*
 * File:  ASTPIConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPIConst.h"

ASTPIConst::~ASTPIConst()
{
    delete name;
    delete ncname;
    delete expr;
}

void ASTPIConst::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTPIConst::dup()
{
    if (ncname)
        return new ASTPIConst(cd, new std::string(*ncname), (expr) ? expr->dup() : NULL);

    return new ASTPIConst(cd, name->dup(), (expr) ? expr->dup() : NULL);
}

ASTNode *ASTPIConst::createNode(scheme_list &sl)
{
    std::string *ncname = NULL;
    ASTNodeCommonData cd;
    ASTNode *name = NULL, *expr = NULL;
    ASTPIConst *res;

    U_ASSERT(sl[1].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);

    if (sl[2].type == SCM_LIST) // computed name
    {
        name = dsGetASTFromSchemeList(*sl[2].internal.list);

        U_ASSERT(sl[3].type == SCM_LIST);
        expr = dsGetASTFromSchemeList(*sl[3].internal.list);

        res = new ASTPIConst(cd, name, expr);
    }
    else
    {
        U_ASSERT(sl[2].type == SCM_STRING);

        ncname = new std::string(sl[2].internal.str);

        U_ASSERT(sl[3].type == SCM_LIST);
        expr = dsGetASTFromSchemeList(*sl[3].internal.list);

        res = new ASTPIConst(cd, ncname, expr);
    }

    U_ASSERT(sl[4].type == SCM_BOOL);
    res->deep_copy = sl[4].internal.b;

    return res;
}

void ASTPIConst::modifyChild(const ASTNode *oldc, ASTNode *newc)
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

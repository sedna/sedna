/*
 * File:  ASTDeclareCopyNsp.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDeclareCopyNsp.h"

void ASTDeclareCopyNsp::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDeclareCopyNsp::dup()
{
    return new ASTDeclareCopyNsp(cd, pres_mod, inh_mod);
}

ASTNode *ASTDeclareCopyNsp::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    opt p_mod, i_mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER && sl[3].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    p_mod = opt(atoi(sl[2].internal.num));
    i_mod = opt(atoi(sl[3].internal.num));

    return new ASTDeclareCopyNsp(cd, p_mod, i_mod);
}

void ASTDeclareCopyNsp::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

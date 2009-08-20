/*
 * File:  ASTLibModule.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLibModule.h"

ASTLibModule::~ASTLibModule()
{
    delete prolog;
    delete moduleDecl;
}

void ASTLibModule::setVersionDecl(ASTNode *vd)
{
    dynamic_cast<ASTProlog *>(prolog)->addVersionDecl(vd);
}

void ASTLibModule::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTLibModule::dup()
{
    return new ASTLibModule(loc, moduleDecl->dup(), prolog->dup());
}

ASTNode *ASTLibModule::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *md = NULL, *prol = NULL;
    ASTLibModule *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    md = dsGetASTFromSchemeList(*sl[2].internal.list);
    prol = dsGetASTFromSchemeList(*sl[3].internal.list);

    res = new ASTLibModule(loc, md, prol);

    res->is_internal = true;

    return res;
}

void ASTLibModule::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (moduleDecl == oldc)
    {
        moduleDecl = newc;
        return;
    }
    if (prolog == oldc)
    {
        prolog = newc;
        return;
    }
}

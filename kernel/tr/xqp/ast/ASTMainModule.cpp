/*
 * File:  ASTMainModule.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMainModule.h"

ASTMainModule::~ASTMainModule()
{
    delete prolog;
    delete query;
}

void ASTMainModule::setVersionDecl(ASTNode *vd)
{
    dynamic_cast<ASTProlog *>(prolog)->addVersionDecl(vd);
}

void ASTMainModule::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTMainModule::dup()
{
    return new ASTMainModule(loc, prolog->dup(), query->dup());
}

ASTNode *ASTMainModule::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *query = NULL, *prol = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    prol = dsGetASTFromSchemeList(*sl[2].internal.list);
    query = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTMainModule(loc, prol, query);
}

void ASTMainModule::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (prolog == oldc)
    {
        prolog = newc;
        return;
    }
    if (query == oldc)
    {
        query = newc;
        return;
    }
}

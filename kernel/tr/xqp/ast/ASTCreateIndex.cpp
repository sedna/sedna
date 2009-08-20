/*
 * File:  ASTCreateIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateIndex.h"

ASTCreateIndex::~ASTCreateIndex()
{
    delete name;
    delete on_path;
    delete by_path;
    delete type;
}

void ASTCreateIndex::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCreateIndex::dup()
{
    return new ASTCreateIndex(loc, name->dup(), on_path->dup(), by_path->dup(), type->dup());
}

ASTNode *ASTCreateIndex::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *name = NULL, *on_path = NULL, *by_path = NULL, *type = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    name = dsGetASTFromSchemeList(*sl[2].internal.list);
    on_path = dsGetASTFromSchemeList(*sl[3].internal.list);
    by_path = dsGetASTFromSchemeList(*sl[4].internal.list);
    type = dsGetASTFromSchemeList(*sl[5].internal.list);

    return new ASTCreateIndex(loc, name, on_path, by_path, type);
}

void ASTCreateIndex::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (name == oldc)
    {
        name = newc;
        return;
    }
    if (on_path == oldc)
    {
        on_path = newc;
        return;
    }
    if (by_path == oldc)
    {
        by_path = newc;
        return;
    }
    if (type == oldc)
    {
        type = newc;
        return;
    }
}

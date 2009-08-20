/*
 * File:  ASTAttr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAttr.h"

ASTAttr::~ASTAttr()
{
    delete pref;
    delete local;
    delete uri;
    destroyASTNodesVector(cont);
}

void ASTAttr::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTAttr::dup()
{
    ASTAttr *res;

    res = new ASTAttr(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(cont));

    if (uri)
        res->uri = new std::string(*uri);

    return res;
}

ASTNode *ASTAttr::createNode(scheme_list &sl)
{
    std::string *pref = NULL, *local = NULL;
    ASTLocation loc;
    ASTNodesVector *cont = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);

    cont = dsGetASTNodesFromSList(*sl[4].internal.list);

    return new ASTAttr(loc, pref, local, cont);
}

void ASTAttr::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (cont)
    {
        for (unsigned int i = 0; i < cont->size(); i++)
        {
            if ((*cont)[i] == oldc)
            {
                (*cont)[i] = newc;
                return;
            }
        }
    }
}

/*
 * File:  ASTElem.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTElem.h"

ASTElem::~ASTElem()
{
    delete pref;
    delete local;
    destroyASTNodesVector(attrs);
    destroyASTNodesVector(cont);
}

void ASTElem::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTElem::dup()
{
    return new ASTElem(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(attrs), duplicateASTNodes(cont));
}

ASTNode *ASTElem::createNode(scheme_list &sl)
{
    std::string *pref = NULL, *local = NULL;
    ASTLocation loc;
    ASTNodesVector *attrs = NULL, *cont = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_LIST && sl[5].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);

    attrs = dsGetASTNodesFromSList(*sl[4].internal.list);
    cont = dsGetASTNodesFromSList(*sl[5].internal.list);

    return new ASTElem(loc, pref, local, attrs, cont);
}

void ASTElem::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (attrs)
    {
        for (unsigned int i = 0; i < attrs->size(); i++)
        {
            if ((*attrs)[i] == oldc)
            {
                (*attrs)[i] = newc;
                return;
            }
        }
    }
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

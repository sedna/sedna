/*
 * File:  ASTFLWOR.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFLWOR.h"

ASTFLWOR::~ASTFLWOR()
{
    destroyASTNodesVector(fls);
    delete where;
    delete order_by;
}

void ASTFLWOR::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTFLWOR::dup()
{
    return new ASTFLWOR(cd, duplicateASTNodes(fls), (where) ? where->dup() : NULL, (order_by) ? order_by->dup() : NULL, ret->dup());
}

ASTNode *ASTFLWOR::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *where_ = NULL, *order_by_ = NULL, *ret_ = NULL;
    ASTNodesVector *fls_ = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    fls_ = dsGetASTNodesFromSList(*sl[2].internal.list);
    where_ = dsGetASTFromSchemeList(*sl[3].internal.list);
    order_by_ = dsGetASTFromSchemeList(*sl[4].internal.list);
    ret_ = dsGetASTFromSchemeList(*sl[5].internal.list);

    return new ASTFLWOR(cd, fls_, where_, order_by_, ret_);
}

void ASTFLWOR::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (fls)
    {
        for (unsigned int i = 0; i < fls->size(); i++)
        {
            if ((*fls)[i] == oldc)
            {
                (*fls)[i] = newc;
                return;
            }
        }
    }
    if (where == oldc)
    {
        where = newc;
        return;
    }
    if (order_by == oldc)
    {
        order_by = newc;
        return;
    }
    if (ret == oldc)
    {
        ret = newc;
        return;
    }
}

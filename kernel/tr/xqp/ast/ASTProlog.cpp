/*
 * File:  ASTProlog.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTProlog.h"

ASTProlog::~ASTProlog()
{
    destroyASTNodesVector(decls);
}

void ASTProlog::addPrologDecl(ASTNode *decl)
{
    decls->push_back(decl);
}

void ASTProlog::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTProlog::dup()
{
    return new ASTProlog(cd, duplicateASTNodes(decls));
}

ASTNode *ASTProlog::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNodesVector *decls = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    decls = dsGetASTNodesFromSList(*sl[2].internal.list);

    return new ASTProlog(cd, decls);
}

void ASTProlog::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (decls)
    {
        for (unsigned int i = 0; i < decls->size(); i++)
        {
            if ((*decls)[i] == oldc)
            {
                (*decls)[i] = newc;
                return;
            }
        }
    }
}

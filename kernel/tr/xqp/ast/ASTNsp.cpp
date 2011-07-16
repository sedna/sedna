/*
 * File:  ASTNsp.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNsp.h"

ASTNsp::~ASTNsp()
{
    delete name;
    delete cont;
}

void ASTNsp::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTNsp::dup()
{
    return new ASTNsp(cd, new std::string(*name), (cont) ? new std::string(*cont) : NULL);
}

ASTNode *ASTNsp::createNode(scheme_list &sl)
{
    std::string *name = NULL;
    ASTNodeCommonData cd;
    std::string *cont = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);

    name = new std::string(sl[2].internal.str);

    if (sl.size() > 3)
    {
        U_ASSERT(sl[3].type == SCM_STRING);
        cont = new std::string(sl[3].internal.str);
    }

    ASTNsp * result = new ASTNsp(cd, name, cont);

    return result;
}

void ASTNsp::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

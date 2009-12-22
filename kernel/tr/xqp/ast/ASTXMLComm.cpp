/*
 * File:  ASTXMLComm.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTXMLComm.h"

ASTXMLComm::~ASTXMLComm()
{
    delete cont;
}

void ASTXMLComm::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTXMLComm::dup()
{
    return new ASTXMLComm(cd, new std::string(*cont));
}

ASTNode *ASTXMLComm::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *cont = NULL;
    ASTXMLComm *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_BOOL);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    cont = new std::string(sl[2].internal.str);

    res = new ASTXMLComm(cd, cont);

    res->deep_copy = sl[3].internal.b;

    return res;
}

void ASTXMLComm::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

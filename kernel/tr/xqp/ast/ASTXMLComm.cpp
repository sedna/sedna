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
    return new ASTXMLComm(loc, new std::string(*cont));
}

ASTNode *ASTXMLComm::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *cont = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    cont = new std::string(sl[2].internal.str);

    return new ASTXMLComm(loc, cont);
}

void ASTXMLComm::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

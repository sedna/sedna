/*
 * File:  ASTCharCont.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCharCont.h"

ASTCharCont::~ASTCharCont()
{
    delete cont;
}


bool ASTCharCont::isSpaceChars() const
{
    unsigned int i = 0;
    unsigned char c;

    while (i < cont->size())
    {
        c = (*cont)[i];

        if (c != ' ' && c != '\t' && c != '\n' && c != '\r')
            return false;

        i++;
    }

    return true;
}


void ASTCharCont::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCharCont::dup()
{
    return new ASTCharCont(loc, new std::string(*cont), orig);
}

ASTNode *ASTCharCont::createNode(scheme_list &sl)
{
    ASTLocation loc;
    Origin ori;
    std::string *cont;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    cont = new std::string(sl[2].internal.str);
    ori = Origin(atoi(sl[3].internal.num));

    return new ASTCharCont(loc, cont, ori);
}

void ASTCharCont::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

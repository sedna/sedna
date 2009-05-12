/*
 * File:  ASTCharCont.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

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
    v.visit(*this);
}

ASTNode *ASTCharCont::dup()
{
    return new ASTCharCont(loc, new std::string(*cont), orig);
}

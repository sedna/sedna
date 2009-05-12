/*
 * File:  ASTLoadFile.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTLoadFile.h"

ASTLoadFile::~ASTLoadFile()
{
    delete file;
    delete doc;
    delete coll;
}

std::string *ASTLoadFile::getFileName() const
{
    std::string *res = new std::string(*file);

    for (unsigned int i = 0; i < res->size(); i++)
        if ((*res)[i] == '\\')
            (*res)[i] = '/';

    return res;
}

void ASTLoadFile::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTLoadFile::dup()
{
    return new ASTLoadFile(loc, new std::string(*file), new std::string(*doc), (coll) ? new std::string(*coll) : NULL);
}

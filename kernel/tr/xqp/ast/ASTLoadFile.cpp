/*
 * File:  ASTLoadFile.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

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
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTLoadFile::dup()
{
    return new ASTLoadFile(cd, new std::string(*file), new std::string(*doc), (coll) ? new std::string(*coll) : NULL);
}

ASTNode *ASTLoadFile::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *file = NULL, *doc = NULL, *coll = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    file = new std::string(sl[2].internal.str);
    doc = new std::string(sl[3].internal.str);

    if (sl.size() > 4)
    {
        U_ASSERT(sl[4].type == SCM_STRING);
        coll = new std::string(sl[4].internal.str);
    }

    return new ASTLoadFile(cd, file, doc, coll);
}

void ASTLoadFile::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

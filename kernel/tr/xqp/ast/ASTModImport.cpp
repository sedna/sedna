/*
 * File:  ASTModImport.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTModImport.h"

ASTModImport::~ASTModImport()
{
    delete name;
    delete uri;

    destroyASTStringVector(hints);
}

void ASTModImport::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTModImport::dup()
{
    ASTStringVector *vec;

    vec = duplicateASTStringVector(hints);

    return new ASTModImport(loc, (name == NULL) ? NULL : new std::string(*name), (uri == NULL) ? NULL : new std::string(*uri), vec);
}

ASTNode *ASTModImport::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *name = NULL, *uri = NULL;
    ASTStringVector *hints = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    name = new std::string(sl[2].internal.str);
    uri = new std::string(sl[3].internal.str);
    hints = dsGetASTStringsFromSList(*sl[4].internal.list);

    if (*name == "") // small workaround about absent NCName (since NCName cannot be "" then we store NULL as "")
    {
        delete name;
        name = NULL;
    }

    return new ASTModImport(loc, name, uri, hints);
}

void ASTModImport::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

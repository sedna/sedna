/*
 * File:  ASTVersionDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVersionDecl.h"

ASTVersionDecl::~ASTVersionDecl()
{
    delete xq_version;
    delete encoding;
}

void ASTVersionDecl::accept(ASTVisitor &v)
{
   v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTVersionDecl::dup()
{
    return new ASTVersionDecl(loc, new std::string(*xq_version), (encoding == NULL) ? NULL : new std::string(*encoding));
}

ASTNode *ASTVersionDecl::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *vers = NULL, *enc = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    vers = new std::string(sl[2].internal.str);

    if (sl.size() > 3)
    {
        U_ASSERT(sl[3].type == SCM_STRING);
        enc = new std::string(sl[3].internal.str);
    }

    return new ASTVersionDecl(loc, vers, enc);
}

void ASTVersionDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

/*
 * File:  ASTOrderModInt.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderModInt.h"

ASTOrderModInt::~ASTOrderModInt()
{
    delete uri;
}

void ASTOrderModInt::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOrderModInt::dup()
{
    return new ASTOrderModInt(loc, mod, (uri) ? new std::string(*uri) : NULL);
}

ASTNode *ASTOrderModInt::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *uri = NULL;
    OrderMod type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    type = OrderMod(atol(sl[2].internal.num));

    if (sl.size() > 3)
    {
         U_ASSERT(sl[3].type == SCM_STRING);
         uri = new std::string(sl[3].internal.str);
    }

    return new ASTOrderModInt(loc, type, uri);
}

void ASTOrderModInt::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

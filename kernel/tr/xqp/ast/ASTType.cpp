/*
 * File:  ASTType.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTType.h"

ASTType::~ASTType()
{
    delete name;
}

void ASTType::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTType::dup()
{
    return new ASTType(loc, new std::string(*name), type);
}

ASTNode *ASTType::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *name = NULL;
    TypeMod type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER && sl[3].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    type = TypeMod(atol(sl[2].internal.num));
    name = new std::string(sl[3].internal.str);

    return new ASTType(loc, name, type);
}

void ASTType::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

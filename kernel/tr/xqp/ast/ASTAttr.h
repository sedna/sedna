/*
 * File:  ASTAttr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ATTR_H_
#define _AST_ATTR_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTAttr : public ASTNode
{
public:
    std::string *pref, *local; // attribute name
    ASTNodesVector *cont; // attribute content; may be NULL

public:
    ASTAttr(ASTLocation &loc, std::string *name, ASTNodesVector *cont_ = NULL) : ASTNode(loc), cont(cont_)
    {
        ASTParseQName(name, &pref, &local);

        delete name;
    }

    ASTAttr(ASTLocation &loc, std::string *elem_pref, std::string *elem_local, ASTNodesVector *cont_ = NULL) :
            ASTNode(loc),
            pref(elem_pref),
            local(elem_local),
            cont(cont_)
    {}

    ~ASTAttr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

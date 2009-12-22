/*
 * File:  ASTAttr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ATTR_H_
#define _AST_ATTR_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTAttr : public ASTNode
{
public:
    std::string *pref, *local; // attribute name
    ASTNodesVector *cont; // attribute content; may be NULL

    std::string *uri; // added by sema; just cached uri, don't need to serialize
    bool deep_copy; // attribute will be attached to virtual_root and copied on demand

public:
    ASTAttr(const ASTNodeCommonData &loc, std::string *name, ASTNodesVector *cont_ = NULL) : ASTNode(loc), cont(cont_), uri(NULL)
    {
        ASTParseQName(name, &pref, &local);

        delete name;
    }

    ASTAttr(const ASTNodeCommonData &loc, std::string *elem_pref, std::string *elem_local, ASTNodesVector *cont_ = NULL) :
            ASTNode(loc),
            pref(elem_pref),
            local(elem_local),
            cont(cont_),
            uri(NULL)
    {}

    ~ASTAttr();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

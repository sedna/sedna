/*
 * File:  ASTElem.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ELEM_H_
#define _AST_ELEM_H_

#include "ASTNode.h"
class ASTVisitor;
class ASTNsp;

#include <string>

class ASTElem : public ASTNode
{
public:
    std::string *pref, *local; // element name
    ASTNodesVector *attrs; // attributes; may be NULL
    ASTNodesVector *cont; // element content; may be NULL

    bool deep_copy; // element will be attached to virtual_root and copied on demand

    const ASTNsp * nsp_node; /* Namespace node, needed to resolve prefix */
    int nsp_node_index; // Needed to store namespace node

public:
    ASTElem(const ASTNodeCommonData &loc, std::string *name, ASTNodesVector *attrs_ = NULL, ASTNodesVector *cont_ = NULL) :
            ASTNode(loc),
            attrs(attrs_),
            cont(cont_),
            nsp_node(NULL),
            nsp_node_index(-1)
    {
        ASTParseQName(name, &pref, &local);

        delete name;
    }

    ASTElem(const ASTNodeCommonData &loc, std::string *elem_pref, std::string *elem_local, ASTNodesVector *attrs_ = NULL, ASTNodesVector *cont_ = NULL) :
            ASTNode(loc),
            pref(elem_pref),
            local(elem_local),
            attrs(attrs_),
            cont(cont_),
            nsp_node(NULL),
            nsp_node_index(-1)
    {
    }

    ~ASTElem();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

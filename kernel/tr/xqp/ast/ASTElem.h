#ifndef _AST_ELEM_H_
#define _AST_ELEM_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTElem : public ASTNode
{
public:
    std::string *pref, *local; // element name

    ASTNodesVector *attrs; // attributes; may be NULL
    ASTNodesVector *cont; // element content; may be NULL

public:
    ASTElem(ASTLocation &loc, std::string *name, ASTNodesVector *attrs_ = NULL, ASTNodesVector *cont_ = NULL) : ASTNode(loc), attrs(attrs_), cont(cont_)
    {
        ASTParseQName(name, &pref, &local);
    }

    ASTElem(ASTLocation &loc, std::string *elem_pref, std::string *elem_local, ASTNodesVector *attrs_ = NULL, ASTNodesVector *cont_ = NULL) :
            ASTNode(loc),
            pref(elem_pref),
            local(elem_local),
            attrs(attrs_),
            cont(cont_)
    {
    }

    ~ASTElem();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

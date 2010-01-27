/*
 * File:  ASTCharCont.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CHAR_CONT_H_
#define _AST_CHAR_CONT_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTCharCont : public ASTNode
{
public:
    enum Origin
    {
        DIRECT,
        CDATA,
        CREF
    };

    std::string *cont; // character content
    Origin orig; // origin of this content (CDATA, CharRef or just plain symbols)

public:
    ASTCharCont(const ASTNodeCommonData &loc, std::string *cont_, Origin o = DIRECT) : ASTNode(loc), cont(cont_), orig(o) {}
    ASTCharCont(const ASTNodeCommonData &loc, const char *cont_, Origin o = DIRECT) : ASTNode(loc), orig(o)
    {
        cont = new std::string(cont_);
    }

    ~ASTCharCont();

    void appendContent(ASTCharCont *cont_)
    {
        cont->append(*(cont_->cont));
    }

    ASTCharCont::Origin getOrigin() const
    {
        return orig;
    }

    bool isSpaceChars() const;

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

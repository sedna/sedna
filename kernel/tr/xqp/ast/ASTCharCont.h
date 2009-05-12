/*
 * File:  ASTCharCont.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CHAR_CONT_H_
#define _AST_CHAR_CONT_H_

#include "ASTNode.h"
#include "AST.h"

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
    ASTCharCont(ASTLocation &loc, std::string *cont_, Origin o = DIRECT) : ASTNode(loc), cont(cont_), orig(o) {}
    ASTCharCont(ASTLocation &loc, const char *cont_, Origin o = DIRECT) : ASTNode(loc), orig(o)
    {
        cont = new std::string(cont_);
    }

    ~ASTCharCont();

    void appendContent(std::string *cont_)
    {
        cont->append(*cont_);
        delete cont_;
    }

    void appendContent(const char *cont_)
    {
        cont->append(cont_);
    }

    void appendContent(ASTCharCont *cont_)
    {
        cont->append(*(cont_->cont));
        delete cont_;
    }

    ASTCharCont::Origin getOrigin() const
    {
        return orig;
    }

    bool isSpaceChars() const;

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

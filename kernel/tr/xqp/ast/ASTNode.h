/*
 * File:  ASTNode.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_NODE_H_
#define _AST_NODE_H_

#if defined(__GNUC__) && (__GNUC__ >= 4) && (__GNUC_MINOR__ >= 2)
#pragma GCC diagnostic ignored "-Wparentheses"
#endif /* GNUC */

#include "tr/xqp/location.hh"

#if defined(__GNUC__) && (__GNUC__ >= 4) && (__GNUC_MINOR__ >= 2)
#pragma GCC diagnostic warning "-Wparentheses"
#endif /* GNUC */

class ASTVisitor;
typedef sedna::location ASTLocation;

struct ASTNodeCommonData
{
    ASTLocation loc;
    bool isCached;

    ASTNodeCommonData()
    {
        isCached = false;
    }

    ASTNodeCommonData(const ASTLocation &loca)
    {
        loc = loca;
        isCached = false;
    }
};

class ASTNode
{
public:
    ASTNodeCommonData cd;

public:
    ASTNode(const ASTNodeCommonData &common)
    {
        cd = common;
    }

    // destructor should deal with children
    virtual ~ASTNode() {}

    // accept visitor
    // children are traversed by visitor
    virtual void accept(ASTVisitor &v) = 0;

    // returns first line of a definition
    int getFirstLine() const
    {
        return cd.loc.begin.line;
    }

    ASTLocation getLocation() const
    {
        return cd.loc;
    }

    ASTLocation *getLocationAddr()
    {
        return &(cd.loc);
    }

    // deep copy of the corresponding subtree
    virtual ASTNode *dup() = 0;

    // modifies one of the children based on pointer-eq
    virtual void modifyChild(const ASTNode *oldc, ASTNode *newc) = 0;
};

#endif

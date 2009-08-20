/*
 * File:  ASTDropColl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DROP_COLL_H_
#define _AST_DROP_COLL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDropColl : public ASTNode
{
public:
    ASTNode *coll;

public:
    ASTDropColl(ASTLocation &loc, ASTNode *coll_) : ASTNode(loc), coll(coll_) {}

    ~ASTDropColl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

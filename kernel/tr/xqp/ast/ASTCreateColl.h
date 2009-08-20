/*
 * File:  ASTCreateColl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_COLL_H_
#define _AST_CREATE_COLL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTCreateColl : public ASTNode
{
public:
    ASTNode *coll;

public:
    ASTCreateColl(ASTLocation &loc, ASTNode *coll_) : ASTNode(loc), coll(coll_) {}

    ~ASTCreateColl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

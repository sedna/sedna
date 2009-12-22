/*
 * File:  ASTDropIndex.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DROP_INDEX_H_
#define _AST_DROP_INDEX_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTDropIndex : public ASTNode
{
public:
    ASTNode *index;

public:
    ASTDropIndex(const ASTNodeCommonData &loc, ASTNode *index_) : ASTNode(loc), index(index_) {}

    ~ASTDropIndex();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

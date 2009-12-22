/*
 * File:  ASTRenameColl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_RENAME_COLL_H_
#define _AST_RENAME_COLL_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTRenameColl : public ASTNode
{
public:
    ASTNode *name_old, *name_new;

public:
    ASTRenameColl(const ASTNodeCommonData &loc, ASTNode *old_, ASTNode *new_) : ASTNode(loc), name_old(old_), name_new(new_) {}

    ~ASTRenameColl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

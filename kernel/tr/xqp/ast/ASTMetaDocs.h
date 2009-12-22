/*
 * File:  ASTMetaDocs.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_META_DOCS_H_
#define _AST_META_DOCS_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTMetaDocs : public ASTNode
{
public:
    ASTNode *coll;
    bool need_stats;

public:
    ASTMetaDocs(const ASTNodeCommonData &loc, ASTNode *coll_, bool stats) : ASTNode(loc), coll(coll_), need_stats(stats) {}

    ~ASTMetaDocs();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

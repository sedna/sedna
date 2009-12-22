/*
 * File:  ASTMetaSchemaCol.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_META_SCHEMA_COLL_H_
#define _AST_META_SCHEMA_COLL_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTMetaSchemaCol : public ASTNode
{
public:
    ASTNode *coll;

public:
    ASTMetaSchemaCol(const ASTNodeCommonData &loc, ASTNode *coll_) : ASTNode(loc), coll(coll_) {}

    ~ASTMetaSchemaCol();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

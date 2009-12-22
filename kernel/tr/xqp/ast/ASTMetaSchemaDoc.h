/*
 * File:  ASTMetaSchemaDoc.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_META_SCHEMA_DOC_H_
#define _AST_META_SCHEMA_DOC_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTMetaSchemaDoc : public ASTNode
{
public:
    ASTNode *doc, *coll;


public:
    ASTMetaSchemaDoc(const ASTNodeCommonData &loc, ASTNode *doc_, ASTNode *coll_ = NULL) : ASTNode(loc), doc(doc_), coll(coll_) {}

    ~ASTMetaSchemaDoc();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTCreateDoc.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_CREATE_DOC_H_
#define _AST_CREATE_DOC_H_

#include "ASTNode.h"
#include "AST.h"

class ASTCreateDoc : public ASTNode
{
public:
    ASTNode *doc, *coll;

public:
    ASTCreateDoc(const ASTNodeCommonData &loc, ASTNode *doc_, ASTNode *coll_ = NULL) : ASTNode(loc), doc(doc_), coll(coll_) {}

    ~ASTCreateDoc();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

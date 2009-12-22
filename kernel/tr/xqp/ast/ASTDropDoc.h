/*
 * File:  ASTDropDoc.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DROP_DOC_H_
#define _AST_DROP_DOC_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTDropDoc : public ASTNode
{
public:
    ASTNode *doc, *coll;

public:
    ASTDropDoc(const ASTNodeCommonData &loc, ASTNode *doc_, ASTNode *coll_ = NULL) : ASTNode(loc), doc(doc_), coll(coll_) {}

    ~ASTDropDoc();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTFLWOR.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_FLWOR_H_
#define _AST_FLWOR_H_

#include "ASTNode.h"
class ASTVisitor;

class ASTFLWOR : public ASTNode
{
public:
    ASTNodesVector *fls; // for-let exprs
    ASTNode *where, *order_by, *ret;

public:
    ASTFLWOR(const ASTNodeCommonData &loc, ASTNodesVector *fls_, ASTNode *where_, ASTNode *ob_, ASTNode *ret_) :
        ASTNode(loc),
        fls(fls_),
        where(where_),
        order_by(ob_),
        ret(ret_)
    {}

    ~ASTFLWOR();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

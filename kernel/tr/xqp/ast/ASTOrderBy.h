/*
 * File:  ASTOrderBy.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ORDER_BY_H_
#define _AST_ORDER_BY_H_

#include "ASTNode.h"
class ASTVisitor;

#include <vector>

#include "tr/xqp/ast/ASTOrderSpec.h"

class ASTOrderBy : public ASTNode
{
public:
    enum OrdType
    {
        STABLE,
        UNSTABLE
    };

    ASTNodesVector *specs;
    OrdType type;

public:
    ASTOrderBy(const ASTNodeCommonData &loc, ASTOrderBy::OrdType t, ASTNodesVector *ord_specs = NULL) : ASTNode(loc), specs(ord_specs), type(t) {}

    void addSpec(ASTOrderSpec *spec)
    {
        specs->push_back(spec);
    }

    ~ASTOrderBy();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

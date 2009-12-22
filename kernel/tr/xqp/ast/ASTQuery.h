/*
 * File:  ASTQuery.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_QUERY_H_
#define _AST_QUERY_H_

#include "ASTNode.h"
class ASTVisitor;

#include <vector>

class ASTQuery : public ASTNode
{
public:
    enum QueryType
    {
        QUERY,
        CREATE,
        UPDATE,
        META
    };

    ASTNode *query;
    QueryType type;

    bool is_trigger;

public:
    ASTQuery(const ASTNodeCommonData &loc, ASTNode *expr, ASTQuery::QueryType qtype) : ASTNode(loc), query(expr), type(qtype)
    {
        is_trigger = false;
    }

    ~ASTQuery();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

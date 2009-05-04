#ifndef _AST_QUERY_H_
#define _AST_QUERY_H_

#include "ASTNode.h"
#include "AST.h"

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

public:
    ASTQuery(ASTLocation &loc, ASTNode *expr, ASTQuery::QueryType qtype) : ASTNode(loc), query(expr), type(qtype) {}

    ~ASTQuery();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

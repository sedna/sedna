#ifndef _AST_ORDER_MOD_INT_H_
#define _AST_ORDER_MOD_INT_H_

#include "ASTNode.h"
#include "AST.h"


class ASTOrderModInt : public ASTNode
{
public:
    enum OrderMod
    {
        ASCENDING,
        DESCENDING,

        EMPTY_GREATEST,
        EMPTY_LEAST,

        COLLATION
    };

    OrderMod mod;

    std::string *uri; // uri for collation

public:
    ASTOrderModInt(ASTLocation &loc, ASTOrderModInt::OrderMod ord_mod, std::string *coll_uri = NULL) : ASTNode(loc), mod(ord_mod), uri(coll_uri) {}

    ~ASTOrderModInt();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

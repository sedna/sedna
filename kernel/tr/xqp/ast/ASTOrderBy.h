#ifndef _AST_ORDER_BY_H_
#define _AST_ORDER_BY_H_

#include "ASTNode.h"
#include "AST.h"

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
    ASTOrderBy(ASTLocation &loc, ASTOrderBy::OrdType t, ASTNodesVector *ord_specs = NULL) : ASTNode(loc), specs(ord_specs), type(t) {}

    void addSpec(ASTOrderSpec *spec)
    {
        specs->push_back(spec);
    }

    ~ASTOrderBy();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

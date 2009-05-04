#ifndef _AST_TYPESWITCH_H_
#define _AST_TYPESWITCH_H_

#include "ASTNode.h"
#include "AST.h"

#include <vector>

class ASTCase;

class ASTTypeSwitch : public ASTNode
{
public:
    ASTNode *expr; // operand expression
    ASTNodesVector *cases;
    ASTCase *def_case;

public:
    ASTTypeSwitch(ASTLocation &loc, ASTNode *op_expr, ASTNodesVector *cs, ASTCase *dc) : ASTNode(loc), expr(op_expr), cases(cs), def_case(dc) {}

    ~ASTTypeSwitch();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

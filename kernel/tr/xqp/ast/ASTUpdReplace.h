#ifndef _AST_UPDATE_REPLACE_H_
#define _AST_UPDATE_REPLACE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTFunDef;

class ASTUpdReplace : public ASTNode
{
public:
    ASTNode *what;
    ASTFunDef *new_expr;

public:
    ASTUpdReplace(ASTLocation &loc, ASTNode *what_, ASTFunDef *new_expr_) : ASTNode(loc), what(what_), new_expr(new_expr_) {}

    ~ASTUpdReplace();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

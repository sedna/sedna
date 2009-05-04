#ifndef _AST_VAR_DECL_H_
#define _AST_VAR_DECL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTVar;
class ASTTypeSeq;

class ASTVarDecl : public ASTNode
{
public:
    ASTVar *var;
    ASTTypeSeq *type;
    ASTNode *expr; // NULL means that variable is external

public:
    ASTVarDecl(ASTLocation &loc, ASTVar *vard, ASTTypeSeq *var_type = NULL, ASTNode *var_expr = NULL) : ASTNode(loc), var(vard), type(var_type), expr(var_expr) {}

    ~ASTVarDecl();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

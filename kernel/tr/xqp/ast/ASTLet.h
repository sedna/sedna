#ifndef _AST_LET_H_
#define _AST_LET_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeVar;
class FunDef;

class ASTLet : public ASTNode
{
public:
    ASTTypeVar *tv; // main for variable
    ASTNode *expr; // for expression

    ASTFunDef *fd;  // FunDef expression for the final for-clause representation

public:
    ASTLet(ASTLocation &loc, ASTTypeVar *var, ASTNode *let_expr) : ASTNode(loc), tv(var), expr(let_expr), fd(NULL) {}

    ~ASTLet();

    void accept(ASTVisitor &v);

    ASTTypeVar *getVar()
    {
        return tv;
    }

    // returns list containing COPY of variables (usual and pos)
    ASTNodesVector *getVarList();

    // we set funDef when we cough up the final For-clause representation
    void setFunDef(ASTFunDef *funDef);

    ASTNode *dup();
};

#endif

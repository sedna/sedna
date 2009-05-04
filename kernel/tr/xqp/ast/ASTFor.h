#ifndef _AST_FOR_H_
#define _AST_FOR_H_

#include "ASTNode.h"
#include "AST.h"

class ASTTypeVar;
class ASTPosVar;
class ASTFunDef;

class ASTFor : public ASTNode
{
public:
    ASTTypeVar *tv; // main for variable
    ASTPosVar *pv; // positional variable; can be NULL
    ASTNode *expr; // for expression

    ASTFunDef *fd;  // FunDef expression for the final for-clause representation

public:
    ASTFor(ASTLocation &loc, ASTTypeVar *var, ASTPosVar *pos_var, ASTNode *for_expr) : ASTNode(loc), tv(var), pv(pos_var), expr(for_expr), fd(NULL) {}

    ~ASTFor();

    void accept(ASTVisitor &v);

    ASTTypeVar *getVar()
    {
        return tv;
    }

    ASTPosVar *getPosVar()
    {
        return pv;
    }

    // returns list containing COPY of variables (usual and pos)
    ASTNodesVector *getVarList();

    // we set funDef when we cough up the final For-clause representation
    void setFunDef(ASTFunDef *funDef);

    ASTNode *dup();
};

#endif

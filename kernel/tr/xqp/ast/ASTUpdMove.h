#ifndef _AST_UPDATE_MOVE_H_
#define _AST_UPDATE_MOVE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTFunDef;

class ASTUpdMove : public ASTNode
{
public:
    enum UpdType
    {
        PRECEDING,
        INTO,
        FOLLOWING
    };

    ASTNode *what;
    ASTFunDef *where;
    UpdType type;

public:
    ASTUpdMove(ASTLocation &loc, ASTNode *what_, ASTFunDef *where_, UpdType type_) : ASTNode(loc), what(what_), where(where_), type(type_) {}

    ~ASTUpdMove();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

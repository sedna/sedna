#ifndef _AST_UPDATE_DEL_H_
#define _AST_UPDATE_DEL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTUpdDel : public ASTNode
{
public:
    enum DelType
    {
        DEEP,
        UNDEEP
    };

    ASTNode *what;
    DelType type;

public:
    ASTUpdDel(ASTLocation &loc, ASTNode *what_, DelType type_) : ASTNode(loc), what(what_), type(type_) {}

    ~ASTUpdDel();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

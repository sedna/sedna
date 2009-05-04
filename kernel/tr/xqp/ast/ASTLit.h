#ifndef _AST_LIT_H_
#define _AST_LIT_H_

#include "ASTNode.h"
#include "AST.h"

class ASTLit : public ASTNode
{
public:
    enum LitType
    {
        INTEGER,
        DOUBLE,
        DECIMAL,
        STRING
    };

    LitType type;
    std::string *lit;

public:
    ASTLit(ASTLocation &loc, LitType type_, std::string *lit_) : ASTNode(loc), type(type_), lit(lit_) {}

    ~ASTLit();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

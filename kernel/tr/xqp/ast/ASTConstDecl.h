#ifndef _AST_CONST_DECL_H_
#define _AST_CONST_DECL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTConstDecl : public ASTNode
{
public:
    enum opt
    {
        STRIP,
        PRESERVE
    };

    ASTConstDecl::opt mod; // strip/preserve modificator

public:
    ASTConstDecl(ASTLocation &loc, ASTConstDecl::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTConstDecl() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

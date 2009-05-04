#ifndef _AST_BOUND_SPACE_DECL_H_
#define _AST_BOUND_SPACE_DECL_H_

#include "ASTNode.h"
#include "AST.h"

class ASTBoundSpaceDecl : public ASTNode
{
public:
    enum opt
    {
        STRIP,
        PRESERVE
    };

    ASTBoundSpaceDecl::opt mod; // strip/preserve modificator

public:
    ASTBoundSpaceDecl(ASTLocation &loc, ASTBoundSpaceDecl::opt decl) : ASTNode(loc), mod(decl) {}

    ~ASTBoundSpaceDecl() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

#ifndef _AST_ORDER_MOD_H_
#define _AST_ORDER_MOD_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOrderModInt;

class ASTOrderMod : public ASTNode
{
public:
    ASTOrderModInt *ad_mod;
    ASTOrderModInt *em_mod;
    ASTOrderModInt *col_mod;


public:
    ASTOrderMod(ASTLocation &loc, ASTOrderModInt *ad = NULL, ASTOrderModInt *em = NULL, ASTOrderModInt *cm = NULL)
        : ASTNode(loc),
          ad_mod(ad),
          em_mod(em),
          col_mod(cm) {}

    ~ASTOrderMod();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

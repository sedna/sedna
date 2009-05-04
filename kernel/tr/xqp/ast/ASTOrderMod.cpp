#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderMod.h"

ASTOrderMod::~ASTOrderMod()
{
    delete ad_mod;
    delete em_mod;
    delete col_mod;
}

void ASTOrderMod::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderMod::dup()
{
    return new ASTOrderMod(loc, (ad_mod) ? static_cast<ASTOrderModInt *>(ad_mod->dup()) : NULL,
                           (em_mod) ? static_cast<ASTOrderModInt *>(em_mod->dup()) : NULL,
                            (col_mod) ? static_cast<ASTOrderModInt *>(col_mod->dup()) : NULL);
}

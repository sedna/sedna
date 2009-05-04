#ifndef _AST_DECLARE_COPY_NSP_H_
#define _AST_DECLARE_COPY_NSP_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDeclareCopyNsp : public ASTNode
{
public:
    enum opt
    {
        PRESERVE,
        NO_PRESERVE,
        INHERIT,
        NO_INHERIT
    };

    ASTDeclareCopyNsp::opt pres_mod, inh_mod; // preserve and inherit modificators

public:
    ASTDeclareCopyNsp(ASTLocation &loc, ASTDeclareCopyNsp::opt pres_decl, ASTDeclareCopyNsp::opt inh_decl) : ASTNode(loc), pres_mod(pres_decl), inh_mod(inh_decl) {}

    ~ASTDeclareCopyNsp() {}

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

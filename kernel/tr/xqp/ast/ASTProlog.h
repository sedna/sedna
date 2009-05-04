#ifndef _AST_PROLOG_H_
#define _AST_PROLOG_H_

#include "ASTNode.h"
#include "AST.h"

#include <vector>

class ASTProlog : public ASTNode
{
public:
    ASTNodesVector *decls;

public:
    ASTProlog(ASTLocation &loc, ASTNodesVector *decls_) : ASTNode(loc), decls(decls_) {}

    ASTProlog(ASTLocation &loc) : ASTNode(loc)
    {
        decls = new ASTNodesVector();
    }

    void addVersionDecl(ASTNode *vd)
    {
        decls->insert(decls->begin(), vd);
    }

    ~ASTProlog();

    void addPrologDecl(ASTNode *decl);
    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

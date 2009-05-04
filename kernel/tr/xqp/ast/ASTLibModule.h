#ifndef _AST_LIB_MODULE_H_
#define _AST_LIB_MODULE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTModuleDecl;
class ASTProlog;

class ASTLibModule : public ASTNode
{
public:
    ASTModuleDecl *moduleDecl; // module declaration
    ASTProlog *prolog; // query prolog; not NULL

public:
    ASTLibModule(ASTLocation &loc, ASTModuleDecl *md, ASTProlog *prol) : ASTNode(loc), moduleDecl(md), prolog(prol) {}

    void setVersionDecl(ASTNode *vd);

    ~ASTLibModule();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

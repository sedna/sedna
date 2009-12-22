/*
 * File:  ASTProlog.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_PROLOG_H_
#define _AST_PROLOG_H_

#include "ASTNode.h"
class ASTVisitor;

#include <vector>

class ASTProlog : public ASTNode
{
public:
    ASTNodesVector *decls;

public:
    ASTProlog(const ASTNodeCommonData &loc, ASTNodesVector *decls_) : ASTNode(loc), decls(decls_) {}

    ASTProlog(const ASTNodeCommonData &loc) : ASTNode(loc)
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
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

/*
 * File:  ASTTypeVar.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TYPE_VAR_H_
#define _AST_TYPE_VAR_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTTypeVar : public ASTNode
{
public:
    ASTNode *type; // ASTTypeSeq for usual variables, ASTType for internal variables (e.g. xs:anyType for fun-def variables)

    ASTNode *var; // ASTVar

public:
    ASTTypeVar(const ASTNodeCommonData &loc, ASTNode *typep, ASTNode *vard) : ASTNode(loc), type(typep), var(vard) {}

    ~ASTTypeVar();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

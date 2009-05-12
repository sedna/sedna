/*
 * File:  ASTVersionDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_VERSION_DECL_H_
#define _AST_VERSION_DECL_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTVersionDecl : public ASTNode
{
public:
    std::string *xq_version, *encoding;

public:
    ASTVersionDecl(ASTLocation &loc, std::string *version, std::string *enc = NULL) : ASTNode(loc), xq_version(version), encoding(enc) {}

    ~ASTVersionDecl();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

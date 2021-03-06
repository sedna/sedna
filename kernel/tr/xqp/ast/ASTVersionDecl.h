/*
 * File:  ASTVersionDecl.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_VERSION_DECL_H_
#define _AST_VERSION_DECL_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTVersionDecl : public ASTNode
{
public:
    std::string *xq_version, *encoding;

public:
    ASTVersionDecl(const ASTNodeCommonData &loc, std::string *version, std::string *enc = NULL) : ASTNode(loc), xq_version(version), encoding(enc) {}

    ~ASTVersionDecl();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

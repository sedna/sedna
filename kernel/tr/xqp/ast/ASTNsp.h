/*
 * File:  ASTNsp.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_NSP_H_
#define _AST_NSP_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTNsp : public ASTNode
{
public:
    std::string *name; // namespace name (NCName)
    std::string *cont; // namespace content; may be NULL; cannot be sequence-like since specs disallow this
    bool boundToElement; // This one is a hint, that tells, that this node is used as proper element namespace.

public:
    ASTNsp(const ASTNodeCommonData &loc, std::string *name_, std::string *cont_ = NULL) : ASTNode(loc), name(name_), cont(cont_), boundToElement(false) {}

    ~ASTNsp();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

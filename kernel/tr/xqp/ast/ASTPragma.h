/*
 * File:  ASTPragma.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_PRAGMA_H_
#define _AST_PRAGMA_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTPragma : public ASTNode
{
public:
    std::string *pref, *local; // pragma name
    std::string *cont; // pragma content

public:
    ASTPragma(const ASTNodeCommonData &loc, std::string *pragma_name, std::string *pragma_cont);

    ASTPragma(const ASTNodeCommonData &loc, std::string *pr_pref, std::string *pr_local, std::string *pragma_cont);

    ~ASTPragma();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

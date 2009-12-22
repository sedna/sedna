/*
 * File:  ASTModImport.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_MOD_IMPORT_H_
#define _AST_MOD_IMPORT_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTModImport : public ASTNode
{
public:
    std::string *name, *uri; // name may be NULL
    ASTStringVector *hints;

public:
    ASTModImport(const ASTNodeCommonData &loc, std::string *mod_nsp, std::string *nsp_uri, ASTStringVector *mod_hints) : ASTNode(loc), name(mod_nsp), uri(nsp_uri), hints(mod_hints) {}

    ~ASTModImport();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

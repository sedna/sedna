/*
 * File:  ASTDefCollation.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DEF_COLLATION_H_
#define _AST_DEF_COLLATION_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDefCollation : public ASTNode
{
public:
    std::string *uri;

public:
    ASTDefCollation(ASTLocation &loc, std::string *coll_uri) : ASTNode(loc), uri(coll_uri) {}

    ~ASTDefCollation();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif

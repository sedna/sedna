/*
 * File:  ASTDropTrg.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_DROP_TRIGGER_H_
#define _AST_DROP_TRIGGER_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTDropTrg : public ASTNode
{
public:
    std::string *trg;

public:
    ASTDropTrg(const ASTNodeCommonData &loc, std::string *trg_) : ASTNode(loc), trg(trg_) {}

    ~ASTDropTrg();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif

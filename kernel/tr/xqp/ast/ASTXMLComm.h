/*
 * File:  ASTXMLComm.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_XML_COMM_H_
#define _AST_XML_COMM_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTXMLComm : public ASTNode
{
public:
    std::string *cont; // character content

public:
    ASTXMLComm(ASTLocation &loc, std::string *cont_) : ASTNode(loc), cont(cont_) {}

    ~ASTXMLComm();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

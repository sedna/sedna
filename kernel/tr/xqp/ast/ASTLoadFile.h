/*
 * File:  ASTLoadFile.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_LOAD_FILE_H_
#define _AST_LOAD_FILE_H_

#include "ASTNode.h"
#include "AST.h"

class ASTLoadFile : public ASTNode
{
public:
    std::string *file, *doc, *coll;

public:
    ASTLoadFile(ASTLocation &loc, std::string *file_, std::string *doc_, std::string *coll_ = NULL) : ASTNode(loc), file(file_), doc(doc_), coll(coll_) {}

    ~ASTLoadFile();

    std::string *getFileName() const;

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif

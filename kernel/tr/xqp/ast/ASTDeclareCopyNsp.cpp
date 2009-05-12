/*
 * File:  ASTDeclareCopyNsp.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDeclareCopyNsp.h"

void ASTDeclareCopyNsp::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDeclareCopyNsp::dup()
{
    return new ASTDeclareCopyNsp(loc, pres_mod, inh_mod);
}

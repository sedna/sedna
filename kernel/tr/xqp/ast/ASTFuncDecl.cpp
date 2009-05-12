/*
 * File:  ASTFuncDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFuncDecl.h"

ASTFuncDecl::ASTFuncDecl(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params,
                            ASTTypeSeq *ret_type, ASTNode *func_body)
        : ASTNode(loc),
          params(func_params),
          ret(ret_type),
          body(func_body)
{
    ASTParseQName(func_name, &pref, &local);
}

ASTFuncDecl::ASTFuncDecl(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params,
                         ASTTypeSeq *ret_type, ASTNode *func_body)

        : ASTNode(loc),
          pref(fun_pref),
          local(fun_local),
          params(func_params),
          ret(ret_type),
          body(func_body)
{
}

ASTFuncDecl::~ASTFuncDecl()
{
    delete pref;
    delete local;
    destroyASTNodesVector(params);
    delete ret;
    delete body;
}

void ASTFuncDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTFuncDecl::dup()
{
    return new ASTFuncDecl(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(params),
                           (ret == NULL) ? NULL : static_cast<ASTTypeSeq *>(ret->dup()), (body == NULL) ? NULL : body->dup());
}

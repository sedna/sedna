/*
 * File:  ASTFuncDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFuncDecl.h"

ASTFuncDecl::ASTFuncDecl(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params,
                            ASTNode *ret_type, ASTNode *func_body)
        : ASTNode(loc),
          params(func_params),
          ret(ret_type),
          body(func_body),
          func_uri(NULL)
{
    ASTParseQName(func_name, &pref, &local);

    delete func_name;
}

ASTFuncDecl::ASTFuncDecl(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params,
                         ASTNode *ret_type, ASTNode *func_body)

        : ASTNode(loc),
          pref(fun_pref),
          local(fun_local),
          params(func_params),
          ret(ret_type),
          body(func_body),
          func_uri(NULL)
{
}

ASTFuncDecl::~ASTFuncDecl()
{
    delete pref;
    delete local;
    delete func_uri;
    destroyASTNodesVector(params);
    delete ret;
    delete body;
}

void ASTFuncDecl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTFuncDecl::dup()
{
    ASTFuncDecl *fd;

    fd =  new ASTFuncDecl(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(params),
                           (ret == NULL) ? NULL : static_cast<ASTTypeSeq *>(ret->dup()), (body == NULL) ? NULL : body->dup());

    if (func_uri)
        fd->func_uri = new std::string(func_uri->c_str());

    return fd;
}

ASTNode *ASTFuncDecl::createNode(scheme_list &sl)
{
    std::string *pref = NULL, *local = NULL;
    ASTLocation loc;
    ASTNodesVector *params = NULL;
    ASTNode *ret = NULL, *body = NULL;
    ASTFuncDecl *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_LIST &&
            sl[5].type == SCM_LIST && sl[6].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);

    params = dsGetASTNodesFromSList(*sl[4].internal.list);
    ret = dsGetASTFromSchemeList(*sl[5].internal.list);
    body = dsGetASTFromSchemeList(*sl[6].internal.list);

    res = new ASTFuncDecl(loc, pref, local, params, ret, body);

    if (sl.size() > 7)
    {
        U_ASSERT(sl[7].type == SCM_STRING);
        res->func_uri = new std::string(sl[7].internal.str);
    }

    return res;
}

void ASTFuncDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (params)
    {
        for (unsigned int i = 0; i < params->size(); i++)
        {
            if ((*params)[i] == oldc)
            {
                (*params)[i] = newc;
                return;
            }
        }
    }
    if (ret == oldc)
    {
        ret = newc;
        return;
    }
    if (body == oldc)
    {
        body = newc;
        return;
    }
}

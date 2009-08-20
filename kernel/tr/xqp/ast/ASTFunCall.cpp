/*
 * File:  ASTFunCall.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFunCall.h"

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params) : ASTNode(loc), params(func_params)
{
    ASTParseQName(func_name, &pref, &local);

    uri = NULL;
    int_name = NULL;

    delete func_name;
}

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNode *func_param) : ASTNode(loc)
{
    ASTParseQName(func_name, &pref, &local);

    uri = NULL;
    int_name = NULL;

    params = new ASTNodesVector();
    params->push_back(func_param);

    delete func_name;
}

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNode *func_param1, ASTNode *func_param2) : ASTNode(loc)
{
    ASTParseQName(func_name, &pref, &local);

    uri = NULL;
    int_name = NULL;

    params = new ASTNodesVector();
    params->push_back(func_param1);
    params->push_back(func_param2);

    delete func_name;
}

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNode *func_param1, ASTNode *func_param2, ASTNode *func_param3) : ASTNode(loc)
{
    ASTParseQName(func_name, &pref, &local);

    uri = NULL;
    int_name = NULL;

    params = new ASTNodesVector();
    params->push_back(func_param1);
    params->push_back(func_param2);
    params->push_back(func_param3);

    delete func_name;
}

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params)
        : ASTNode(loc),
          pref(fun_pref),
          local(fun_local),
          params(func_params)
{
    uri = NULL;
    int_name = NULL;
}

ASTFunCall::~ASTFunCall()
{
    delete pref;
    delete local;
    destroyASTNodesVector(params);

    delete uri;
    delete int_name;
}

void ASTFunCall::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTFunCall::dup()
{
    ASTFunCall *res;

    res = new ASTFunCall(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(params));

    if (uri)
        res->uri = new std::string(*uri);

    if (int_name)
        res->int_name = new std::string(*int_name);

    return res;
}

ASTNode *ASTFunCall::createNode(scheme_list &sl)
{
    std::string *pref = NULL, *local = NULL;
    ASTLocation loc;
    ASTNodesVector *params = NULL;
    ASTFunCall *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);

    params = dsGetASTNodesFromSList(*sl[4].internal.list);

    res = new ASTFunCall(loc, pref, local, params);

    if (sl.size() > 5)
    {
        U_ASSERT(sl[5].type == SCM_STRING);
        res->uri = new std::string(sl[5].internal.str);

        if (sl.size() > 6)
        {
            U_ASSERT(sl[6].type == SCM_STRING);
            res->int_name = new std::string(sl[6].internal.str);
        }
    }

    return res;
}

void ASTFunCall::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
}

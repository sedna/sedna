/*
 * File:  ASTOption.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOption.h"

ASTOption::~ASTOption()
{
    delete pref;
    delete local;
    delete opt;

    delete uri;
    delete options;
}

void ASTOption::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOption::dup()
{
    ASTOption *res;

    res = new ASTOption(cd, new std::string(*pref), new std::string(*local), new std::string(*opt));

    if (uri)
        res->uri = new std::string(uri->c_str());

    if (options)
    {
        res->options = new std::vector<option>;

        for (unsigned int i = 0; i < options->size(); i++)
        {
            res->options->push_back(option((*options)[i].first, (*options)[i].second));
        }
    }

    return res;
}

ASTNode *ASTOption::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *pref = NULL, *local = NULL, *opt = NULL;
    ASTOption *res;
    scheme_list *sl_opts = NULL, *sl_opt;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);
    opt = new std::string(sl[4].internal.str);

    res =  new ASTOption(cd, pref, local, opt);

    if (sl.size() > 5)
    {
        U_ASSERT(sl[5].type == SCM_STRING && sl[6].type == SCM_LIST);

        res->uri = new std::string(sl[5].internal.str);

        sl_opts = sl[6].internal.list;

        res->options = new std::vector<option>;

        for (unsigned int i = 0; i < sl_opts->size(); i++)
        {
            U_ASSERT((*sl_opts)[i].type == SCM_LIST);

            sl_opt = (*sl_opts)[i].internal.list;

            U_ASSERT((*sl_opt)[0].type == SCM_STRING && (*sl_opt)[1].type == SCM_STRING);

            res->options->push_back(option((*sl_opt)[0].internal.str, (*sl_opt)[1].internal.str));
        }
    }

    return res;
}

void ASTOption::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}

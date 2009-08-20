/*
 * File:  ASTCreateTrg.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateTrg.h"

ASTCreateTrg::~ASTCreateTrg()
{
    delete name;
    delete path;
    destroyASTNodesVector(do_exprs);

    delete leaf_name;
    delete trimmed_path;
}

void ASTCreateTrg::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCreateTrg::dup()
{
    ASTCreateTrg *res;
    res = new ASTCreateTrg(loc, new std::string(*name), t_mod, a_mod, path->dup(), g_mod, duplicateASTNodes(do_exprs));

    if (leaf_name)
        res->leaf_name = new std::string(*leaf_name);

    res->leaf_type = leaf_type;

    if (trimmed_path)
        res->trimmed_path = trimmed_path->dup();

    return res;
}

ASTNode *ASTCreateTrg::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *name;
    ASTNode *path = NULL;
    ASTNodesVector *expr = NULL;
    TrgMod ab, idr, ns;
    ASTCreateTrg *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_NUMBER && sl[4].type == SCM_NUMBER && sl[5].type == SCM_LIST &&
             sl[6].type == SCM_NUMBER && sl[7].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    name = new std::string(sl[2].internal.str);
    ab = TrgMod(atol(sl[3].internal.num));
    idr = TrgMod(atol(sl[4].internal.num));
    path = dsGetASTFromSchemeList(*sl[5].internal.list);
    ns = TrgMod(atol(sl[6].internal.num));
    expr = dsGetASTNodesFromSList(*sl[7].internal.list);

    res = new ASTCreateTrg(loc, name, ab, idr, path, ns, expr);

    if (sl.size() > 8)
    {
        U_ASSERT(sl[8].type == SCM_STRING && sl[9].type == SCM_NUMBER && sl[10].type == SCM_LIST);

        res->leaf_name = new std::string(sl[8].internal.str);
        res->leaf_type = atol(sl[9].internal.num);
        res->trimmed_path = dsGetASTFromSchemeList(*sl[10].internal.list);
    }

    return res;
}

void ASTCreateTrg::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (path == oldc)
    {
        path = newc;
        return;
    }
    if (do_exprs)
    {
        for (unsigned int i = 0; i < do_exprs->size(); i++)
        {
            if ((*do_exprs)[i] == oldc)
            {
                (*do_exprs)[i] = newc;
                return;
            }
        }
    }
    if (trimmed_path == oldc)
    {
        trimmed_path = newc;
        return;
    }
}

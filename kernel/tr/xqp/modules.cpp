/*
 * File:  modules.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/XQuerytoLR.h"
#include "tr/auth/auc.h"
#include <string>
#include "tr/executor/root/PPQueryRoot.h"

char *get_module(const char *module_uri)
{
    char *res = NULL;
    size_t size = 0;
    PPSubQuery *tree = NULL;
    bool tree_built = false;
    bool tree_opened = false;
    bool mem_alloced = false;

    db_entity *me = se_new db_entity;
    me->name = se_new char[strlen(module_uri) + 1];
    strcpy(me->name, module_uri);
    me->type = dbe_module;

    /* authorization check */
    auth_for_query(counted_ptr<db_entity>(me));

    std::string module_xquery = std::string("string(doc('") + module_uri + "', '$modules')/module/text())";

    try
    {
        tree = dynamic_cast<PPSubQuery *>(build_subquery_qep(module_xquery.c_str(), TL_XQuery));
        U_ASSERT(tree != NULL);
        tree_built = true;

        tuple_cell tc;
        tuple t = tuple(1);

        tree->open();
        tree_opened = true;

        /*
         * get data
         *
         * for module we shoul always obtain something since we throw an exception otherwise
         */
        if (!tree->next(t))
            throw USER_EXCEPTION2(SE1003, "Error in get_module function");

        if (!t.cells[0].is_atomic() || t.cells[0].get_atomic_type() != xs_string)
            throw USER_EXCEPTION2(SE1003, "Error in get_module function");

        U_ASSERT(t.cells[0].get_strlen() < SIZE_MAX);

        size = (size_t)(t.cells[0].get_strlen());
        res = (char*)malloc(size + 1);
        if (!res)
            throw USER_EXCEPTION2(SE1003, "Error in get_module function");
        mem_alloced = true;
        t.cells[0].copy_string(res);

        if (tree->next(t))
            throw USER_EXCEPTION2(SE1003, "Error in get_module function");

        // close qep tree
        tree->close();

        delete tree;
        tree = NULL;

    }
    catch (SednaUserException)
    {
        if (mem_alloced) free(res);
        if (tree_opened) tree->close();
        if (tree_built)  delete tree;

        res = (char*)malloc(sizeof(char) * 3);
        strcpy(res, "#f");
    }

    return res;
}

std::string prepare_modules(const std::vector<client_file> &cf_vec, std::string *module_name)
{
    char buf[1000];
    std::string  plain_batch_text, module;
    StringVector batch, arr;
    sedna::XQueryDriver *xqd; // to parse library module

    for (unsigned int i = 0; i < cf_vec.size(); i++)
    {
        const client_file & f = cf_vec.at(i);
        plain_batch_text = "";

        while (!f.stream->eof()) 
        {
            f.stream->read(buf, sizeof(buf));
            plain_batch_text.append(buf, f.stream->gcount());
        }

        batch.push_back(plain_batch_text);
    }

    xqd = new sedna::XQueryDriver();

    try
    {
        parse_batch(xqd, TL_XQueryMod, batch, module_name);

        if (xqd->getModulesCount() == 0)
            throw USER_EXCEPTION2(4001, "failed to parse library module");

        module = "(";
        for (unsigned int i = 0; i < xqd->getModulesCount(); i++)
            module.append(xqd->getIRRepresentation(i));
        module.append(")");
    }
    catch (SednaUserException)
    {
        delete xqd;
        throw;
    }

    delete xqd;

    return module;
}

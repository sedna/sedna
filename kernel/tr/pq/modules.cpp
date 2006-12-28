/*
 * File:  por2qep.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "por2qep.h"

const char *get_module(const char *module_uri)
{
    char *res = NULL;
    int size = 0;
    qep_subtree *tree = NULL;
    bool tree_built = false;
    bool tree_opened = false;
    bool mem_alloced = false;


    std::string module_query_in_por = "(1 (PPFnString (1 (PPAxisChild text () (1 (PPAxisChild qname (\"\" \"module\") (1 (PPDocInCol (1 (PPConst \"$modules\" !xs!string)) (1 (PPConst \"";
    module_query_in_por += module_uri;
    module_query_in_por += "\" !xs!string))))))))))";

    try {
        tree = build_qep(module_query_in_por.c_str(), 0);
        tree_built = true;
        tuple_cell tc;
        tuple t = tuple(1);

        // open qep tree
        tree->tree.op->open();
        tree_opened = true;

        // execute
        tree->tree.op->next(t);
        if (!t.cells[0].is_atomic() || t.cells[0].get_atomic_type() != xs_string)
            throw USER_EXCEPTION2(SE1003, "Error in get_module function");

        size = t.cells[0].get_strlen();
        // Andrey Fomichev: I use malloc here because this piece of memory will be
        // released by Chicken (by C-call free())
        res = (char*)malloc(size + 1);
        if (!res)
            throw USER_EXCEPTION2(SE1003, "Error in get_module function"); 
        mem_alloced = true;
        t.cells[0].copy_string(res);

        tree->tree.op->next(t);
        if (!t.is_eos()) 
            throw USER_EXCEPTION2(SE1003, "Error in get_module function"); 

        // close qep tree
        tree->tree.op->close();

        delete_qep(tree);
        tree = NULL;

    } catch (SednaUserException &e) {
        if (mem_alloced) free(res);
        if (tree_opened) tree->tree.op->close();
        if (tree_built)  delete_qep(tree);

        res = (char*)malloc(sizeof(char) * 3);
        strcpy(res, "#f");
    }

    return res;
}

/*
Insert this into .scm module where you are going to call the get-module function
(declare (foreign-declare "const char* c_get_module(const char*);"))
(define get-module (foreign-callback-lambda c-string* "c_get_module" c-string))
*/

extern "C" {

const char *c_get_module(const char * module_uri)
{
    return get_module(module_uri);
}

}


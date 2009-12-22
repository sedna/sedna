/*
 * File:  pq.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include <string>
#include "tr/pq/pq.h"
#include "common/errdbg/d_printf.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/tr_globals.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "chicken.h"
#include "tr/structures/schema.h"
#include "tr/structures/metadata.h"
#include "common/utils.h"
#include "tr/tr_utils.h"
#include "chicken_panic.h"
#include <cassert>

#ifdef SE_MEMORY_TRACK
#undef malloc
#undef free
#endif

using namespace std;

EXTERN_DECLARE_TIME_VARS

StmntsArray* prepare_phys_repr(const string &query_in_LR, QueryType type);

//-----------------------------------------------
// DL: safer input string for Scheme part

// The forward declaration is important here, since there is no way of
// specifying the extern "C" declaration within a friend declaration
extern "C" char* get_scm_input_string();

class Part_scheme_input
{
    friend char* get_scm_input_string();
private:
    static char *scm_input_string;
    static bool was_allocated;
private:
    Part_scheme_input();  // Prohibits object creation
public:
    static void allocate_input(const std::string &in_prototype);
};

char*   Part_scheme_input::scm_input_string = 0;
bool    Part_scheme_input::was_allocated    = false;

void Part_scheme_input::allocate_input(const std::string &in_prototype)
{
    // I call malloc here because this memory
    // will be freed later by Chicken (Andrey)
    scm_input_string =
        static_cast<char*>(malloc(sizeof(char) * (in_prototype.size() + 1)));
    if(!scm_input_string)
    {
       throw USER_EXCEPTION(SE4009);
    }
    ::strcpy(scm_input_string, in_prototype.c_str());
    was_allocated = true;
}

//-----------------------------------------------

static char *scm_output_string = NULL;
static bool Chicken_initialized = false;

StmntsArray* prepare_phys_repr(const string &query_in_LR, QueryType type)
{
    GET_TIME(&t1_scm);

    int status = 0;

        // prepare query for sending to scheme part
    StmntsArray *st_array = se_new StmntsArray();
    script_struct st;

    Part_scheme_input::allocate_input(query_in_LR);

    if (!Chicken_initialized)
    {
        status = CHICKEN_initialize(0, 0, 0, (void*)C_toplevel);
        //d_printf2("CHICKEN_initialize = %d\n", status);
        // Overriding usual_panic:
        C_panic_hook = chicken_panic_throw_exception;
        status = CHICKEN_run(NULL);
        //d_printf2("CHICKEN_run = %d\n", status);
        Chicken_initialized = true;
    }

    char query_string[128];
    memset(query_string, '\0', 128);
    strcat(query_string, "(process-query-in-scheme ");
    strcat(query_string, int2string((int)type).c_str());
    strcat(query_string, ")");

    C_word scheme_eval_res;
    status = CHICKEN_eval_string(query_string, &scheme_eval_res);
    if (status != 1)
    {
        char buf[1024];
        CHICKEN_get_error_message (buf, 1024);
        //d_printf2("Error evaluating Scheme part = %s\n", buf);
        throw USER_EXCEPTION2(SE4004, buf);
    }

    //d_printf2("Scheme part evaluation result: %s\n", scm_output_string);

    string por(scm_output_string);
    free(scm_output_string);
    scm_output_string = NULL;

    scheme_list *qep_trees_in_scheme_lst = NULL;
    qep_trees_in_scheme_lst = make_tree_from_scheme_list(por.c_str());

    if (   qep_trees_in_scheme_lst->size() < 2
        || qep_trees_in_scheme_lst->at(0).type != SCM_BOOL)
       throw USER_EXCEPTION(SE4005);


    if (!qep_trees_in_scheme_lst->at(0).internal.b)
    {
       string  error = (qep_trees_in_scheme_lst->at(1).internal.list)->at(2).internal.str;
       // d_printf2("error str=%s\n", error.c_str());
       int error_num = atoi((qep_trees_in_scheme_lst->at(1).internal.list)->at(1).internal.num);

       delete_scheme_list(qep_trees_in_scheme_lst);
       delete st_array;

       throw USER_EXCEPTION2(error_num, error.c_str());
    }

    int size = qep_trees_in_scheme_lst->size();

    for (int k = 1; k < size; k++)
    {
        if (qep_trees_in_scheme_lst->at(k).type != SCM_LIST)
           throw USER_EXCEPTION(SE4005);

        st.stmnt = qep_trees_in_scheme_lst->at(k).internal.list;

        st_array->stmnts.push_back(st);
    }

    st_array->root = qep_trees_in_scheme_lst;

    GET_TIME(&t2_scm);
    ADD_TIME(t_total_scm, t1_scm, t2_scm);

    return st_array;
}


std::string prepare_modules(const std::vector<client_file> &cf_vec, std::string *module_name)
{
    //read from file
    char buf[1000];
    string  plain_batch_text, module;
    StringVector batch, arr;
    sedna::XQueryDriver *xqd; // to parse library module

    for (unsigned int i = 0; i < cf_vec.size(); i++)
    {
        plain_batch_text = "";

        while(!feof(cf_vec[i].f))
        {
            size_t len= fread(buf, sizeof(char), sizeof(buf), cf_vec[i].f);

            plain_batch_text.append(buf, len);
        }

        batch.push_back(plain_batch_text);
    }

    xqd = new sedna::XQueryDriver();

    parse_batch(xqd, TL_XQuery, batch, module_name);

    if (xqd->getModulesCount() == 0)
        throw USER_EXCEPTION2(4001, "faile to parse library module");

    module = "(";
    for (unsigned int i = 0; i < xqd->getModulesCount(); i++)
        module.append(xqd->getIRRepresentation(i));
    module.append(")");

    delete xqd;

    return module;
}

std::string prepare_module(std::string init_module)
{
    int status = 0;

    Part_scheme_input::allocate_input(init_module);

    if (!Chicken_initialized)
    {
        status = CHICKEN_initialize(0, 0, 0, (void*)C_toplevel);
        //d_printf2("CHICKEN_initialize = %d\n", status);
        // Overriding usual_panic:
        C_panic_hook = chicken_panic_throw_exception;
        status = CHICKEN_run(NULL);
        //d_printf2("CHICKEN_run = %d\n", status);
        Chicken_initialized = true;
    }

    char query_string[128];
    memset(query_string, '\0', 128);
    strcat(query_string, "(process-module-in-scheme ");
    strcat(query_string, int2string((int)TL_ForAuth).c_str());
    strcat(query_string, ")");

    C_word scheme_eval_res;
    status = CHICKEN_eval_string(query_string, &scheme_eval_res);

    if (status != 1)
    {
        char buf[1024];
        CHICKEN_get_error_message (buf, 1024);
        //d_printf2("Error evaluating Scheme part = %s\n", buf);
        throw USER_EXCEPTION2(SE4004, buf);
    }

    string pc_module(scm_output_string);
    free(scm_output_string);
    scm_output_string = NULL;

    scheme_list *qep_trees_in_scheme_lst = NULL;
    qep_trees_in_scheme_lst = make_tree_from_scheme_list(pc_module.c_str());

    if (qep_trees_in_scheme_lst->size() < 3 || qep_trees_in_scheme_lst->at(0).type != SCM_BOOL)
        throw USER_EXCEPTION(SE4005);


    if (!qep_trees_in_scheme_lst->at(0).internal.b)
    {
        string error = (qep_trees_in_scheme_lst->at(1).internal.list)->at(2).internal.str;
        // d_printf2("error str=%s\n", error.c_str());
        throw USER_EXCEPTION2(atoi((qep_trees_in_scheme_lst->at(1).internal.list)->at(1).internal.num), error.c_str());
    }

    std::string res = qep_trees_in_scheme_lst->at(1).internal.str;
    delete_scheme_list(qep_trees_in_scheme_lst);

    return res;
}



// wrappers for log library functions, etc
extern "C" {

extern void scm_error(int code, char* message, char* component);
extern void scm_debug(int code, char* message, char* component);

void scm_error(int code, char* message, char* component)
{
//    d_printf2("error: %s\n", message);
//      error(code, message, component);
//      free(message);
//      free(component);
}

void scm_debug(int code, char* message, char* component)
{
//    d_printf2("debug: %s\n", message);
//      debug(code, message, component);
//      free(message);
//      free(component);
}



// boolean values: 0 (negative) or 1 (affirmative)
int is_run_rewriter()               { return tr_globals::run_rewriter; }
int is_run_popt()                   { return tr_globals::run_popt; }
int is_show_time()                  { return tr_globals::show_time; }
int is_print_intermed()             { return tr_globals::print_intermed; }
char* get_user_login()              { return tr_globals::login; }
int is_server_mode()                { return tr_globals::server_mode; }
int is_authorization()              { return tr_globals::authorization; }
int is_authentication()             { return tr_globals::authentication; }
int is_first_transaction()          { return tr_globals::first_transaction; }

//extern "C"
char* get_scm_input_string()
{
    assert(
        Part_scheme_input::scm_input_string &&
        Part_scheme_input::was_allocated &&
        "Attempting to pass a badly allocated scm_input_string to Chicken"
        );
    const char *const res = Part_scheme_input::scm_input_string;
    Part_scheme_input::scm_input_string = 0;
    Part_scheme_input::was_allocated    = false;
    // c-string* (will free memory)
    return const_cast<char*>(res);
}

void  set_scm_output_string(char* s)/*{ scm_output_string = s; }*/      // c-string (will be copied)
{
    int size = strlen(s);
    scm_output_string = (char*)malloc(sizeof(char) * (size + 1));
    strcpy(scm_output_string, s);
}
} // end of extern "C"




///////////////////////////////////////////////////////////////////////////////
/// Functions for converting descriptive schema to Scheme lists
///////////////////////////////////////////////////////////////////////////////

#include <sstream>

void convert_schema_node_to_scheme_list(schema_node_cptr node, std::ostream& sstr)
{
    sstr << "(";
    switch (node->type)
    {
        case element        : sstr << "elem";
                              break;
        case text           : sstr << "text";
                              break;
        case attribute      : sstr << "attr";
                              break;
        case document       : sstr << "doc";
                              break;
        case xml_namespace  : sstr << "ns";
                              break;
        case comment        : sstr << "comm";
                              break;
        case pr_ins         : sstr << "pr-ins";
                              break;
        default             : throw USER_EXCEPTION2(SE1051, "Unexpected node type in convert_schema_node_to_scheme_list");
    }

    if (node->name)
    {
        sstr << " (\"";
        if ((node->get_xmlns() != NULL) && node->get_xmlns()->uri) sstr << node->get_xmlns()->uri;
        sstr << "\" \"" << node->name << "\")";
    }
    sstr << " (" << int2string(node->nodecnt) << " " << int2string(node->blockcnt) << " " << int2string(node->extnids) << ")";

    for (sc_ref_item *ref = node->children.first; ref != NULL; ref = ref->next)
    {
        if (ref == node->children.first) sstr << " ";
        convert_schema_node_to_scheme_list(ref->object.snode, sstr);
    }
    sstr << ")";
}

char* descriptive_schema_to_scheme_list(const char* name, int is_collection)
{
    //d_printf3("descriptive_schema_to_scheme_list parameters: %s %d\n", name, is_collection);

    schema_node_cptr root = XNULL;

    if (is_collection) root = find_collection(name);
    else root = find_document(name);

    if (!root.found())
    {
        if (is_collection)
            throw USER_EXCEPTION2(SE2003, (std::string("Collection '") + name + "'").c_str());
        else
            throw USER_EXCEPTION2(SE2006, (std::string("Document '") + name + "'").c_str());
    }

    std::ostringstream sstr;
    convert_schema_node_to_scheme_list(root, sstr);

    char *str = (char*)malloc(sstr.str().length() + 1);
    strcpy(str, sstr.str().c_str());
    return str;
}

extern "C" {

char* __c__descriptive_schema_to_scheme_list(const char* name, int is_collection)
{
    return descriptive_schema_to_scheme_list(name, is_collection);
}

}

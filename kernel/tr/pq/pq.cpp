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

#ifdef SE_MEMORY_TRACK
#undef malloc
#undef free
#endif

using namespace std;

EXTERN_DECLARE_TIME_VARS

StmntsArray* prepare_phys_repr(const string &query_in_LR, QueryType type);

StmntsArray *prepare_stmnt(QueryType type, const char *stmnt)
{
    if (type != TL_XQuery && type != TL_POR && type != TL_ForSemAnal &&
        type != TL_ForAuth && type != TL_ForMarkLRet && type != TL_ForConvToPOR &&
        type != TL_POR)
        throw USER_EXCEPTION(SE4002);
//    d_printf2("Parser %s\n --------------------------- \n", stmnt);
    StringVector v = parse_batch(type, stmnt);

    if (v.size() >1) 
       throw USER_EXCEPTION(SE4003);

    StmntsArray* stmnt_;
    stmnt_ = prepare_phys_repr(v[0].c_str(), type);

    return stmnt_;

}


static char *scm_input_string = NULL;
static char *scm_output_string = NULL;


StmntsArray* prepare_phys_repr(const string &query_in_LR, QueryType type)
{
    GET_TIME(&t1_scm);

    static bool Chicken_initialized = false;
    int status = 0;

        // prepare query for sending to scheme part
    StmntsArray *st_array = se_new StmntsArray();
    script_struct st;

    scm_input_string = (char*)malloc(query_in_LR.size() + 1); // I call malloc here because this memory
                                                              // will be freed later by Chicken (Andrey)
    if (scm_input_string == NULL)
       throw USER_EXCEPTION(SE4009);
    strcpy(scm_input_string, query_in_LR.c_str());


    if (!Chicken_initialized)
    {
        status = CHICKEN_initialize(0, 0, 0, (void*)C_toplevel);
        //d_printf2("CHICKEN_initialize = %d\n", status);
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

    scm_input_string = NULL;
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



std::string prepare_module(FILE* f, std::string& out_module_name)
{
   //read from file
   char buf[1000];
   string  plain_batch_text;
   int status = 0;


   while(!feof(f))
   {
     
      size_t len= fread(buf, sizeof(char), sizeof(buf), f);

      plain_batch_text.append(buf, len);
   }

   StringVector v = parse_batch(TL_XQuery, plain_batch_text.c_str());


   scm_input_string = (char*)malloc(v[0].size() + 1); // I call malloc here because this memory
                                                                  // will be freed later by Chicken (Andrey)
   if (scm_input_string == NULL)
      throw USER_EXCEPTION(SE4009);

   strcpy(scm_input_string, v[0].c_str());

   char query_string[128];
   memset(query_string, '\0', 128);
   strcat(query_string, "(process-module-in-scheme ");
   strcat(query_string, int2string((int)TL_XQuery).c_str());
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

    scm_input_string = NULL;
    scm_output_string = NULL;

    scheme_list *qep_trees_in_scheme_lst = NULL;
    qep_trees_in_scheme_lst = make_tree_from_scheme_list(pc_module.c_str());
    
    if (   qep_trees_in_scheme_lst->size() < 3
        || qep_trees_in_scheme_lst->at(0).type != SCM_BOOL) 
       throw USER_EXCEPTION(SE4005);


    if (!qep_trees_in_scheme_lst->at(0).internal.b)
    {
       string  error = (qep_trees_in_scheme_lst->at(1).internal.list)->at(2).internal.str;
       // d_printf2("error str=%s\n", error.c_str());
       throw USER_EXCEPTION2(atoi((qep_trees_in_scheme_lst->at(1).internal.list)->at(1).internal.num), error.c_str());
    }

    out_module_name = qep_trees_in_scheme_lst->at(2).internal.str;
    return qep_trees_in_scheme_lst->at(1).internal.str;
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


//extern int is_run_rewriter();
//extern int is_show_time();
//extern int is_print_intermed();
//extern int get_user_id();
//extern int is_server_mode();
//extern int is_auth();

// boolean values: 0 (negative) or 1 (affirmative)
int is_run_rewriter()				{ return run_rewriter; }
int is_show_time()					{ return show_time; }
int is_print_intermed()				{ return print_intermed; }
char* get_user_login()				{ return login; }
int is_server_mode()				{ return server_mode; }
int is_auth()                       { return auth; }
int is_run_popt()					{ return run_popt; }

char* get_scm_input_string()	    { return scm_input_string; }	// c-string* (will free memory)
void  set_scm_output_string(char* s)/*{ scm_output_string = s; }*/		// c-string (will be copied)
{
    int size = strlen(s);
    scm_output_string = (char*)malloc(size + 1);
    strcpy(scm_output_string, s);
}
} // end of extern "C"




///////////////////////////////////////////////////////////////////////////////
/// Functions for converting descriptive schema to Scheme lists
///////////////////////////////////////////////////////////////////////////////

#include <sstream>

void convert_schema_node_to_scheme_list(schema_node *node, std::ostream& sstr)
{
    sstr << "(";
    switch (node->type)
    {
        case element		: sstr << "elem";
                              break;
        case text			: sstr << "text";
                              break;
        case attribute		: sstr << "attr";
                              break;
        case document		: sstr << "doc";
                              break;
        case xml_namespace	: sstr << "ns";
                              break;
        case comment		: sstr << "comm";
                              break;
        case pr_ins			: sstr << "pr-ins";
                              break;
        default				: throw USER_EXCEPTION2(SE1051, "Unexpected node type in convert_schema_node_to_scheme_list");
    }

    if (node->name)
    {
        sstr << " (\"";
        if (node->xmlns && node->xmlns->uri) sstr << node->xmlns->uri;
        sstr << "\" \"" << node->name << "\")";
    }
    sstr << " (" << int2string(node->nodecnt) << " " << int2string(node->blockcnt) << " " << int2string(node->extnids) << ")";

    for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
    {
        if (ref == node->first_child) sstr << " ";
        convert_schema_node_to_scheme_list(ref->snode, sstr);
    }
    sstr << ")";
}

char* descriptive_schema_to_scheme_list(const char* name, int is_collection)
{
    //d_printf3("descriptive_schema_to_scheme_list parameters: %s %d\n", name, is_collection);

    schema_node *root;

    if (is_collection) root = find_collection(name);
    else root = find_document(name);

    if (!root) 
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

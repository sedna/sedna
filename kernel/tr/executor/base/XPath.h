/*
 * File:  XPath.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPATH_H
#define _XPATH_H

#include <string>
#include <vector>
#include <iostream>

#include "common/sedna.h"

#include "tr/executor/base/xsd.h"
#include "tr/executor/por2qep/scheme_tree.h"

enum Axis     
{ 
    axis_child,
    axis_descendant,
    axis_attribute,
    axis_self,
    axis_descendant_or_self,
    axis_descendant_attr,
    axis_parent
};

enum NodeTestType  
{ 
    node_test_processing_instruction, // processing-instruction()
    node_test_comment,                // comment()
    node_test_text,                   // text()
    node_test_node,                   // node()
    node_test_string,                 // fn:string()
    node_test_qname,                  // QName
    node_test_wildcard_star,          // *
    node_test_wildcard_ncname_star,   // NCName:*
    node_test_wildcard_star_ncname,   // *:NCName
    node_test_function_call,          // func(?)
    node_test_var_name                // $x
};

struct PPOpIn;

struct NodeTestData
{
    char* uri;
    char* ncname_prefix;
    char* ncname_local;
    PPOpIn *ppnode;
};
                     
struct NodeTest
{
    Axis axis;
    NodeTestType type;
    NodeTestData data;

    void print(std::ostream& str);
    void print_to_lr(std::ostream& str);
};


struct NodeTestOr
{
    NodeTest *nt;  // NodeTest array
    int s;         // size

    void print(std::ostream& str);
    void print_to_lr(std::ostream& str);
};


typedef std::vector<int> PathExprDistr;

struct PathExpr
{
    NodeTestOr *nto;  // NodeTestOr array
    int s;            // size

    void print(std::ostream& str = std::cerr);
    void print_to_lr(std::ostream& str = std::cerr);
};

typedef PathExpr RelPath;


void *create_PathExpr(const PathExprDistr &distr, bool persistent);
void delete_PathExpr(PathExpr *expr);

class dynamic_context;
void PathExpr2lr(PathExpr *path, std::ostream& str);
PathExpr *lr2PathExpr(dynamic_context *cxt, scheme_list *path_lst, bool persistent);
PathExpr *lr2PathExpr(dynamic_context *cxt, const char *str, bool persistent);

struct schema_node;
PathExpr *build_PathExpr(schema_node *from, schema_node *to);


void *PathExpr_mem_alloc(size_t size);
void *PathExpr_pers_alloc(size_t size);
inline void *PathExpr_malloc(size_t size, bool persistent)
{
    return (persistent ? PathExpr_pers_alloc(size) : PathExpr_mem_alloc(size));
}
typedef void* (*malloc_func)(size_t);
inline malloc_func PathExpr_malloc_func(bool persistent)
{
    return (persistent ? PathExpr_pers_alloc : PathExpr_mem_alloc);
}

// free dynamic memory allocated for local PathExprs
void PathExpr_local_free();
// free allocated persistent heap memory (call it on transaction rollback)
void PathExpr_pers_free();
// reset structures that stores allocated persistent heap memory (call it on transaction commit)
void PathExpr_reset_pers();


void set_node_test_type_and_data(scheme_list *lst, 
                                 NodeTestType &nt_type, //out parameter
                                 NodeTestData &nt_data, //out parameter
                                 bool persistent);


#endif




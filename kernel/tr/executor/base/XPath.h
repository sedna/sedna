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
#include "tr/structures/schema.h"

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
    /* KindTest */
    node_test_processing_instruction, // processing-instruction(name)
    node_test_comment,                // comment()
    node_test_text,                   // text()
    node_test_node,                   // node()
    node_test_element,                // element(name | *)
    node_test_attribute,              // attribute(name | *)
    node_test_document,               // document-node(element(* | name))
    
    /* NameTest */
    node_test_qname,                  // QName
    node_test_wildcard_star,          // *
    node_test_wildcard_ncname_star,   // NCName:*
    node_test_wildcard_star_ncname,   // *:NCName
};

struct NodeTestData
{
    char* uri;
    char* ncname_prefix;
    char* ncname_local;
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

struct PathExprMemoryManager {
    void * (*alloc)(size_t);
    void (*free)(void*);
    void (*free_all)();
    void * (*realloc)(void*, size_t);
};

extern PathExprMemoryManager * pe_local_aspace;
extern PathExprMemoryManager * pe_catalog_aspace;

void *create_PathExpr(const PathExprDistr &distr, PathExprMemoryManager * mm);
//void delete_PathExpr(PathExpr *expr);

class dynamic_context;
void PathExpr2lr(PathExpr *path, std::ostream& str);
PathExpr *lr2PathExpr(dynamic_context *cxt, scheme_list *path_lst, PathExprMemoryManager * mm);
PathExpr *lr2PathExpr(dynamic_context *cxt, const char *str, PathExprMemoryManager * mm);

PathExpr *build_PathExpr(schema_node_cptr from, schema_node_cptr to);

void set_node_test_type_and_data(scheme_list *lst,
                                 NodeTestType &nt_type, //out parameter
                                 NodeTestData &nt_data, //out parameter
                                 PathExprMemoryManager * mm);

#endif




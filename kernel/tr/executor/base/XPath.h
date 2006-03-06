/*
 * File:  XPath.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPATH_H
#define _XPATH_H

#include <string>
#include <vector>
#include <iostream>
#include "scheme_tree.h"

/*******************************************************************************
The following queries are still possible:
doc/a/node()/(.|b)
*******************************************************************************/
 
///
/// NCName
///
struct NCName
{
    char *n;  // name

    void print(std::ostream& str);
    void print_to_lr(std::ostream& str);
};


inline bool operator ==(const NCName &n1, const NCName &n2)
{
    return strcmp(n1.n, n2.n) == 0;
}

inline bool operator <(const NCName &n1, const NCName &n2)
{
    return strcmp(n1.n, n2.n) < 0;
}


///
/// QName - qualified name from Namespaces in XML standard
///
struct QName
{
    NCName Prefix;
    NCName LocalPart;

    void print(std::ostream& str);
    void print_to_lr(std::ostream& str);
};


inline bool operator < (const QName & n1, const QName & n2)
{
    if (n1.Prefix == n2.Prefix) return (n1.LocalPart < n2.LocalPart);
    else return (n1.Prefix < n2.Prefix);
}


enum Axis     { axis_child,
                axis_descendant,
                axis_attribute,
                axis_self,
                axis_descendant_or_self,
                axis_descendant_attr,
                axis_parent
              };

enum NodeTestType  { node_test_processing_instruction,	// processing-instruction()
                     node_test_comment,					// comment()
                     node_test_text,					// text()
                     node_test_node,					// node()
                     node_test_string,					// fn:string()
                     node_test_qname,					// QName
                     node_test_wildcard_star,			// *
                     node_test_wildcard_ncname_star,	// NCName:*
                     node_test_wildcard_star_ncname,	// *:NCName
                     node_test_function_call,			// func(?)
                     node_test_var_name					// $x
                   };

class PPOpIn;

struct NodeTestData
{
    NCName ncname;
    QName qname;
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
void set_NCName(NCName *n, const char* value, bool persistent);

struct variable_context;
void PathExpr2lr(PathExpr *path, std::ostream& str);
PathExpr *lr2PathExpr(variable_context *cxt, scheme_list *path_lst, bool persistent);
PathExpr *lr2PathExpr(variable_context *cxt, const char *str, bool persistent);

struct schema_node;
PathExpr *build_PathExpr(schema_node *from, schema_node *to);


// free dynamic memory allocated for local PathExprs
void PathExpr_local_free();
// free allocated persistent heap memory (call it on transaction rollback)
void PathExpr_pers_free();
// reset structures that stores allocated persistent heap memory (call it on transaction commit)
void PathExpr_reset_pers();



#endif




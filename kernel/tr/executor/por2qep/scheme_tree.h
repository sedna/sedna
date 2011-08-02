/*
 * File:  scheme_tree.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SCHEME_TREE_H
#define SCHEME_TREE_H

#include <vector>
#include <string>

#include "common/sedna.h"

enum scheme_type { SCM_BOOL, 
                   SCM_SYMBOL, 
                   SCM_CHAR, 
                   SCM_NUMBER, 
                   SCM_STRING, 
                   SCM_LIST };

struct scm_elem;

typedef std::vector<struct scm_elem> scheme_list;

union scm_elem_internal
{
    bool b;                /* SCM_BOOL   */
    char *symb;            /* SCM_SYMBOL */
    char ch;               /* SCM_CHAR   */
    char *num;             /* SCM_NUMBER */
    char *str;             /* SCM_STRING */
    scheme_list *list;     /* SCM_LIST   */
};

struct scm_elem
{
    scheme_type type;
    scm_elem_internal internal;
};

scheme_list *make_tree_from_scheme_list(const char *);

void delete_scheme_list(scheme_list *);

void walk_scheme_list(scheme_list *, std::string);

class AutoSchemeList {
  private:
    scheme_list * list;
  public:
    const scheme_list * get() const { return list; };

    AutoSchemeList(const char * str) : list(NULL) {
        list = make_tree_from_scheme_list(str);
    };

    ~AutoSchemeList() {
        if (list != NULL) {
            delete_scheme_list(list);
        }
    }
};

#endif


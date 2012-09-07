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

#define CL_CHECK_SYMBOL(X, Pos, Symb) (((X)->at(Pos).type == SCM_SYMBOL) && strcmpex((X)->at(Pos).internal.symb, (Symb)) == 0)
#define CL_TYPE(X, Pos, Tp) ((X)->at(Pos).type == Tp)

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

inline static
scheme_list * scmGetList(const scheme_list * list, unsigned index, const char * error) {
    if ((list->size() <= index) || (list->at(index).type != SCM_LIST)) {
        throw USER_EXCEPTION2(SE1004, error);
    };

    return (list->at(index).internal.list);
};

inline static
const char * scmGetSymbol(const scheme_list * list, unsigned index, const char * error) {
    if ((list->size() <= index) || (list->at(index).type != SCM_SYMBOL)) {
        throw USER_EXCEPTION2(SE1004, error);
    };

    return (list->at(index).internal.symb);
};

inline static
const char * scmGetString(const scheme_list * list, unsigned index, const char * error) {
    if ((list->size() <= index) || (list->at(index).type != SCM_STRING)) {
        throw USER_EXCEPTION2(SE1004, error);
    };

    return (list->at(index).internal.str);
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


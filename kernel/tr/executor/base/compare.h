/*
 * File:  compare.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _COMPARE_H
#define _COMPARE_H

#include <list>
#include "tuple.h"


typedef int (*tuple_cell_compare_fun)(const tuple_cell&, const tuple_cell&);

struct order_spec
{
    tuple_cell_compare_fun f;
    bool ascending;
    bool empty_greatest;
};

typedef std::list<order_spec> order_spec_list;


int tuple_compare(const order_spec_list& osl, int cells_number, tuple_cell *cells1, tuple_cell *cells2);

#endif

/*
 * File:  cpp_popt.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <strstream>

#include "common/sedna.h"

#include "tr/sqp/popt/att_xpath.h"
#include "tr/sqp/popt/serialize2lr.h"
#include "common/errdbg/d_printf.h"

const char *cpp_popt(const char * str)
{
    scheme_list *att_in_scheme_lst = NULL;
    att_attr *att = NULL;
    popt_plan plan;
    std::ostrstream s;

    //d_printf1("\n%s\n\n", str);

    att_in_scheme_lst = make_tree_from_scheme_list(str);
    att = make_att_attr(att_in_scheme_lst);
    
    switch (att->type)
    {
       case att_xpath: 
           plan = popt_att_xpath(att->xpath);
           break;
       default: throw USER_EXCEPTION2(SE1051, "Unexpected type of att attribute");
    }

    serialize2lr_popt_plan(s, plan);

    delete_scheme_list(att_in_scheme_lst);
    delete_att_attr(att);


    int size = s.pcount();
    // Andrey Fomichev: I use malloc here because this piece of memory will be
    // released by Chicken (by C-call free())
    char *res = (char*)malloc(size + 1);
    memcpy(res, s.str(), size);
    res[size] = '\0';

#ifdef POPT_DEBUG
    d_printf1("\n========== Optimized plan ==========\n");
    d_printf2("%s", res);
    d_printf1("\n========== Optimized plan ==========\n");
#endif

    return res;
}


extern "C" {

const char *c_popt(const char * str)
{
    return cpp_popt(str);
}

}


/*
 * File:  crminternals.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CRMINTERNALS_H
#define _CRMINTERNALS_H

/* This file should contain functions and data structures
   which are used within crmutils only. */

#include "common/sedna.h"
#include "tr/crmutils/crmbase.h"
  

void print_text(xptr text,              /* pointer to the text to print */
                se_ostream& crmout,     /* output strem to print into */
                t_print ptype,          /* xml, sxml, etc ... */
                t_item xq_type);        /* node type (text, element, etc...)*/



#endif /* _CRMINTERNALS_H */

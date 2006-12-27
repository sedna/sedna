/*
 * File:  string_operations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _STRING_OPERATIONS_H
#define _STRING_OPERATIONS_H

#include "sedna.h"
#include "tuple.h"
#include "strings.h"


// do_not_check_type should be set to true only for XQuery B.2 Operator Mapping implementation
int fn_compare(const tuple_cell &a1, 
               const tuple_cell &a2, 
               CollationHandler* handler /*= NULL*/);


#endif

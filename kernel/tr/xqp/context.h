/*
 * File:  context.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CONTEXT_H
#define _CONTEXT_H

#include "common/sedna.h"

/* in_context class is responsible for context, which is passed to the grammar
   rules. A in_context class consists of the status field, which indicates
   either recognizing expr located in the predicate or not.
*/
class in_context
{
public:
  in_context();
};

class out_context
{
public:
  out_context();
};

#endif







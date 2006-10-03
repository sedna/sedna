/*
 * File:  XmlNames.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _XMLNAMES_H
#define _XMLNAMES_H

#include "sedna.h"
#include "PPBase.h"

bool chech_constraints_for_xs_NMTOKEN(const tuple_cell *tc);
bool chech_constraints_for_xs_Name(const tuple_cell *tc);
bool chech_constraints_for_xs_NCName(const tuple_cell *tc);

#endif

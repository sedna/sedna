/*
 * File:  xs_names.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _XS_NAMES_H
#define _XS_NAMES_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
/// There are six types which are more or less based on Name 
/// production defined in XML specification:
/// xs:NMTOKEN - match the Nmtoken production.
/// xs:Name    - match the Name production.
/// xs:NCName, xs:ID, xs:IDREF, xs:ENTITY - 
///              match the NCName production.
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

/// Note! Following functions allow values to be non-normalized,
///       i.e. to be with trailing and leading whitespaces.
///       Moreover, in most cases you need to perform normalization
///       after checking.

/// Note! Both tuple_cell and char* versions of these functions
///       must be used only with CORRECT UTF-8 strings.

////////////////////////////////////////////////////////////////
/// Check constraints on tuple_cell
////////////////////////////////////////////////////////////////
bool chech_constraints_for_xs_NMTOKEN (const tuple_cell *tc);
bool chech_constraints_for_xs_Name    (const tuple_cell *tc);
bool chech_constraints_for_xs_NCName  (const tuple_cell *tc);

////////////////////////////////////////////////////////////////
/// Check constraints on char*
/// -- char* must point to zero ended C-string contained
///    possibly non-normalized value.
////////////////////////////////////////////////////////////////
bool        chech_constraints_for_xs_NMTOKEN (const char* s);
bool        chech_constraints_for_xs_Name    (const char* s);
bool        chech_constraints_for_xs_NCName  (const char* s, int n);
inline bool chech_constraints_for_xs_NCName  (const char* s)
{
    return chech_constraints_for_xs_NCName(s, strlen(s));
}



#endif

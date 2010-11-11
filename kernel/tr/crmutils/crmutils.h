/*
 * File:  crmutils.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CRMUTILS_H
#define _CRMUTILS_H

#include "common/sedna.h"
#include "common/xptr.h"
#include "tr/crmutils/exec_output.h"
#include "tr/crmutils/crmbase.h"
#include "tr/crmutils/serialization.h"
#include "tr/cat/catptr.h"
#include "tr/executor/base/tuple.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif

class dynamic_context;

/* predefined debug&error output stream */
extern se_stdlib_ostream crm_dbg;

////////////////////////////////////////////////////////////////////////////////
/// Print utils
////////////////////////////////////////////////////////////////////////////////

typedef enum se_output_method t_print;

/* Print physical operations stack in debug mode */
void print_pp_stack(se_ostream* dostr);

////////////////////////////////////////////////////////////////////////////////
/// Debug utils
////////////////////////////////////////////////////////////////////////////////

void printSimpleDebugInfo(schema_node_cptr snode, se_ostream& crmout);
void getDebugInfo(schema_node_cptr snode, xptr& node);

#endif /* _CRMUTILS_H */


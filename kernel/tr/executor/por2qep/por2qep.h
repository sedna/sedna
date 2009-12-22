/*
 * File:  por2qep.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _POR2QEP_H
#define _POR2QEP_H

#include "common/sedna.h"
#include "tr/executor/base/PPOperations.h"
#include "tr/executor/por2qep/scheme_tree.h"

struct qep_subtree
{
    dynamic_context *cxt;
    PPOpIn tree;
};

PPQueryEssence *build_qep(const char*  por, bool is_ast);
qep_subtree    *build_subqep(const char*  por, bool is_ast);

void delete_qep(PPQueryEssence *qep);
void delete_qep_unmanaged(PPQueryEssence *qep);
void delete_qep(qep_subtree *qep);

db_entity *make_db_entity(scheme_list *ent_lst, bool explicit_name = true);
tuple_cell make_const(const scm_elem& const_type, const scm_elem& const_value);
xmlscm_type lr_atomic_type2xmlscm_type(const char *type);


#endif


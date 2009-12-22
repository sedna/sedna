/*
 * File:  auc.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __AUC_H
#define __AUC_H

#include <map>

#include "common/sedna.h"

#include "tr/structures/system_tables.h"
#include "tr/executor/base/xptr_sequence.h"


#define INSERT_STATEMENT                      1
#define DELETE_STATEMENT                      2
#define RENAME_STATEMENT                      4
#define REPLACE_STATEMENT                     8


#define BLOCK_AUTH_CHECK                     -1
#define DEPLOY_AUTH_CHECK                     1

struct dbe_properties
{
    int update_privileges;      // user's update privileges on this db_entity
    bool current_statement;     // is db_entity refered in curent statement
};

void getSednaAuthMetadataPath(char* path);

void auth_for_query(counted_ptr<db_entity> dbe);

void auth_for_load_module(const char* module_name);

void auth_for_drop_module(const char* mod_name);

void auth_for_create_document(const char* doc_name);

void auth_for_load_document(const char* doc_name);

void auth_for_create_collection(const char* coll_name);

void auth_for_create_document_collection(const char* doc_name, const char *coll_name);

void auth_for_load_document_collection(const char* doc_name, const char *coll_name);

void auth_for_create_index(const char* ind_name, const char *obj_name, bool is_collection);

void auth_for_create_trigger(const char *trg_name);

void auth_for_drop_object(const char* obj_name, const char *obj_type, bool just_check);

void auth_for_rename_collection(const char* old_name, const char* new_name);

void clear_authmap();

void clear_current_statement_authmap();

bool is_auth_check_needed(int update_privilege);

/* Parameters:
 * seq               - sequence of nodes subject to update;
 * update_privilege  - set of update privileges;
 * direct            - direct/indirect pointers in seq.
 */
void auth_for_update(xptr_sequence* seq, int update_privilege, bool direct);

#endif /* __AUC_H */

/*
 * File:  auc.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AUC
#define _AUC

#include "common/sedna.h"
#include <map>
#include <set>
#include <iostream>

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xptr_sequence.h"


#define INSERT_STATEMENT                      1
#define DELETE_STATEMENT                      2
#define RENAME_STATEMENT                      4
#define REPLACE_STATEMENT                     8


#define BLOCK_AUTH_CHECK                     -1
struct dbe_properties { int update_privileges;     // this user's update privileges on this db_entity
					    bool current_statement;     // is db_entity refered in curent statement
					    bool was_updated;
					  };
					   
typedef std::map<counted_ptr<db_entity>, struct dbe_properties> auth_map;

void getSednaAuthMetadataPath(char* path);

void auth_for_query(counted_ptr<db_entity> dbe);

void auth_for_load_module(const char* module_name);

void clear_authmap();

void clear_current_statement_authmap();

bool is_auth_check_needed(int update_privilege);

//param: 
// seq - sequence of nodes subject to update;
// update_privilege - set of update privileges;
// direct - direct/indirect pointers in seq
void auth_for_update(xptr_sequence* seq, int update_privilege, bool direct);
#endif

/*
 * File:  ipc_ops.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _IPC_OPS_H
#define _IPC_OPS_H

#include "common/sedna.h"

#include "common/u/ushm.h"
#include "common/config.h"

void
open_gov_shm  ();

int
close_gov_shm ();

void
send_command_to_gov(int port_number, char gov_address[], int cmd);

int
get_db_id_by_name(gov_config_struct* cfg, const char* db_name);

int
get_next_free_db_id(gov_config_struct* cfg);

void
erase_database_cell_in_gov_shm(int db_id, gov_config_struct* cfg);

bool
is_database_running(gov_config_struct* cfg, int database_id);

void
fill_database_cell_in_gov_shm(gov_config_struct* cfg,
                              int db_id,
                              const char* db_name,
                              int bufs_num,
                              double upd_crt,
                              int max_log_files,
                              int tmp_file_initial_size);

void
get_sednaconf_values(gov_header_struct* cfg);


/* Typed pointers to the sedna_gov_shm_ptr */
#define GOV_HEADER_GLOBAL_PTR    ( GOV_HEADER_STRUCT_PTR(sedna_gov_shm_ptr) )
#define GOV_CONFIG_GLOBAL_PTR    ( GOV_CONFIG_STRUCT_PTR(sedna_gov_shm_ptr) )

#endif /* _IPC_OPS_H_ */


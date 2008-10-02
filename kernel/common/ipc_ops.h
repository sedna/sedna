/*
 * File:  ipc_ops.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _IPC_OPS_H
#define _IPC_OPS_H

#include "common/sedna.h"

#include "common/u/ushm.h"
#include "common/config.h"

void*  open_gov_shm(UShMem *gov_shm_service_dsc);
int    close_gov_shm(UShMem gov_shm_service_dsc, void* gov_shared_mem);

void   send_command_to_gov(int port_number, int cmd);
int    get_db_id_by_name(gov_config_struct* cfg, const char* db_name);
int    get_next_free_db_id(gov_config_struct* cfg);
void   erase_database_cell_in_gov_shm(int db_id, gov_config_struct* cfg);
void   fill_database_cell_in_gov_shm(gov_config_struct* cfg,
                                     int db_id,
                                     const char* db_name, 
                                     int bufs_num,
                                     int max_trs_num,
                                     int logical_log_file_size,
                                     double upd_crt);

void   get_sednaconf_values(gov_header_struct* cfg);

#endif /* _IPC_OPS_H_ */


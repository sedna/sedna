/*
 * File:  ipc_ops.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _IPC_H
#define _IPC_H

#include <string>

#include "common/u/ushm.h"
#include "common/u/uutils.h"
#include "common/u/upipe.h"
#include "common/u/ushm.h"
#include "common/config.h"


// return 0 indicates success
// negative return values indicates an error
extern std::string elem_content;

int WriteHead(UPIPE p, int *cmd, int *len);

// return 0 indicates success
// negative return values indicates an error
int ReadHead(UPIPE p, int *cmd, int *len);

void send_command_to_gov(int port_number, int cmd);

void* open_gov_shm(UShMem *gov_shm_service_dsc);

int close_gov_shm(UShMem gov_shm_service_dsc, void* gov_shared_mem);

void get_gov_config_parameters_from_sednaconf(gov_header_struct* cfg);

int get_db_id_by_name(gov_config_struct* cfg, const char* db_name);

int get_next_free_db_id(gov_config_struct* cfg);

void erase_database_cell_in_gov_shm(int db_id, gov_config_struct* cfg);

void fill_database_cell_in_gov_shm(gov_config_struct* cfg,
                                   int db_id,
                                   const char* db_name, 
                                   int bufs_num,
                                   int max_trs_num,
                                   int logical_log_file_size,
                                   double upd_crt);

std::string erase_ws(std::string str);

void get_default_sednaconf_values(gov_header_struct* cfg);

#endif


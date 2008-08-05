/*
 * File:  tr_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TR_GLOBALS_H
#define _TR_GLOBALS_H

#include "common/sedna.h"
#include "common/base.h"
#include "tr/client_core.h"
#include "common/argtable.h"
#include "common/persistent_db_data.h"
#include "tr/auth/auc.h"
#include "common/pping.h"

#define TR_ARG_MAX_LENGTH       511

extern void *gov_shm_pointer;

extern int tr_s_help;
extern int tr_l_help;
extern int tr_version;

extern int run_rewriter;
extern int run_popt;
extern int show_time;
extern int socket_port;
extern int print_intermed;
extern int server_mode;
extern int debug_mode;
extern int write_phys_log;
extern int user_id;
extern int internal_auth_switch;
extern int first_transaction;
extern int authentication;
extern int authorization;
extern int query_timeout;

extern char db_name[];
extern char filename[];
extern char q_type[];
extern QueryType query_type;
extern char login[];
extern char password[];
extern char output_file[];

extern const size_t narg;
extern arg_rec tr_argtable[];

extern client_core* client;
extern transaction_id trid;
extern session_id sid;

extern bool is_init_gov_shm;

extern UShMem gov_shm_dsc;
extern void* gov_shared_mem;

extern persistent_db_data* entry_point;

extern msg_struct sp_msg;
extern bool is_need_checkpoint_on_transaction_commit;

extern bool is_ro_mode; // can change during transaction execution!!!
extern bool need_ph_reinit;
extern bool is_ft_disabled;

extern int db_id;

namespace tr_globals 
{
extern pping_client *ppc;
}
#endif

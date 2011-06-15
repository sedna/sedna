/*
 * File:  config.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CONFIG_H_
#define _CONFIG_H_

#include "common/u/ushm.h"
#include "common/base.h"
#include "common/u/uprocess.h"

/*
 * Defines possible states in which each database can be.
 * SM_OM_WORKING      - SM is working and any transaction is able to connect
 * OM_SM_SHUTDOWN     - SM is going to shutdown
 * OM_SM_SPECIAL_MODE - SM is working but only transactions in special mode are
                        able to connect
 * OM_SM_DOWN         - SM is not running
 */
enum sm_operation_mode {
    OM_SM_WORKING      = 0,
    OM_SM_SHUTDOWN     = 1,
    OM_SM_SPECIAL_MODE = 2,
    OM_SM_DOWN         = 3,
};

struct gov_sess_struct
{
    int idfree; //0->not used 1->session in progress 2->session finished
    int stop;   //1->stop command; 0->not stop
};

struct gov_db_struct
{
    char db_name[SE_MAX_DB_NAME_LENGTH + 1];
    sm_operation_mode mode;
    UPID sm_pid;

    int bufs_num;
    int max_trs_num;
    double upd_crt;
    int max_log_files;
    int tmp_file_initial_size;  /* size in PAGES */
};

struct gov_header_struct
{
    volatile int is_server_stop;      /// possible values are defined by 'enum stoptype'
                                      /// 1->indicates that sedna wants to stop
    UPID gov_pid;
    char SEDNA_DATA[SEDNA_DATA_VAR_SIZE];
    int  os_primitives_id_min_bound;

    char lstnr_addr[U_MAX_FNAME];     /// Governor listening address
    int  lstnr_port_number;           /// Governor listening port
    int  ping_port_number;            /// Process ping port
    int  el_level;                    /// Event log severity level
    int  ka_timeout;                  /// Session keep alive timeout

    /* Maximum depth of the physical operations stack in executor */
    int pp_stack_depth;
};

struct gov_config_struct
{
    gov_header_struct gov_vars;
    gov_db_struct db_vars[MAX_DBS_NUMBER];
    gov_sess_struct sess_vars[MAX_SESSIONS_NUMBER];
};


#define GOV_HEADER_STRUCT_PTR(ptr)    (& (((gov_config_struct*)ptr)->gov_vars) )
#define GOV_CONFIG_STRUCT_PTR(ptr)    ( (gov_config_struct*)ptr )


#endif /* _CONFIG_H_ */

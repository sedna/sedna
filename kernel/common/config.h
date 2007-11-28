#ifndef _CONFIG_H_
#define _CONFIG_H_

#include "common/u/ushm.h"
#include "common/base.h"
#include "common/config.h"

void* open_gov_shm(UShMem *gov_shm_service_dsc);
int close_gov_shm(UShMem gov_shm_service_dsc, void* gov_shared_mem);

struct gov_sess_struct
{
   int idfree; //0->not used 1->session in progress 2->session finished
   int stop; //1->stop command; 0->not stop
};


struct gov_db_struct
{
   char db_name[SE_MAX_DB_NAME_LENGTH + 1];
   int is_stop; //0->indicates that sm is working, 1->indicates that sm want to stop; -1 indicates that sm is down
   UPID sm_pid;

   int bufs_num;
   int max_trs_num;
   double upd_crt;
   int phys_log_ext_portion;
   int phys_log_size;
   int logical_log_file_size;//size of one logical log's file
};

struct gov_header_struct
{
   int is_server_stop;//0->indicates that sedna operates;//1->indicates that sedna want to stop
   UPID gov_pid;
   char SEDNA_DATA[SEDNA_DATA_VAR_SIZE];
   int os_primitives_id_min_bound;

   int lstnr_port_number;
   int ping_port_number;
};

struct gov_config_struct
{
  gov_header_struct gov_vars;
  gov_db_struct db_vars[MAX_DBS_NUMBER];
  gov_sess_struct sess_vars[MAX_SESSIONS_NUMBER];
};

/*
#define GOV_SHM_BEGIN ((char*)gov_shared_mem)
#define GOV_SHM_SIZE (sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct) + MAX_SESSIONS_NUMBER*sizeof(gov_sess_struct))
#define GOV_SHM_HEADER_BEGIN_PTR (GOV_SHM_BEGIN)
#define GOV_SHM_DBS_BEGIN_PTR  (GOV_SHM_BEGIN + sizeof(gov_header_struct))
#define GOV_SHM_SESS_BEGIN_PTR (GOV_SHM_BEGIN + sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct))
*/

#endif

#include "common/processes/command_lines.h"
#include "common/u/uprocess.h"
#include "common/u/uutils.h"
#include "common/config.h"
#include "common/ipc_ops.h"
#include "common/structures/sm_structures.h"
#include "common/structures/cdb_structures.h"
#include <string>

using namespace std;

/* This function constructs command line for sm launch. SMInfo overrides cfg. If sm is supposed to be started with default settings sminfo must be NULL. Sminfo must be initialized before this function 
 (at this moment it is initialized on cdb or sm start. */
               //       !TODO: fix upd-crt
               //       string(u_dtoa

string constructClForSM (gov_config_struct* cfg, SMInfo* sminfo, char * dbname) {
    string command_line;
    char buf[U_MAX_PATH + 10]; //!FIXME -- there should be some reasonable buf size
    
    U_ASSERT(cfg != NULL);
    U_ASSERT(dbname != NULL);
    int db_id = get_db_id_by_name(cfg, dbname);
    
    if (sminfo == NULL) {
      command_line = uGetImageProcPath(buf, __sys_call_error) + string("/") + SM_EXE
               + string(" -background-mode off ") 
               + string(" -db-id ") + int2string(db_id) 
               + string(" -sedna-data ") + string(cfg->gov_vars.SEDNA_DATA)
               + string(" -bufs-num ") + int2string(cfg->db_vars[db_id].bufs_num) 
               + string(" -upd-crt ") + string(" 0.25 ") 
               + string(" -tmp-file-init-size ") + int2string(cfg->db_vars[db_id].tmp_file_initial_size)
               + string(" -max-log-files ") + int2string(cfg->db_vars[db_id].max_log_files)
               + string(" -gov-address ") + string(cfg->gov_vars.lstnr_addr) 
               + string(" -port-number ") + int2string(cfg->gov_vars.lstnr_port_number)
               + string(" -max-stack-depth ") + int2string(cfg->gov_vars.pp_stack_depth)
               + string(" -min-bound ") + int2string(cfg->gov_vars.os_primitives_id_min_bound) + string(" ") 
               + string(dbname);

    } else {
      command_line = uGetImageProcPath(buf, __sys_call_error) + string("/") + SM_EXE
               + string(" -background-mode off ") 
               + string("-db-id ") + int2string(db_id) 
               + string(" -sedna-data \"") + string(cfg->gov_vars.SEDNA_DATA) + string("\"") 
               + string(" -bufs-num ") + int2string(sminfo->bufs_num) 
               + string(" -upd-crt ") + string(" 0.25 ") 
               + string(" -tmp-file-init-size ") + int2string(sminfo->tmp_file_initial_size)
               + string(" -max-log-files ") + int2string(sminfo->max_log_files) 
               + string(" -gov-address ") + string(cfg->gov_vars.lstnr_addr) 
               + string(" -port-number ") + int2string(cfg->gov_vars.lstnr_port_number)
               + string(" -max-stack-depth ") + int2string(cfg->gov_vars.pp_stack_depth)
               + string(" -min-bound ") + int2string(cfg->gov_vars.os_primitives_id_min_bound) + string(" ") 
               + string(dbname);
    }
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
    elog(EL_LOG, (command_line.c_str()));
#endif
    return command_line;
}

string constructClForCdb (gov_config_struct* cfg, CdbParameters* cdbinfo, int db_id) {
    string command_line;
    char buf[U_MAX_PATH + 10]; //!FIXME -- there should be some reasonable buf size
    
    U_ASSERT(cfg != NULL);
    
    command_line = uGetImageProcPath(buf, __sys_call_error) + string("/") + SM_EXE
            + string(" -background-mode off ") 
            + string("-db-id ") + int2string(db_id) 
            + string(" -sedna-data \"") + string(cfg->gov_vars.SEDNA_DATA) + string("\"") 
            + string(" -bufs-num ") + int2string(cdbinfo->bufs_num) 
            + string(" -upd-crt ") + string(" 0.25 ") 
            + string(" -tmp-file-init-size ") + int2string(cdbinfo->tmp_file_initial_size)
            + string(" -max-log-files ") + int2string(cdbinfo->max_log_files) 
            + string(" -gov-address ") + string(cfg->gov_vars.lstnr_addr) 
            + string(" -port-number ") + int2string(cfg->gov_vars.lstnr_port_number)
            + string(" -max-stack-depth ") + int2string(cfg->gov_vars.pp_stack_depth)
            + string(" -min-bound ") + int2string(cfg->gov_vars.os_primitives_id_min_bound) + string(" ") 
            + string(" -cdb ")
            + string(cdbinfo->db_name);

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
    elog(EL_LOG, (command_line.c_str()));
#endif
    return command_line;
}

string constructClForTrn (gov_header_struct * cfg, char* dbname, int db_id) {
  string command_line;
  char buf[U_MAX_PATH + 10]; //!FIXME -- there should be some reasonable buf size
    
  U_ASSERT(cfg != NULL);
  U_ASSERT(dbname != NULL);
  
  command_line = uGetImageProcPath(buf, __sys_call_error) + string("/") + SESSION_EXE
               + string(" -db-id ") + int2string(db_id)
               + string(" -sedna-data ") + string(cfg->SEDNA_DATA)
               + string(" -gov-address ") + string(cfg->lstnr_addr) 
               + string(" -port ") + int2string(cfg->lstnr_port_number) 
               + string(" -max-stack-depth ") + int2string(cfg->pp_stack_depth)
               + string(" -min-bound ") + int2string(cfg->os_primitives_id_min_bound) + string(" ") 
               + string(dbname);
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
//   elog(EL_LOG, (command_line.c_str()));
#endif
  return command_line;
}

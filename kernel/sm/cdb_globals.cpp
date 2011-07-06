/*
 * File:  cdb_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "common/base.h"
#include "common/config.h"
#include "sm/cdb_globals.h"
#include "common/utils.h"
#include "common/u/uprocess.h"
#include "common/u/uhdd.h"
#include "common/u/uutils.h"
#include "common/version.h"
#include "common/xptr.h"

using namespace std;
using namespace cdb_globals;


namespace cdb_globals {
    int data_file_max_size          = 0;     /* 0 means that size is not limited */
    int tmp_file_max_size           = 0;     /* 0 means that size is not limited */
    int data_file_extending_portion;         /* size in Mb */
    int tmp_file_extending_portion;          /* size in Mb */
    int data_file_initial_size;              /* size in Mb */
    int log_file_size;                       /* size in Mb */
    int layer_size;                          /* size of database layer -- Mb */
    char db_security[32];                    /* either 'authorization', 'authentication' or 'off' */
}

static int cdb_s_help = 0;
static int cdb_l_help = 0;
static int cdb_version = 0;

static const size_t cdb_narg = 16;

static arg_rec cdb_argtable[] =
{
  {"--help",                   NULL,   arg_lit, &cdb_l_help,                       "0",   "\t\t\tdisplay this help and exit"},
  {"-help",                    NULL,   arg_lit, &cdb_s_help,                       "0",   "\t\t\t\tdisplay this help and exit"},
  {"-version",                 NULL,   arg_lit, &cdb_version,                      "0",   "\t\t\tdisplay product version and exit"},
  {"-data-file-max-size",      " Mbs", arg_int, &data_file_max_size,               "0",   "\tthe max size of data file (in Mb),\n\t\t\t\tdefault 0 (unlimited size)"},
  {"-tmp-file-max-size",       " Mbs", arg_int, &tmp_file_max_size,                "0",   "\tthe max size of tmp file (in Mb),\n\t\t\t\tdefault 0 (unlimited size)"},
  {"-data-file-ext-portion",   " Mbs", arg_int, &data_file_extending_portion,      "100", "\tthe data file extending portion size (in Mb), \n\t\t\t\tdefault 100Mb"},
  {"-tmp-file-ext-portion",    " Mbs", arg_int, &tmp_file_extending_portion,       "100", "\tthe tmp file extending portion size (in Mb),\n\t\t\t\tdefault 100Mb"},
  {"-data-file-init-size",     " Mbs", arg_int, &data_file_initial_size,           "100", "\tthe data file initial size (in Mb),\n\t\t\t\tdefault 100Mb"},
  {"-tmp-file-init-size",      " Mbs", arg_int, &sm_globals::tmp_file_initial_size,"100", "\tthe tmp file initial size (in Mb),\n\t\t\t\tdefault 100Mb"},
  {"-bufs-num",                " N",   arg_int, &sm_globals::bufs_num,             "1600","\t\t\tthe number of buffers in main memory,\n\t\t\t\tdefault 1600 (the size of the buffer is 64Kb)"},
  {"-upd-crt",                 " N",   arg_dbl, &sm_globals::upd_crt,              "0.25","\t\t\tupdate criterion parameter \n\t\t\t\t(fraction of database), default 0.25"},
  {"-max-log-files",           " N",   arg_int, &sm_globals::max_log_files,        "3",   "\t\tmaximum log files until log truncate\n\t\t\t\tdefault: 3"},
  {"-log-file-size",           " Mbs", arg_int, &log_file_size,                    "100", "\t\tmaximum one log file size (in Mb)\n\t\t\t\tdefault 100Mb"},
  {"-layer-size",              " Mbs", arg_int, &layer_size,                       "0",   "\t\tmaximum size of the database layer (in Mb)\n\t\t\t\tdefault 1Gb"},
  {"-db-security",             "  security level",  arg_str,  &db_security,         "authentication", "  level of database security:\n\t\t\t\t 1) 'off' - none;\n\t\t\t\t 2) 'authentication' (default);\n\t\t\t\t 3) 'authorization'"},
  {NULL,                       "\n   db_name", arg_str, &sm_globals::db_name,      "???", "   \t\t\tname of the database to be created"}
};


static void
print_cdb_usage(int ret_code)
{
    fprintf(stdout, "Usage: se_cdb [options] dbname\n\n");
    fprintf(stdout, "options:\n%s\n", arg_glossary(cdb_argtable, cdb_narg, "  "));
    exit(ret_code);
}

void
parse_cdb_command_line(int argc, char** argv)
{
   char errmsg[1000];

   if (argc == 1)
       print_cdb_usage(1);

   int res = arg_scanargv(argc, argv, cdb_argtable, cdb_narg, NULL, errmsg, NULL);

   if (cdb_s_help == 1 || cdb_l_help == 1)
       print_cdb_usage(0);

   if (cdb_version == 1) {
       print_version_and_copyright("Sedna Create Data Base Utility");
       exit(0);
   }
   if (0 == res)
       throw USER_EXCEPTION2(SE4601, errmsg);
}




void
setup_cdb_globals(gov_config_struct* cfg)
{
   if (log_file_size <= 0)
	   throw USER_EXCEPTION2(SE4601, "'log_file_size' parameter is incorrect (must be >0)");
   if (sm_globals::upd_crt < 0 || sm_globals::upd_crt > 1)
	   throw USER_EXCEPTION2(SE4601, "'upd-crt' parameter is incorrect (must be in [0;1])");
   if (sm_globals::max_log_files < 1)
	   throw USER_EXCEPTION2(SE4601, "'max-log-files' parameter is incorrect (must be >= 1)");
   if (sm_globals::bufs_num < 1)
	   throw USER_EXCEPTION2(SE4601, "'bufs-num' parameter is incorrect (must be >= 1)");
   if (sm_globals::max_log_files < 1)
	   throw USER_EXCEPTION2(SE4601, "'max-log-files' parameter is incorrect (must be >= 1)");
   if (cdb_globals::layer_size != 0 && (cdb_globals::layer_size < 0 || (lsize_t)cdb_globals::layer_size * 1024 * 1024 < VMM_REGION_MIN_SIZE))
       throw USER_EXCEPTION2(SE4601, "'layer_size' parameter is incorrect (must be >=64Mb)");

   if (strcmp(db_security, "???") == 0)
       strcpy(db_security, "authentication");

   if ((strcmp(db_security, "off") != 0) && (strcmp(db_security, "authentication") != 0) && (strcmp(db_security, "authorization") != 0))
       throw USER_EXCEPTION2(SE4601, "'db-security' parameter is incorrect");

   if (strcmp(sm_globals::db_name, "???") == 0)
      throw USER_EXCEPTION2(SE4601, "The name of the database must be specified");

   if(data_file_initial_size < 1)
	   throw USER_EXCEPTION2(SE4601, "'data_file_init_size' parameter is incorrect (must be >= 1)");
   if(sm_globals::tmp_file_initial_size < 1)
	   throw USER_EXCEPTION2(SE4601, "'tmp_file_init_size' parameter is incorrect (must be >= 1)");
   if(data_file_max_size != 0 && (data_file_max_size < 0 || data_file_max_size < data_file_initial_size))
	   throw USER_EXCEPTION2(SE4601, "'data_file_max_size' parameter is incorrect (must be >= 1 and < data_file_initial_size, or 0 for infinite size)");
   if(tmp_file_max_size != 0 && (tmp_file_max_size < 1 || tmp_file_max_size < sm_globals::tmp_file_initial_size))
	   throw USER_EXCEPTION2(SE4601, "'tmp_file_max_size' parameter is incorrect (must be >= 1 and < tmp_file_initial_size, or 0 for infinite size)");

   if(data_file_extending_portion < 1 || data_file_extending_portion > (data_file_max_size == 0 ? INT32_MAX : data_file_max_size ))
	   throw USER_EXCEPTION2(SE4601, "'data_file_ext_portion' parameter is incorrect (must be >= 1 and <= data_file_max_size)");
   if(tmp_file_extending_portion < 1 || tmp_file_extending_portion > (tmp_file_max_size == 0 ? INT32_MAX : tmp_file_max_size ))
	   throw USER_EXCEPTION2(SE4601, "'tmp_file_ext_portion' parameter is incorrect (must be >= 1 and <= tmp_file_max_size)");

   check_db_name_validness(sm_globals::db_name);

   if (strlen(cfg->gov_vars.SEDNA_DATA) + strlen(sm_globals::db_name) + 14 > U_MAX_PATH)
       throw USER_EXCEPTION2(SE1009, "Path to database files is too long");

   strcpy(sm_globals::db_files_path, cfg->gov_vars.SEDNA_DATA);
   strcat(sm_globals::db_files_path, "/data/");
   strcat(sm_globals::db_files_path, sm_globals::db_name);
   strcat(sm_globals::db_files_path, "_files/");
}


static inline string
replaceAll(string context, const char* src, const char* dst) {
    size_t lookHere = 0;
    size_t foundHere;
    const string from(src);
    const string to(dst);
    while((foundHere = context.find(from, lookHere)) != string::npos) {
        context.replace(foundHere, from.size(), to);
        lookHere = foundHere + to.size();
    }
    return context;
}

void create_cfg_file()
{
   char buf[100];
   unsigned int nbytes_written = 0;
   string cfg_file_content;
   UFile cfg_file_handle;
   USECURITY_ATTRIBUTES *def_sa, *dir_sa;

   string cfg_files_path = string(SEDNA_DATA)+"/cfg";
   string cfg_file_name  = cfg_files_path + "/" + string(sm_globals::db_name) + "_cfg.xml";

   if(uCreateSA(&dir_sa, U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);
   if (uMkDir(cfg_files_path.c_str(), dir_sa, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4300, cfg_files_path.c_str());

   if(uCreateSA(&def_sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);
   cfg_file_handle = uCreateFile(cfg_file_name.c_str(),
  	                            0,
  	                            U_READ_WRITE,
  	                            U_WRITE_THROUGH,
  	                            def_sa, __sys_call_error);
   if (cfg_file_handle == U_INVALID_FD)
      throw USER_EXCEPTION2(SE4040, "database configuration file");

   uReleaseSA(def_sa, __sys_call_error);
   uReleaseSA(dir_sa, __sys_call_error);

   string db_name_str = replaceAll(string(sm_globals::db_name), "&", "&amp;");

   cfg_file_content =  "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
   cfg_file_content += "<db>\n";
   cfg_file_content += "   <name>" + db_name_str + string("</name>\n");
   cfg_file_content += "   <bufs_num>" + int2string(sm_globals::bufs_num) + string("</bufs_num>\n");
   cfg_file_content += "   <max_log_files>" + int2string(sm_globals::max_log_files) + string("</max_log_files>\n");
   cfg_file_content += "   <tmp_file_initial_size>" + int2string(sm_globals::tmp_file_initial_size) + string("</tmp_file_initial_size>\n");

   sprintf(buf, "%.2f", sm_globals::upd_crt);

   cfg_file_content += "   <upd_crt>" + string(buf) + string("</upd_crt>\n");
   cfg_file_content += "</db>\n";

   int res = uWriteFile(cfg_file_handle,
                        cfg_file_content.c_str(),
                        (unsigned)cfg_file_content.size(),
                        &nbytes_written,
                        __sys_call_error);

   if ( res == 0 || nbytes_written != cfg_file_content.size())
      throw USER_EXCEPTION2(SE4045, cfg_file_name.c_str());

   if (uCloseFile(cfg_file_handle, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4043, "configuration file");
}

void create_data_directory()
{
   string data_files_path = string(SEDNA_DATA) + string("/data/");
   USECURITY_ATTRIBUTES *dir_sa;

   if(uCreateSA(&dir_sa, U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);

   if (uMkDir(data_files_path.c_str(), dir_sa, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4300, data_files_path.c_str());

   data_files_path += string(sm_globals::db_name) + "_files";

   if (uMkDir(data_files_path.c_str(), dir_sa, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4300, data_files_path.c_str());

   uReleaseSA(dir_sa, __sys_call_error);
}


lsize_t determine_layer_size(int db_id, const gov_header_struct& cfg)
{
    char buf[128];
    char path_buf[U_MAX_PATH + 10];
    lsize_t layer_size = 0;
    UPID pid;
    UPHANDLE process_handle;
    UShMem p_cdb_callback_file_mapping;
    lsize_t *p_cdb_callback_data;

    U_ASSERT(db_id != -1);

    uSetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, u_itoa(db_id, buf, 10), NULL, __sys_call_error);
    uSetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, u_itoa(cfg.os_primitives_id_min_bound, buf, 10), NULL, __sys_call_error);

    std::string path_str = uGetImageProcPath(path_buf, __sys_call_error) + std::string("/") + SESSION_EXE;
    strcpy(path_buf, path_str.c_str());

    // Result from transaction will be returned to this shared memory segment
    // So, we should create and attach it
    if (uCreateShMem(&p_cdb_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, sizeof(lsize_t), NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4016, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
    p_cdb_callback_data = (lsize_t *)uAttachShMem(&p_cdb_callback_file_mapping, NULL, 0, __sys_call_error);
    if (p_cdb_callback_data == NULL)
        throw USER_EXCEPTION2(SE4023, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    // setting hint size from command line
    *p_cdb_callback_data = (lsize_t)(cdb_globals::layer_size * 1024 * 1024);

    if (uCreateProcess(path_buf,
        false, // inherit handles
        NULL,
        U_DETACHED_PROCESS,
        &process_handle,
        NULL,
        &pid,
        NULL,
        NULL,
        __sys_call_error) != 0)
        throw USER_ENV_EXCEPTION("Cannot create process to determine VMM region", false);

    int status = 0;
    int res = 0;

    res = uWaitForChildProcess(pid, process_handle, &status, __sys_call_error);
    if (0 != res || status)
        throw USER_ENV_EXCEPTION((std::string("Cannot determine VMM region, status: ") + int2string(status) + ", result: " + int2string(res)).c_str(), false);

    uCloseProcessHandle(process_handle, __sys_call_error);

    // for the next se_trn run
    uSetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, "-1", NULL, __sys_call_error);

    // size of the layer should be right there
    layer_size = *p_cdb_callback_data;

    // dettach/destroy the mapping
    if (uDettachShMem(&p_cdb_callback_file_mapping, p_cdb_callback_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
    if (uReleaseShMem(&p_cdb_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4020, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    if (layer_size < VMM_REGION_MIN_SIZE)
        throw USER_EXCEPTION2(SE1031, (std::string("Determined layer size: ") + int2string(LAYER_ADDRESS_SPACE_SIZE)).c_str());

    elog(EL_INFO,  ("Layer address space size = 0x%x", layer_size));

    return layer_size;
}

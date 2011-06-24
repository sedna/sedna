/*
* File:  ipc_ops.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/


#include <string>

#include "common/sedna.h"

#include "common/ipc_ops.h"
#include "common/base.h"
#include "common/utils.h"
#include "common/u/usocket.h"
#include "common/u/uhdd.h"
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"
#include "expat.h"


static std::string elem_content;
char* SEDNA_DATA = NULL;
void* sedna_gov_shm_ptr = NULL;
static UShMem gov_shm_service_dsc;

/******************************************************************************
 * Governor shared memory open/close
 ******************************************************************************/


void
open_gov_shm()
{
    if ( NULL == sedna_gov_shm_ptr )
    {
        if (0 != uOpenShMem(&gov_shm_service_dsc,
            GOVERNOR_SHARED_MEMORY_NAME,
            __sys_call_error))
            throw USER_EXCEPTION2(SE4400, "Can't open GOVERNOR shared memory");   /// SEDNA server is not running


        sedna_gov_shm_ptr = uAttachShMem(&gov_shm_service_dsc,
            NULL,
            0,
            __sys_call_error);

        if (NULL == sedna_gov_shm_ptr)
            throw USER_EXCEPTION2(SE4023, "GOVERNOR shared memory");   /// Can't attach to shared memory
    }
}


int
close_gov_shm()
{
    if ( NULL != sedna_gov_shm_ptr )
    {
        if ( 0 != uDettachShMem(&gov_shm_service_dsc, sedna_gov_shm_ptr, __sys_call_error))
            return -1;

        if ( 0 != uCloseShMem(&gov_shm_service_dsc, __sys_call_error))
            return -1;
    }

    return 0;
}


/******************************************************************************
 * Governor communication mechanism
 ******************************************************************************/


void send_command_to_gov(int port_number, char gov_address[], int cmd)
{
    USOCKET s;
    int rc;
    char *ptr;
    int32_t tmp;

    s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);

    if (uconnect_tcp(s, port_number, gov_address, __sys_call_error) == 0)
    {
        tmp = htonl(cmd);
        ptr = (char*) &(tmp);
        rc = 0;
        while(rc < 4)
        {
            rc += usend(s, ptr+rc, 4-rc, __sys_call_error);
        }
        rc = 0;
        ptr = (char*) &(rc);
        while(rc < 4)
        {
            rc += usend(s, ptr+rc, 4-rc, __sys_call_error);
        }
        ushutdown_close_socket(s, __sys_call_error);
    }
    else
        d_printf2("SOCKET ERROR: %s\n",usocket_error_translator());
}


int get_db_id_by_name(gov_config_struct* cfg, const char* db_name)
{
    for(int i=0; i< MAX_DBS_NUMBER; ++i)
    {
        if (strcmp(cfg->db_vars[i].db_name, db_name) == 0)
            return i;
    }

    return -1;
}

int get_next_free_db_id(gov_config_struct* cfg)
{
    for (int i=0; i< MAX_DBS_NUMBER; ++i)
    {
        if (cfg->db_vars[i].db_name[0] == '\0')
            return i;
    }

    return -1;
}

bool is_database_running(gov_config_struct* cfg, int database_id)
{
    return cfg->db_vars[database_id].mode != OM_SM_DOWN;
}

void erase_database_cell_in_gov_shm(int db_id, gov_config_struct* cfg)
{
    if (!cfg) return;
    if (db_id >= MAX_DBS_NUMBER || db_id < 0) return;

    memset(&(cfg->db_vars[db_id]), '\0', sizeof(gov_db_struct));
}


void fill_database_cell_in_gov_shm(gov_config_struct* cfg,
                                   int db_id,
                                   const char* db_name,
                                   int bufs_num,
                                   int max_trs_num,
                                   double upd_crt,
                                   int max_log_files,
                                   int tmp_file_initial_size /* size in PAGES */)
{
    strcpy(cfg->db_vars[db_id].db_name, db_name);
    cfg->db_vars[db_id].mode = OM_SM_DOWN;
    cfg->db_vars[db_id].sm_pid = -1;
    cfg->db_vars[db_id].bufs_num = bufs_num;
    cfg->db_vars[db_id].max_trs_num = max_trs_num;
    cfg->db_vars[db_id].upd_crt = upd_crt;
    cfg->db_vars[db_id].max_log_files = max_log_files;
    cfg->db_vars[db_id].tmp_file_initial_size = tmp_file_initial_size;
}


/******************************************************************************
 * Parser for sednaconf file
 ******************************************************************************/


static void startElement_gov_cfg(void *cfg, const char *name, const char **atts)
{
}

static void endElement_gov_cfg(void *cfg, const char *name)
{
    if (strcmp(name, "sedna_data") == 0)
    {
        strcpy(((gov_header_struct*)cfg)->SEDNA_DATA, trim(elem_content).c_str());
    }
    if (strcmp(name, "listener_address") == 0)
    {
        strcpy(((gov_header_struct*)cfg)->lstnr_addr, trim(elem_content).c_str());
    }
    if (strcmp(name, "listener_port") == 0)
    {
        ((gov_header_struct*)cfg)->lstnr_port_number = atoi(trim(elem_content).c_str());
    }
    if (strcmp(name, "ping_port") == 0)
    {
        ((gov_header_struct*)cfg)->ping_port_number = atoi(trim(elem_content).c_str());
    }
    if (strcmp(name, "os_primitives_id_min_bound") == 0)
    {
        ((gov_header_struct*)cfg)->os_primitives_id_min_bound = atoi(trim(elem_content).c_str());
    }
    if (strcmp(name, "event_log_level") == 0)
    {
        ((gov_header_struct*)cfg)->el_level = atoi(trim(elem_content).c_str());
    }
    if (strcmp(name, "keep_alive_timeout") == 0)
    {
        ((gov_header_struct*)cfg)->ka_timeout = atoi(trim(elem_content).c_str());
    }
    if (strcmp(name, "session_stack_depth") == 0)
    {
        ((gov_header_struct*)cfg)->pp_stack_depth = atoi(trim(elem_content).c_str());
    }

    elem_content = "";
}

static void characterData_gov_cfg(void *cfg, const XML_Char *s, int len)
{
    elem_content.append(s, len);
}

static void parse_config_file(gov_header_struct* cfg, std::string& cfg_text)
{
    XML_Parser parser = XML_ParserCreate (NULL);
    XML_SetElementHandler (parser, startElement_gov_cfg, endElement_gov_cfg);
    XML_SetCharacterDataHandler (parser, characterData_gov_cfg);
    XML_SetUserData (parser, cfg);

    if(cfg_text.length() > INT_MAX)
        throw USER_EXCEPTION2(SE4201,  "Too long sednaconf.xml file");

    int parse_res = XML_Parse (parser, cfg_text.c_str(), (int)cfg_text.length(), 1);

    if(parse_res == XML_STATUS_ERROR)
        throw USER_EXCEPTION2(SE4201,  "sednaconf.xml");

    XML_ParserFree(parser);
}


/******************************************************************************
 * Retrieve system config parameters
 ******************************************************************************/


void get_sednaconf_values(gov_header_struct* cfg)
{
    char sedna_cfg_file[U_MAX_PATH + 30];
    char proc_buf[U_MAX_PATH + 1];

    FILE* fs;
    char buf[1024];
    size_t size;
    std::string cfg_text;
    cfg_text.reserve(10240);

    uGetImageProcPath(proc_buf, __sys_call_error);
    if (proc_buf[0] == '\0')
        throw USER_EXCEPTION(SE4081);

    cfg->is_server_stop = SE_STOP_NO;
    strcpy(cfg->lstnr_addr, "localhost");
    cfg->lstnr_port_number = 5050;
    cfg->ping_port_number = 5151;
    cfg->os_primitives_id_min_bound = 1500;
    cfg->el_level = 3;
    cfg->ka_timeout = 0;
    cfg->pp_stack_depth = 4000;

    strcpy(cfg->SEDNA_DATA, proc_buf);
    strcpy(sedna_cfg_file,  proc_buf);

#ifdef _WIN32
    strcat(cfg->SEDNA_DATA, "\\..");
    strcat(sedna_cfg_file, "\\..\\etc\\sednaconf.xml");
#else
    strcat(cfg->SEDNA_DATA, "/..");
    strcat(sedna_cfg_file, "/../etc/sednaconf.xml");
#endif

    fs = fopen(sedna_cfg_file, "r");

#ifndef _WIN32
    if(NULL == fs)
    {
        strcpy(sedna_cfg_file, "/etc/sednaconf.xml");
        fs = fopen(sedna_cfg_file, "r");
    }
#endif /* _WIN32 */

    if (fs != NULL)
    {
        d_printf2("sedna_cfg_file=%s\n", sedna_cfg_file);

        while (true)
        {
            size = fread(buf, sizeof(char), 1024, fs);
            if (ferror(fs)) throw USER_EXCEPTION2(SE4044, sedna_cfg_file);
            cfg_text.append(buf, size);
            if (feof(fs)) break;
        }

        fclose(fs);
        parse_config_file(cfg, cfg_text);
    }
}


/*
 * The following methods fills given buffer with the sedna_data path.
 * They MUST not throw exception and should use fprintf(stderr, " ... ") to
 * log out error message.
 * The reason why we need this method is sedna soft fault mechanism, which
 * can be triggred at the very begining of any sedna process when
 * SEDNA_DATA variable is not set.
 */
static void get_sedna_data_path(const char* cfg_text, char* buf)
{
      const char* beg, *fin;
      const char* sedna_data_open_tag  = "<sedna_data>";
      const char* sedna_data_close_tag = "</sedna_data>";
      unsigned short value_shift = (unsigned short)strlen(sedna_data_open_tag);
      std::string save_buf(buf);

      beg = strstr(cfg_text, sedna_data_open_tag);
      fin = strstr(cfg_text, sedna_data_close_tag);

      d_printf2("cfg_text=%s\n", cfg_text);
      if (beg == NULL || fin == NULL) return;

      memcpy(buf, beg + value_shift, fin - beg + value_shift);
      buf[fin - beg + value_shift] = '\0';

      std::string tmp(buf);
      tmp = trim(tmp);

      if(tmp.length() > U_MAX_PATH)
      {
          fprintf(stderr, "Path in the 'sedna_data' parameter is too long in sednaconf.xml. Going to use default value.\n");
          strcpy(buf, save_buf.c_str());
      }
      else
          strcpy(buf, tmp.c_str());
}

int set_sedna_data(char* sd_buf, sys_call_error_fun fun)
{
    char proc_buf[U_MAX_PATH + 1];
    char sedna_cfg_file[U_MAX_PATH + 1];
    FILE* fs;
    char buf[1024];
    size_t size;
    std::string cfg_text;
    cfg_text.reserve(10240);

    uGetImageProcPath(proc_buf, fun);
    if (proc_buf[0] == '\0')
    {   fprintf(stderr, "Can't get process path to set sedna data\n");
    return 0;
    }

    strcpy(sd_buf, proc_buf);  /// Copy default SEDNA_DATA value
    strcpy(sedna_cfg_file, proc_buf);

#ifdef _WIN32
    strcat(sd_buf, "\\..");
    strcat(sedna_cfg_file, "\\..\\etc\\sednaconf.xml");
#else
    strcat(sd_buf, "/..");
    strcat(sedna_cfg_file, "/../etc/sednaconf.xml");
#endif

    fs = fopen(sedna_cfg_file, "r");

#ifndef _WIN32
    if(NULL == fs)
    {
        strcpy(sedna_cfg_file, "/etc/sednaconf.xml");
        fs = fopen(sedna_cfg_file, "r");
    }
#endif /* _WIN32 */

    if (fs != NULL)
    {
        d_printf2("sedna_cfg_file=%s\n", sedna_cfg_file);
        while (true)
        {
            size = fread(buf, sizeof(char), 1024, fs);
            if (ferror(fs))
            {
                fclose(fs);
                fprintf(stderr, "Can't read sednaconf.xml to set sedna data path. Going to use default value.\n");
                return 1;
            }
            cfg_text.append(buf, size);
            if (feof(fs)) break;
        }
        fclose(fs);

        get_sedna_data_path(cfg_text.c_str(), sd_buf);
    }

    d_printf2("sedna data path retrieved=%s\n", sd_buf);

    return 1;
}


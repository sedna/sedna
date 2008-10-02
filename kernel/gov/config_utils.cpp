/*
 * File:  config_utils.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "common/u/uhdd.h"
#include "gov/config_utils.h"
#include "common/errdbg/d_printf.h"
#include "gov/gov_globals.h"
#include "common/ipc_ops.h"
#include "common/utils.h"


static void fulfill_sm_parameters_from_config_files(gov_config_struct* cfg);
static std::string elem_content;


void fulfill_config_parameters(gov_config_struct* cfg)
{
  char proc_buf[U_MAX_PATH + 1];
  
  /*
    First of all we load the cfg parameter by default values.
    Then we check whether the config file exists and if it exists then we load parameters from it.
    Then we analyze the command line parameters and overwrite corresponding parameters in cfg structure.
  */

  memset(cfg, '\0', sizeof(gov_config_struct));
  get_sednaconf_values(&(cfg->gov_vars));

  if (lstnr_port != cfg->gov_vars.lstnr_port_number)
      cfg->gov_vars.lstnr_port_number = lstnr_port;
  if (ping_port != cfg->gov_vars.ping_port_number) 
      cfg->gov_vars.ping_port_number = ping_port;

  cfg->gov_vars.gov_pid = uGetCurrentProcessId(__sys_call_error);

  for (int i = 0; i<MAX_DBS_NUMBER; i++)
  {
     cfg->db_vars[i].db_name[0] = '\0';
     cfg->db_vars[i].is_stop = -1;
     cfg->db_vars[i].sm_pid = 0;
  }

  for (int i = 0; i<MAX_SESSIONS_NUMBER; i++)
  {
     cfg->sess_vars[i].idfree = 0;
     cfg->sess_vars[i].stop = 0;
  }

  fulfill_sm_parameters_from_config_files(cfg);
}


/******************************************************************************
                   Parser for database config files
/******************************************************************************/


static void startElement_sm_cfg(void *cnt, const char *name, const char **atts)
{
}

static void endElement_sm_cfg(void *cfg, const char *name)
{
  if (strcmp(name, "bufs_num") == 0)
  {  
     ((gov_db_struct*)cfg)->bufs_num = atoi(trim(elem_content).c_str());
  }
  if (strcmp(name, "max_trs_num") == 0)
  {
     ((gov_db_struct*)cfg)->max_trs_num = atoi(trim(elem_content).c_str());
  }
  if (strcmp(name, "logical_log_file_size") == 0)
  {
     ((gov_db_struct*)cfg)->logical_log_file_size = atoi(trim(elem_content).c_str()) * 0x100000;
  }
  if (strcmp(name, "upd_crt") == 0)
  {
     ((gov_db_struct*)cfg)->upd_crt = atof(trim(elem_content).c_str());
  }
  elem_content = "";
}

static void characterData_sm_cfg(void *cnt, const XML_Char *s, int len)
{
   elem_content.append(s, len);
}

static void parse_sm_config_file(gov_db_struct* db_cfg, std::string cfg_text)
{
  XML_Parser parser = XML_ParserCreate (NULL);
  XML_SetElementHandler (parser, startElement_sm_cfg, endElement_sm_cfg);
  XML_SetCharacterDataHandler (parser, characterData_sm_cfg);

  XML_SetUserData (parser, db_cfg);

  d_printf2("cfg_text=%s\n", cfg_text.c_str());

  int parse_res = XML_Parse (parser, cfg_text.c_str(), cfg_text.length(), 1);

  if(parse_res == XML_STATUS_ERROR)
    throw USER_EXCEPTION2(SE4201,  "database config file");

  XML_ParserFree(parser);
}


/******************************************************************************
                   Fulfill databases parameters
/******************************************************************************/


static void fulfill_sm_parameters_from_config_files(gov_config_struct* cfg)
{
   UDir cfg_dir;
   UFindDataStruct find_data;   
   int res, index, size;
   FILE * fs;
   char buf[1024];
   std::string cfg_text;
   cfg_text.reserve(10240);

#ifdef _WIN32
   std::string cfg_dir_name = std::string(cfg->gov_vars.SEDNA_DATA) + "\\cfg";
#else
   std::string cfg_dir_name = std::string(cfg->gov_vars.SEDNA_DATA) + "/cfg";
#endif
   
   cfg_dir =  uFindFirstFile(cfg_dir_name.c_str(), &find_data, __sys_call_error);
   if (cfg_dir == U_INVALID_DIR) return; /// There is no cfg direcory

      
   for(int i=0 ; 1 ;)
   {
      if (i>= MAX_DBS_NUMBER)
         throw USER_EXCEPTION2(SE4412, cfg_dir_name.c_str());

      if ((index = std::string(find_data.fname).find("_cfg.xml")) != std::string::npos)
      {
          strcpy(cfg->db_vars[i].db_name, (std::string(find_data.fname).substr(0, index)).c_str());

          fs = fopen((cfg_dir_name + std::string("/") + cfg->db_vars[i].db_name + std::string("_cfg.xml")).c_str(), "r");

          if (fs == NULL) 
             throw USER_EXCEPTION2(SE4042, (cfg_dir_name + std::string("/") + cfg->db_vars[i].db_name + std::string("_cfg.xml")).c_str());

          cfg_text = "";

          while(!feof(fs))
          {
              size_t len = fread (buf, sizeof(char), 1024, fs);

              if ( ferror(fs) )
                throw USER_EXCEPTION2(SE4044,  "database config file");

               cfg_text.append(buf, len);
          }

          if (fs) fclose(fs);

          parse_sm_config_file(&(cfg->db_vars[i]), cfg_text);

          ++i;
      }

      res = uFindNextFile(cfg_dir, &find_data, __sys_call_error);      

      if (res == 0) break; /// There is no more files in cfg dir

      if (res == -1)
         throw USER_EXCEPTION2(SE4083, cfg_dir_name.c_str());
   }

   if (!uFindClose(cfg_dir, __sys_call_error))
      throw USER_EXCEPTION2(SE4054, cfg_dir_name.c_str());
}


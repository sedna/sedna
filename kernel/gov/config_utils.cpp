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


#define MERGE_GOV_GLOBAL_VARIABLE(command_line_name, config_name)     ((-1 == (command_line_name)) ? \
                                                                      (command_line_name) = (config_name) : \
                                                                      (config_name) = (command_line_name))

void fulfill_config_parameters(gov_config_struct* cfg)
{
  memset(cfg, '\0', sizeof(gov_config_struct));

  ///  Within this the following we load the cfg parameters by default values.
  ///  Then we check whether the config file exists and if it exists then we overwrite
  ///  parameters from it.
  get_sednaconf_values(&(cfg->gov_vars));

  ///  Then we merge the command line parameters and corresponding parameters in cfg structure.
  ///  Command line parameter has priority over corresponding parameter in cfg structure.
  if ('\0' == gov_globals::cl_lstnr_addr[0])  {
    strcpy(gov_globals::cl_lstnr_addr, cfg->gov_vars.lstnr_addr);
  } else  {
    strcpy(cfg->gov_vars.lstnr_addr, gov_globals::cl_lstnr_addr);
  }
  MERGE_GOV_GLOBAL_VARIABLE(gov_globals::cl_lstnr_port, cfg->gov_vars.lstnr_port_number);
  MERGE_GOV_GLOBAL_VARIABLE(gov_globals::cl_ping_port,  cfg->gov_vars.ping_port_number);
  MERGE_GOV_GLOBAL_VARIABLE(gov_globals::cl_el_level,   cfg->gov_vars.el_level);
  MERGE_GOV_GLOBAL_VARIABLE(gov_globals::cl_ka_timeout, cfg->gov_vars.ka_timeout);
  MERGE_GOV_GLOBAL_VARIABLE(gov_globals::cl_pp_stack_depth, cfg->gov_vars.pp_stack_depth);

  cfg->gov_vars.gov_pid = uGetCurrentProcessId(__sys_call_error);

  for (int i = 0; i<MAX_DBS_NUMBER; i++)
  {
     cfg->db_vars[i].db_name[0] = '\0';
     cfg->db_vars[i].mode = OM_SM_DOWN;
     cfg->db_vars[i].sm_pid = -1;
     cfg->db_vars[i].upd_crt = 0.25;
     cfg->db_vars[i].max_log_files = 3;
     cfg->db_vars[i].tmp_file_initial_size = 1600;
  }

  for (int i = 0; i<MAX_SESSIONS_NUMBER; i++)
  {
     cfg->sess_vars[i].idfree = 0;
     cfg->sess_vars[i].stop = 0;
  }

  /// Parse "cfg/${db_name}_cfg.xml" files
  fulfill_sm_parameters_from_config_files(cfg);
}


/******************************************************************************
                   Parser for database config files
******************************************************************************/


static void startElement_sm_cfg(void *cnt, const char *name, const char **atts)
{
}

static void endElement_sm_cfg(void *cfg, const char *name)
{
  if (strcmp(name, "bufs_num") == 0)
  {
     ((gov_db_struct*)cfg)->bufs_num = atoi(trim(elem_content).c_str());
  }
  if (strcmp(name, "upd_crt") == 0)
  {
     ((gov_db_struct*)cfg)->upd_crt = atof(trim(elem_content).c_str());
  }
  if (strcmp(name, "max_log_files") == 0)
  {
     ((gov_db_struct*)cfg)->max_log_files = atoi(trim(elem_content).c_str());
  }
  if (strcmp(name, "tmp_file_initial_size") == 0)
  {
     ((gov_db_struct*)cfg)->tmp_file_initial_size = (int)MBS2PAGES(atoi(trim(elem_content).c_str()));
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

  if(cfg_text.length() >= INT_MAX)
    throw USER_EXCEPTION2(SE4201,  "too long database config file");

  int parse_res = XML_Parse (parser, cfg_text.c_str(), (int)cfg_text.length(), 1);

  if(parse_res == XML_STATUS_ERROR)
    throw USER_EXCEPTION2(SE4201,  "database config file");

  XML_ParserFree(parser);
}


/******************************************************************************
                   Fulfill databases parameters
******************************************************************************/


static void fulfill_sm_parameters_from_config_files(gov_config_struct* cfg)
{
   UDir cfg_dir;
   UFindDataStruct find_data;
   int res;
   std::string::size_type index;
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


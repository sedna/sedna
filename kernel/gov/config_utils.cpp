#include <string>

#include "common/config.h"
#include "common/u/uhdd.h"
#include "gov/config_utils.h"
#include "common/errdbg/d_printf.h"
#include "gov/gov_globals.h"
#include "../libs/expat/expat.h"
#include "common/ipc_ops.h"


using namespace std;

void fullfill_config_parameters(gov_config_struct* cfg)
{
  /*first of all we load the cfg parameter by default values,
    then we check whether the config file exists and if it exists then we load parameters from it
    Then we analyze the command line parameters and overwrite corresponding parameters in cfg structure
  */

  char proc_buf[U_MAX_PATH + 1];

  //init cfg from default values
  memset(cfg, '\0', sizeof(gov_config_struct));

  get_default_sednaconf_values(&(cfg->gov_vars));

  cfg->gov_vars.gov_pid = uGetCurrentProcessId(__sys_call_error);
  int i;
  for (i = 0; i<MAX_DBS_NUMBER; i++)
  {
     cfg->db_vars[i].db_name[0] = '\0';
     cfg->db_vars[i].is_stop = -1;
     cfg->db_vars[i].sm_pid = 0;
  }

  for (i = 0; i<MAX_SESSIONS_NUMBER; i++)
  {
     cfg->sess_vars[i].idfree = 0;
     cfg->sess_vars[i].stop = 0;
  }

  fulfill_common_system_parameters_from_config_file_and_command_line(cfg, proc_buf);

  fulfill_sm_parameters_from_config_files(cfg);

}


void fulfill_common_system_parameters_from_config_file_and_command_line(gov_config_struct* cfg, const char* gov_proc_buf)
{
  //overwrite parameters from config file (if it exists!)

  get_gov_config_parameters_from_sednaconf(&(cfg->gov_vars));

  //overwrite parameters values given via command line
  if (lstnr_port != 5050)//initial value
     cfg->gov_vars.lstnr_port_number = lstnr_port;

  if (ping_port != 5151) 
     cfg->gov_vars.ping_port_number = ping_port;
}

void fulfill_sm_parameters_from_config_files(gov_config_struct* cfg)
{
#ifdef _WIN32
   string cfg_dir_name = string(cfg->gov_vars.SEDNA_DATA) + "\\cfg";
#else
   string cfg_dir_name = string(cfg->gov_vars.SEDNA_DATA) + "/cfg";
#endif


   UDir cfg_dir;
   UFindDataStruct find_data;   

   cfg_dir =  uFindFirstFile(cfg_dir_name.c_str(), &find_data, __sys_call_error);

   if (cfg_dir == U_INVALID_DIR)
      return;//there is no cfg direcory

   int res, index;
   FILE * fs;
   string cfg_text;
   cfg_text.reserve(10240);//reserve 10Kb
   int size;
   char buf[1024];


   for(int i=0 ; 1 ;)
   {
      if (i>= MAX_DBS_NUMBER)
         throw USER_EXCEPTION2(SE4412, cfg_dir_name.c_str());

      if ((index = string(find_data.fname).find("_cfg.xml")) != std::string::npos)
      {//database config file
          strcpy(cfg->db_vars[i].db_name, (string(find_data.fname).substr(0, index)).c_str());

          fs = fopen((cfg_dir_name + string("/") + cfg->db_vars[i].db_name + string("_cfg.xml")).c_str(), "r");

          if (fs == NULL) 
             throw USER_EXCEPTION2(SE4042, (cfg_dir_name + string("/") + cfg->db_vars[i].db_name + string("_cfg.xml")).c_str());

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

//        parse config file
//        cfg->db_vars[i].phys_log_size = 0;
//        cfg->db_vars[i].logical_log_file_size = 0;
          ++i;
      }

      res = uFindNextFile(cfg_dir, &find_data, __sys_call_error);      

      if (res == 0)
          break;//there is no more files in cfg dir

      if (res == -1)
         throw USER_EXCEPTION2(SE4083, cfg_dir_name.c_str());
   }


   if (!uFindClose(cfg_dir, __sys_call_error))
      throw USER_EXCEPTION2(SE4054, cfg_dir_name.c_str());
}

void parse_sm_config_file(gov_db_struct* db_cfg, std::string cfg_text)
{
  XML_Parser parser = XML_ParserCreate (NULL);

  XML_SetElementHandler (parser, startElement_sm_cfg, endElement_sm_cfg);

  XML_SetCharacterDataHandler (parser, characterData_sm_cfg);
   
  XML_SetUserData (parser, db_cfg);

  int parse_res;

  d_printf2("cfg_text=%s\n", cfg_text.c_str());

  parse_res = XML_Parse (parser, cfg_text.c_str(), cfg_text.length(), 1);

  if(parse_res == XML_STATUS_ERROR)
    throw USER_EXCEPTION2(SE4201,  "database config file");

  XML_ParserFree(parser);
}


/******************************************************************************/
// HANDLERS FOR PARSING Storage Manager (SM) CONFIG FILES
/******************************************************************************/

void startElement_sm_cfg(void *cnt, const char *name, const char **atts)
{
}

void endElement_sm_cfg(void *cfg, const char *name)
{
  if (strcmp(name, "bufs_num") == 0)
  {  
     ((gov_db_struct*)cfg)->bufs_num = atoi(erase_ws(elem_content.c_str()).c_str());
  }

  if (strcmp(name, "max_trs_num") == 0)
  {
     ((gov_db_struct*)cfg)->max_trs_num = atoi(erase_ws(elem_content.c_str()).c_str());
  }

  if (strcmp(name, "phys_log_ext_portion") == 0)
  {
     ((gov_db_struct*)cfg)->phys_log_ext_portion = atoi(erase_ws(elem_content.c_str()).c_str()) * 0x100000;
  }

  if (strcmp(name, "init_phys_log_size") == 0)
  {
     ((gov_db_struct*)cfg)->phys_log_size = atoi(erase_ws(elem_content.c_str()).c_str()) * 0x100000;
  }

  if (strcmp(name, "logical_log_file_size") == 0)
  {
     ((gov_db_struct*)cfg)->phys_log_size = atoi(erase_ws(elem_content.c_str()).c_str()) * 0x100000;
  }

  if (strcmp(name, "upd_crt") == 0)
  {
     ((gov_db_struct*)cfg)->upd_crt = atof(erase_ws(elem_content.c_str()).c_str());
  }

  elem_content = "";
}



void characterData_sm_cfg(void *cnt, const XML_Char *s, int len)
{
   elem_content.append(s, len);
}


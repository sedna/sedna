/*
 * File:  ipc_ops.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "common/ipc_ops.h"
#include "common/base.h"
#include "common/u/usocket.h"
#include "common/u/uhdd.h"
#include "common/errdbg/d_printf.h"
#include "common/config.h"
#include "../libs/expat/expat.h"

#include <string>

using namespace std;

bool gov_shared_memory_opened = false;

void parse_config_file(gov_header_struct* cfg, string& cfg_text);
void startElement_gov_cfg(void *cfg, const char *name, const char **atts);
void endElement_gov_cfg(void *cfg, const char *name);
void characterData_gov_cfg(void *cfg, const XML_Char *s, int len);
char* get_sedna_data_path(const char* cfg_text, char* buf);
string erase_ws(string str);


int WriteHead(UPIPE p, int *cmd, int *len)
{
   int res = uWritePipeAll(p, cmd, sizeof(int), __sys_call_error);
   if(res < 0) 
   {
     d_printf1("Pipe error\n");
     return -1;
   }

   res = uWritePipeAll(p, len, sizeof(int), __sys_call_error);
   if(res < 0) 
   {
      d_printf1("Pipe error\n");
      return -1;
   }
   return 0;
}

int ReadHead(UPIPE p, int *cmd, int *len)
{
   int res = uReadPipeAll(p, cmd, sizeof(int), __sys_call_error);
   if (res < 0) 
   {
      d_printf1("Pipe error\n");
      return -1;
   }

   res = uReadPipeAll(p, len, sizeof(int), __sys_call_error);
   if (res < 0) 
   {
      d_printf1("Pipe error\n");
      return -1;
   }

   return 0;
}



void* open_gov_shm(UShMem *gov_shm_service_dsc)
{
   void* gov_shared_mem;

   if (0 != uOpenShMem(gov_shm_service_dsc,
                       GOVERNOR_SHARED_MEMORY_NAME,
                       sizeof(gov_config_struct),
                       __sys_call_error
                      ))
      throw USER_EXCEPTION(SE4400);


   gov_shared_mem = uAttachShMem(*gov_shm_service_dsc,
                                 NULL,
                                 sizeof(gov_config_struct),
                                 __sys_call_error
                                );

   if (gov_shared_mem == NULL)
      throw USER_EXCEPTION2(SE4023, "GOVERNOR_SHARED_MEMORY_NAME");

   gov_shared_memory_opened = true;

   return gov_shared_mem;

}


int close_gov_shm(UShMem gov_shm_service_dsc, void* gov_shared_mem)
{
  if (gov_shared_memory_opened)
  {
     if ( 0 != uDettachShMem(gov_shm_service_dsc, gov_shared_mem, __sys_call_error))
       return -1;

     if ( 0 != uCloseShMem(gov_shm_service_dsc, __sys_call_error))
       return -1;
  }

  return 0;
}

/*****************************************************************/
/*****************************************************************/
/*****************************************************************/
/*****************************************************************/

void get_default_sednaconf_values(gov_header_struct* cfg)
{
  char proc_buf[U_MAX_PATH + 1];
  uGetImageProcPath(proc_buf, __sys_call_error);
  if (proc_buf[0] == '\0') 
      throw USER_EXCEPTION(SE4081);

#ifdef _WIN32
  strcpy(cfg->SEDNA_DATA, proc_buf);
  strcat(cfg->SEDNA_DATA, "\\..");
#else
  strcpy(cfg->SEDNA_DATA, proc_buf);
  strcat(cfg->SEDNA_DATA, "/..");
#endif

  cfg->is_server_stop = 0;
  cfg->lstnr_port_number = 5050;
  cfg->ping_port_number = 5151;
  cfg->os_primitives_id_min_bound = 1500;
}

void get_gov_config_parameters_from_sednaconf(gov_header_struct* cfg)
{

  //find and parse sednaconf.xml
  char sedna_cfg_file[U_MAX_PATH + 1];
  char proc_buf[U_MAX_PATH + 1];
  char sedna_data_path[U_MAX_PATH + 1];

  uGetImageProcPath(proc_buf, __sys_call_error);
  if (proc_buf[0] == '\0') 
      throw USER_EXCEPTION(SE4081);


#ifdef _WIN32
  strcpy(sedna_cfg_file, proc_buf);
  strcat(sedna_cfg_file, "\\..\\etc\\sednaconf.xml");
#else
  strcpy(sedna_cfg_file, proc_buf);
  strcat(sedna_cfg_file, "/..");
  strcat(sedna_cfg_file, "/etc/sednaconf.xml");
#endif

  FILE* fs;
  char buf[1024];
  int i, size;

  d_printf2("sedna_cfg_file=%s\n", sedna_cfg_file);

  fs = fopen(sedna_cfg_file, "r");
  std::string cfg_text;
  cfg_text.reserve(10240);//reserve 10Kb

  if (fs != NULL)  
  {//exist sednaconf.xml in etc directory
     d_printf1("exist sednaconf.xml in local etc\n");

     while (true)
     {
        size = fread(buf, sizeof(char), 1024, fs);

        if (ferror(fs)) throw USER_EXCEPTION2(SE4044, "sednaconf.xml");

        cfg_text.append(buf, size);

        if (feof(fs)) break; 
     }

     fclose(fs);

     parse_config_file(cfg, cfg_text);

     strcpy(cfg->SEDNA_DATA, get_sedna_data_path(cfg_text.c_str(), sedna_data_path));
  }
  else
  {
#ifndef _WIN32 //UNIX
     strcpy(sedna_cfg_file, "/etc/sednaconf.xml");
     fs = fopen(sedna_cfg_file, "r");

     if (fs != NULL)  
     {//exist sednaconf.xml in etc directory
        while(true)
        {
           size = fread(buf, sizeof(char), 1024, fs);

           if (ferror(fs)) throw USER_EXCEPTION2(SE4044, "sednaconf.xml");

           cfg_text.append(buf, size);

           if (feof(fs)) break;
        }

        fclose(fs);

        parse_config_file(cfg, cfg_text);
        strcpy(cfg->SEDNA_DATA, get_sedna_data_path(cfg_text.c_str(), sedna_data_path));
     }
#endif
  }
}


void parse_config_file(gov_header_struct* cfg, string& cfg_text)
{
  XML_Parser parser = XML_ParserCreate (NULL);

  XML_SetElementHandler (parser, startElement_gov_cfg, endElement_gov_cfg);

  XML_SetCharacterDataHandler (parser, characterData_gov_cfg);
   
  XML_SetUserData (parser, cfg);

  int parse_res;

  parse_res = XML_Parse (parser, cfg_text.c_str(), cfg_text.length(), 1);

  if(parse_res == XML_STATUS_ERROR)
    throw USER_EXCEPTION2(SE4201,  "sednaconf.xml");

  XML_ParserFree(parser);
}

/*****************************************************************************
                HANDLERS FOR PARSING Governor CONFIG FILE
/******************************************************************************/

std::string elem_content;

void startElement_gov_cfg(void *cfg, const char *name, const char **atts)
{
}

void endElement_gov_cfg(void *cfg, const char *name)
{
  if (strcmp(name, "sedna_data") == 0)
  {  
     strcpy(((gov_header_struct*)cfg)->SEDNA_DATA, erase_ws(elem_content.c_str()).c_str());
  }

  if (strcmp(name, "listener_port") == 0)
  {
     ((gov_header_struct*)cfg)->lstnr_port_number = atoi(erase_ws(elem_content.c_str()).c_str());
  }

  if (strcmp(name, "ping_port") == 0)
  {
     ((gov_header_struct*)cfg)->ping_port_number = atoi(erase_ws(elem_content.c_str()).c_str());
  }

  if (strcmp(name, "os_primitives_id_min_bound") == 0)
  {
     ((gov_header_struct*)cfg)->os_primitives_id_min_bound = atoi(erase_ws(elem_content.c_str()).c_str());
  }

  elem_content = "";
}



void characterData_gov_cfg(void *cfg, const XML_Char *s, int len)
{
   elem_content.append(s, len);
}



/*****************************************************************/
/*****************************************************************/
/*****************************************************************/
/*****************************************************************/

void send_command_to_gov(int port_number, int cmd)
{
  USOCKET s;
  int rc;
  char *ptr;
  __int32 tmp;
    
  s = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);

  if (uconnect_tcp(s, port_number, "127.0.0.1", __sys_call_error) == 0)
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

void erase_database_cell_in_gov_shm(int db_id, gov_config_struct* cfg)
{
  if (!cfg) return;
  if (db_id >= MAX_DBS_NUMBER || db_id < 0) return;

  memset(&(cfg->db_vars[db_id]), '\0', sizeof(gov_db_struct));

  return;
}


void fill_database_cell_in_gov_shm(gov_config_struct* cfg,
                                   int db_id,
                                   const char* db_name, 
                                   int bufs_num,
                                   int max_trs_num,
                                   int phys_log_ext_portion,
                                   int phys_log_size,
                                   int logical_log_file_size,
                                   double upd_crt)
{
   strcpy(cfg->db_vars[db_id].db_name, db_name);
   cfg->db_vars[db_id].is_stop = -1;
   cfg->db_vars[db_id].sm_pid = 0;
   cfg->db_vars[db_id].bufs_num = bufs_num;
   cfg->db_vars[db_id].max_trs_num = max_trs_num;
   cfg->db_vars[db_id].phys_log_ext_portion = phys_log_ext_portion;
   cfg->db_vars[db_id].phys_log_size = phys_log_size;
   cfg->db_vars[db_id].logical_log_file_size = logical_log_file_size;
   cfg->db_vars[db_id].upd_crt = upd_crt;

   return;
}            
/***************************************************************************
                           HELPERS
****************************************************************************/


string erase_ws(std::string str)
{
   std::string res;
   bool b;
   int i;

   //erase whitespaces from the begin of string
   for (i=0; i< str.size(); i++)
   {
     if (!(str[i] == ' ' || str[i] == '\t' || str[i] == '\r' || str[i] == '\n'))
        break;
   }

   res = str.substr(i, str.size() - i);  

   //erase whitespaces from the end of string
   for (i = res.size() - 1; i >= 0; --i)
   {
     if (! (res[i] == ' ' || res[i] == '\t' || res[i] == '\r' || res[i] == '\n'))
        break;
   }

   res = res.substr(0, i+1);

   return res;
}

char* SEDNA_DATA;

char* get_sedna_data_path(const char* cfg_text, char* buf)
{
   const char* beg, *fin;

   beg = strstr(cfg_text, "<sedna_data>");
   fin = strstr(cfg_text, "</sedna_data>");

   d_printf2("cfg_text=%s\n", cfg_text);
   if (beg == NULL || fin == NULL)
      return NULL;

   memcpy(buf, beg + 12, (int)fin-((int)beg + 12));
   buf[(int)fin-((int)beg + 12)] = '\0';

   std::string tmp(buf);
   tmp = erase_ws(tmp);
   
   if(tmp.length() > U_MAX_PATH) throw USER_EXCEPTION2(SE4300, "sedna_data parameter in sednaconf.xml is too long");
   
   strcpy(buf, tmp.c_str());

   return buf;   
}


int set_sedna_data(char* SEDNA_DATA, sys_call_error_fun fun)
{
  char proc_buf[U_MAX_PATH + 1];
  char sedna_cfg_file[U_MAX_PATH + 1];
  char sedna_data_path[U_MAX_PATH + 1];
  FILE* fs;
  char* cfg_text;
  int cfg_text_free_size;
  size_t size;
  char buf[1024];
  int ind;
  


  uGetImageProcPath(proc_buf, fun);
  if (proc_buf[0] == '\0') 
     return 0;
  

  //copy default values
#ifdef _WIN32
  strcpy(SEDNA_DATA, proc_buf);
  strcat(SEDNA_DATA, "\\..");
#else
  strcpy(SEDNA_DATA, proc_buf);
  strcat(SEDNA_DATA, "/..");
//  strcpy(SEDNA_DATA, "/var/lib/sedna");
#endif

#ifdef _WIN32
  strcpy(sedna_cfg_file, proc_buf);
  strcat(sedna_cfg_file, "\\..");
  strcat(sedna_cfg_file, "\\etc\\sednaconf.xml");
#else
  strcpy(sedna_cfg_file, proc_buf);
  strcat(sedna_cfg_file, "/..");
  strcat(sedna_cfg_file, "/etc/sednaconf.xml");
#endif

  d_printf2("sedna_cfg_file=%s\n", sedna_cfg_file);

  fs = fopen(sedna_cfg_file, "r");
  cfg_text = (char*)malloc(1024*sizeof(char));
  cfg_text[0] = '\0';
  cfg_text_free_size = 1023;
  if (fs != NULL)  
  {//exist sednaconf.xml in etc directory
     d_printf1("exist sednaconf.xml in local etc\n");

     while (true)
     {
        size = fread(buf, sizeof(char), 1024, fs);

        if (ferror(fs)) return 0;

        if (cfg_text_free_size < size)
           cfg_text = (char*)realloc(cfg_text, 2*1024 + strlen(cfg_text));

        ind = strlen(cfg_text) + size;
        memcpy(cfg_text + sizeof(char)*strlen(cfg_text), buf, size);
        cfg_text[ind] = '\0';

        if (feof(fs)) break; 
     }

     fclose(fs);

     strcpy(SEDNA_DATA, get_sedna_data_path(cfg_text, sedna_data_path));
  }
  else
  {
#ifndef _WIN32 //UNIX
     strcpy(sedna_cfg_file, "/etc/sednaconf.xml");
     fs = fopen(sedna_cfg_file, "r");

     if (fs != NULL)  
     {//exist sednaconf.xml in etc directory
        while(true)
        {
           size = fread(buf, sizeof(char), 1024, fs);

           if (ferror(fs)) return 0;

           if (cfg_text_free_size < size)
              cfg_text = (char*)realloc(cfg_text, 2*1024 + strlen(cfg_text));

           ind = strlen(cfg_text) + size;
           memcpy(cfg_text + sizeof(char)*strlen(cfg_text), buf, size);
           cfg_text[ind] = '\0';

           if (feof(fs)) break;
        }

        fclose(fs);
        strcpy(SEDNA_DATA, get_sedna_data_path(cfg_text, sedna_data_path));
     }
#endif       
  }

  d_printf2("SEDNA_DATA=%s\n", SEDNA_DATA);

#ifndef _WIN32
  USECURITY_ATTRIBUTES sa = U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK;
  if (uMkDir(SEDNA_DATA, &sa, fun) == 0)
      return 0;
#endif  

  return 1;
}

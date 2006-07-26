/*
 * File:  sedna.c
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "d_printf.h"

char SEDNA_DATA[SEDNA_DATA_VAR_SIZE];

int is_init_sedna_data = 0;

char* get_sedna_data_path(char* cfg_text, char* buf)
{
   char* beg, *fin;

   beg = strstr(cfg_text, "<sedna_data>");
   fin = strstr(cfg_text, "</sedna_data>");

   d_printf2("cfg_text=%s\n", cfg_text);
   if (beg == NULL || fin == NULL)
      return NULL;

   memcpy(buf, beg + 12, (int)fin-((int)beg + 12));
   buf[(int)fin-((int)beg + 12)] = '\0';

   return buf;   
}


int set_sedna_data(sys_call_error_fun fun)
{
  char proc_buf[U_MAX_PATH + 1];
  char sedna_cfg_file[U_MAX_PATH + 1];
  char sedna_data_path[U_MAX_PATH + 1];
  FILE* fs;
  char* cfg_text;
  int cfg_text_free_size;
  

  if (is_init_sedna_data)
     return;


  uGetImageProcPath(proc_buf, fun);
  if (proc_buf[0] == '\0') 
     return 0;
  

  //copy default values
#ifdef _WIN32
  strcpy(SEDNA_DATA, proc_buf);
  strcat(SEDNA_DATA, "\\..");
#else
  strcpy(SEDNA_DATA, "/var/lib/sedna");
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
  cfg_text = malloc(1024*sizeof(char));
  cfg_text[0] = '\0';
  cfg_text_free_size = 1023;
  if (fs != NULL)  
  {//exist sednaconf.xml in etc directory
     char buf[1024];
     size_t size;
     int ind;
     d_printf1("exist sednaconf.xml in local etc\n");

     while (true)
     {
        size = fread(buf, sizeof(char), 1024, fs);

        if (ferror(fs)) return 0;

        if (cfg_text_free_size < size)
           cfg_text = realloc(cfg_text, 2*1024 + strlen(cfg_text));

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
        size = fread(buf, sizeof(char), 1024, fs);

        if (ferror(fs)) return 0;

        if (cfg_text_free_size < size)
           cfg_text = realloc(cfg_text, 2*1024 + strlen(cfg_text));

        ind = strlen(cfg_text) + size;
        memcpy(cfg_text + sizeof(char)*strlen(cfg_text), buf, size);
        cfg_text[ind] = '\0';

        if (feof(fs)) break; 
     }

     fclose(fs);

     strcpy(SEDNA_DATA, get_sedna_data_path(cfg_text, sedna_data_path));
#endif       
  }

  d_printf2("SEDNA_DATA=%s\n", SEDNA_DATA);

#ifndef _WIN32
  USECURITY_ATTRIBUTES sa = U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK;
  if (uMkDir(SEDNA_DATA, &sa, fun) == 0)
      return 0;
#endif  

  is_init_sedna_data = 1;
  return 1;
}

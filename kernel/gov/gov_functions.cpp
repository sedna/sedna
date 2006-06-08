/*
 * File:  gov_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#ifdef _WIN32
#include <io.h>
#else
#include <dirent.h>
#endif

#include "sedna.h"

#include "gov_globals.h"
#include "gov_table.h"
#include "d_printf.h"
#include "uhdd.h"
#include "ugc.h"
#include "pping.h"

using namespace std;



/****************************************************************************
                         UGC functions
*****************************************************************************/

void clean_resources(bool background_off_from_background_on)
{
  gov_ugc(background_off_from_background_on);

  string cfg_files_dir;


#ifdef _WIN32

  char buf[4096];
  char buf2[4096];
  char *cur_dir;
  cur_dir  = uGetCurrentWorkingDirectory(buf, 4096, __sys_call_error);

  cfg_files_dir = string(SEDNA_DATA) + string("\\cfg");
  d_printf2("cfg_files_dir=%s\n", cfg_files_dir.c_str());

  if (uChangeWorkingDirectory(cfg_files_dir.c_str(), __sys_call_error) != 0 )
  {
     if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
        throw USER_EXCEPTION(SE4604); 

     return;//there is no any sign about databases
  }
  

  struct _finddata_t cfg_file;
  long dsc;

  //find first cfg file

  if ( (dsc = _findfirst("*_cfg.xml", &cfg_file)) == -1L)
  {
     if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
        throw USER_EXCEPTION(SE4604); 

     return;
  }

  string db_name(cfg_file.name);
  db_name = db_name.substr(0, db_name.size() - 8);

  d_printf2("db_name=%s\n", db_name.c_str());
                                          
  sm_ugc(background_off_from_background_on, db_name.c_str());

  while( _findnext(dsc, &cfg_file) == 0)
  {
     db_name = string(cfg_file.name);
     db_name = db_name.substr(0, db_name.size() - 8);
     d_printf2("db_name=%s\n", db_name.c_str());
     sm_ugc(background_off_from_background_on, db_name.c_str());
  }
     
  _findclose(dsc);

  if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
     throw USER_EXCEPTION(SE4604); 
#else
  DIR *dir;
  struct dirent* dent;
  string db_name;

  cfg_files_dir = string(SEDNA_DATA) + string("/cfg");
                                                                               
  d_printf2("cfg_files_dir=%s\n", cfg_files_dir.c_str());

  dir = opendir(cfg_files_dir.c_str());
  if (dir == NULL) return;

  string ext;
  while ( NULL != (dent = readdir (dir)) )
  {
     db_name = string(dent->d_name);
     if (db_name.size() < 8) continue;//not cfg file
     //ext = db_name.substr(db_name.size()-3, 3);
     //if (ext != "xml") continue;
     db_name = db_name.substr(0, db_name.size() - 8);
     d_printf2("db_name=%s\n", db_name.c_str());
     set_global_names(db_name.c_str());
     sm_ugc(background_off_from_background_on, db_name.c_str());
  }

  if (0 != closedir(dir))
     throw USER_EXCEPTION2(SE4054, cfg_files_dir.c_str());
#endif  
}



bool is_first_start_of_gov()
{
  try
  {
    pping_client ppc(5151, EL_GOV);

    SednaUserException e = USER_EXCEPTION(SE4400);
    ppc.startup(e);
    ppc.shutdown();
    return false;

  } catch (...) {
    return true;//cannot connect to pping server
  }
}

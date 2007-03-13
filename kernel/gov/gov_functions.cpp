/*
 * File:  gov_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#ifdef _WIN32
#include <io.h>
#else
#include <dirent.h>
#include <grp.h>
#include <time.h>
#include <locale.h>
#include <langinfo.h>
#endif

#include "common/sedna.h"

#include "gov/gov_globals.h"
#include "gov/gov_table.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uhdd.h"
#include "common/u/uutils.h"
#include "common/ugc.h"
#include "common/pping.h"

using namespace std;



/****************************************************************************
                         UGC functions
*****************************************************************************/

void clean_resources(gov_config_struct& cfg, bool background_off_from_background_on)
{
//gov global names already set
  gov_ugc(background_off_from_background_on, cfg.gov_vars.os_primitives_id_min_bound);

  string cfg_files_dir;

  for (int i=0; i<MAX_DBS_NUMBER; ++i)
  {
     if (cfg.db_vars[i].db_name[0] != '\0')
     {
        set_global_names(cfg.gov_vars.os_primitives_id_min_bound, i);
        sm_ugc(background_off_from_background_on, i, cfg.gov_vars.os_primitives_id_min_bound); 
     }
  }
}



bool is_first_start_of_gov(int ping_port)
{
  try
  {
    pping_client ppc(ping_port, EL_GOV);

    SednaUserException e = USER_EXCEPTION(SE4400);
    ppc.startup(e);
    ppc.shutdown();
    return false;

  } catch (...) {
    return true;//cannot connect to pping server
  }
}

void RenameLastSoftFaultDir(const char* SEDNA_DATA)
{

  std::string buf;
  std::string last_sf_dir;
   
  buf = SEDNA_DATA;

#ifdef _WIN32
  buf += "\\data";
  last_sf_dir = buf + std::string("\\") + SE_LAST_SOFT_FAULT_DIR;
#else
  buf += "/data";
  last_sf_dir = buf + std::string("/") + SE_LAST_SOFT_FAULT_DIR;
#endif




  if(uIsFileExist(last_sf_dir.c_str(), NULL))
  {
    char buf2[128];

#ifdef _WIN32
     //get Date of creation for this directory
    FILETIME ftCreate;
    SYSTEMTIME stUTC, stLocal;

    UFile hFile = uOpenFile(
                       last_sf_dir.c_str(),
                       U_SHARE_READ,
                       U_READ,
                       FILE_FLAG_BACKUP_SEMANTICS,
                       NULL 
                     );

    if ( hFile == U_INVALID_FD )
     throw USER_EXCEPTION2(SE4042, last_sf_dir.c_str());  


    // Retrieve the file times for the file.
    if (!GetFileTime(hFile, &ftCreate, NULL, NULL))
        throw USER_EXCEPTION(SE4410);

    // Convert the last-write time to local time.
    FileTimeToSystemTime(&ftCreate, &stUTC);
    SystemTimeToTzSpecificLocalTime(NULL, &stUTC, &stLocal);

    // Build a string showing the date and time.
    sprintf(buf2, "%02d-%02d-%d-%02d-%02d",
            stLocal.wDay, stLocal.wMonth, stLocal.wYear,
            stLocal.wHour, stLocal.wMinute);

    uCloseFile(hFile, NULL);

#else

    struct stat statbuf;
    struct tm      *tm;
    char   datestring[256];
   
    if (stat(last_sf_dir.c_str(), &statbuf) == -1)
       throw USER_EXCEPTION2(SE4042, last_sf_dir.c_str());  

    tm = localtime(&statbuf.st_mtime);

    /* Get localized date string. */
    //strftime(datestring, sizeof(datestring), nl_langinfo(D_T_FMT), tm);
    strftime(datestring, sizeof(datestring), "%02d-%02m-%Y-%02H-%02M", tm);

    sprintf(buf2, "%s", datestring);

   //get time of creation for this directory under Linux
#endif


#ifdef _WIN32
    std::string new_name = buf + std::string("\\") + string(SE_SOFT_FAULT_LOG_DIR) + buf2;
#else
    std::string new_name = buf + std::string("/") + string(SE_SOFT_FAULT_LOG_DIR) + buf2;
#endif

    if (uIsFileExist(new_name.c_str(), NULL))
    { //need to find free xxxxx_i  directory
       int i = 1;
       char val[128];
       for (;;)
       {
         if (!uIsFileExist((new_name + string(".")  + u_itoa(i, val, 10)).c_str(), NULL)) break;
        
         i++;
       }

       new_name += string(".")  + u_itoa(i, val, 10);
    }


    if(uMoveFile(last_sf_dir.c_str(),
                 new_name.c_str(),
                 NULL) == 0)
        throw USER_EXCEPTION(SE4410);


  }

  return;
}

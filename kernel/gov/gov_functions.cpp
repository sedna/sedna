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
#include "common/errdbg/d_printf.h"
#include "common/structures/config_data.h"
#include "u/uhdd.h"
#include "u/usystem.h"
#include "u/uutils.h"


/****************************************************************************
                         UGC functions
*****************************************************************************/
/*
void clean_resources(gov_config_struct& cfg, bool background_off_from_background_on)
{
//gov global names already set
  gov_ugc(background_off_from_background_on, cfg.gov_vars.os_primitives_id_min_bound);

  string cfg_files_dir;

  for (int i=0; i<MAX_DBS_NUMBER; ++i)
  {
     if (cfg.db_vars[i].db_name[0] != '\0')
     {
        SetGlobalNamesDB(i);
        sm_ugc(background_off_from_background_on, i, cfg.gov_vars.os_primitives_id_min_bound); 
     }
  }
}*/

void log_out_system_information()
{
    U_UTSNAME buf;
#ifdef SEDNA_X64
    const char* proc = "64bit";
#else
    const char* proc = "32bit";
#endif

#ifdef _ppc_
    const char* arch = " PPC";
#else
    const char* arch = "";
#endif

    elog(EL_INFO, ("SEDNA version is %s.%s (%s %s%s)", SEDNA_VERSION, SEDNA_BUILD, proc, ACTIVE_CONFIGURATION, arch));
    if(uUname(&buf, __sys_call_error))
        elog(EL_WARN, ("Can't get system information!"));
    else

    elog(EL_INFO, ("System: %s %s (%s) %s", buf.sysname, buf.release, buf.version, buf.machine));
}


void check_data_folder_existence(const char * dataDirectory)
{
    if (!uIsFileExist(dataDirectory, __sys_call_error))
    {
        if (uMkDir(dataDirectory, NULL, __sys_call_error) == 0)
            throw USER_EXCEPTION2(SE4300, dataDirectory);
    }
}

void RenameLastSoftFaultDir()
{

  std::string buf;
  std::string last_sf_dir;

  buf = SEDNA_DATA;

  last_sf_dir = buf + std::string(U_PATH_DELIMITER) + SE_LAST_SOFT_FAULT_DIR;

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
    struct tm tm;
    struct tm *tm_res;
  
    if (stat(last_sf_dir.c_str(), &statbuf) == -1)
       throw USER_EXCEPTION2(SE4042, last_sf_dir.c_str());  
    
    /* localtime_r is thread safe version of localtime */
    tm_res = localtime_r(&statbuf.st_mtime, &tm);

    /* Get localized date string. */
    if(tm_res != NULL)
        sprintf(buf2, "%02d-%02d-%04d-%02d-%02d",
                tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900,
                tm.tm_hour, tm.tm_min);
    else
        sprintf(buf2, "%s", "lt-error");
#endif


    std::string new_name = buf + std::string(U_PATH_DELIMITER) + std::string(SE_SOFT_FAULT_LOG_DIR) + buf2;

    if (uIsFileExist(new_name.c_str(), NULL))
    { //need to find free xxxxx_i  directory
       int i = 1;
       char val[128];
       for (;;)
       {
         if (!uIsFileExist((new_name + std::string(".")  + u_itoa(i, val, 10)).c_str(), NULL)) break;

         i++;
       }

       new_name += std::string(".")  + u_itoa(i, val, 10);
    }

    if(uMoveFile(last_sf_dir.c_str(),
                 new_name.c_str(),
                 NULL) == 0)
        throw USER_EXCEPTION(SE4410);
  }

  return;
}
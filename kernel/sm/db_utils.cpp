/*
 * File:  db_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include "base.h"
#include "uhdd.h"
#include "cdb_globals.h"
#include "db_utils.h"
#ifdef _WIN32
#include <io.h>
#else
#include <sys/types.h>
#include <dirent.h>
#endif



using namespace std;

//returns 0 if database not exist
//returns 1 if at least one file (or dir) was deleted
//returns 2 if database can't be deleted
int cleanup_db(const char* db_name)
{
   int res;
   bool db_exist = false;
   //delete cfg file

   if (uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str(), __sys_call_error))  
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str(), __sys_call_error);
      if (res == 0)//failure
         return 2;
   }
   

   //delete data file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str(), __sys_call_error)) 
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str(), __sys_call_error);
      if (res == 0)
         return 2;
      
   }


   //delete tmp file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str(), __sys_call_error))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str(), __sys_call_error);
      if (res == 0)
         return 2;
   }

   //delete llog file
   res = delete_logical_log(db_name);

   if (res == 2) return 2;
   if (res > 0) db_exist = true;


   //delete plog file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".plog").c_str(), __sys_call_error))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".plog").c_str(), __sys_call_error);
      if (res == 0)
         return 2;   
   }


   //delete ph.bu file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str(), __sys_call_error))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str(), __sys_call_error);
      if (res == 0)
         return 2;
   }


   //delete ph file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str(), __sys_call_error))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str(), __sys_call_error);
      if (res == 0)
         return 2;
   }


   //delete db data directory
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files").c_str(), __sys_call_error))
   {
     db_exist = true;
     res = uDelDir((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files").c_str(), __sys_call_error);
      if (res == 0)
         return 2;

   }

   if (!db_exist) return 0;
   else return 1;
}

bool exist_db(const char* db_name)
{
   bool res1, res2, res3, res4, res5, res6, res7;

   res1 = uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str(), __sys_call_error);

   res2 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str(), __sys_call_error);

   res3 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str(), __sys_call_error);

   res4 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".plog").c_str(), __sys_call_error);

   res5 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str(), __sys_call_error);

   res6 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str(), __sys_call_error);

   res7 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files").c_str(), __sys_call_error);


   if (res1 || res2 || res3 || res4 || res5 || res6 || res7) return true;
   else return false;
}


int load_metadata_in_database(const char* db_name)
{
#if (AUTH_SWITCH == 1)
  UPID pid;
  UPHANDLE proc_h;

  //!!! Run SM !!!
  string run_command;
  int ret_status;
  int res;
  char buf[U_MAX_PATH + SE_MAX_DB_NAME_LENGTH + 16];

  run_command = uGetImageProcPath(buf, __sys_call_error) + string("/se_sm ") + db_name; 
  strcpy(buf, run_command.c_str());

  if (0 != uCreateProcess(buf,
                          true, // inherit handles
                          NULL,
                          U_DETACHED_PROCESS,
                          &proc_h,
                          NULL,
                          &pid,
                          NULL,
                          NULL,
                          __sys_call_error
                         ))
     throw SYSTEM_EXCEPTION("Can't stratup SM to load metadata");

  res = uWaitForChildProcess(pid, proc_h, &ret_status, __sys_call_error);

  if (res == 0 && ret_status != 0)
      throw USER_EXCEPTION2(SE4211, (string(db_name) + string(" (may be because there is no enough RAM)")).c_str());
  else if (res != 0 || ret_status != 0)
  {
     throw SYSTEM_EXCEPTION("Can't startup SM to load metadata");
  }


   //!!! Load Security Document !!!  

  run_command = uGetImageProcPath(buf, __sys_call_error) + string("/") + SESSION_EXE +
                string(" ") + db_name;


  uSetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, "1", __sys_call_error);

  strcpy(buf, run_command.c_str());
  if (0 != uCreateProcess(buf,
                          true, // inherit handles
                          NULL,
                          U_DETACHED_PROCESS,
                          &proc_h,
                          NULL,
                          &pid,
                          NULL,
                          NULL,
                          __sys_call_error
                         ))
     throw SYSTEM_EXCEPTION("Can't load metadata");


  res = uWaitForChildProcess(pid, proc_h, &ret_status, __sys_call_error);

  if (res != 0)
     throw SYSTEM_EXCEPTION("Can't load metadata");

  int ret_code = 0;

  if (ret_status != 0)
     ret_code = 1;//database creation failure (may be because of concurrent transactions)


  //!!! Stop SM !!!
  run_command = uGetImageProcPath(buf, __sys_call_error) + string("/se_smsd ") + db_name; 
  strcpy(buf, run_command.c_str());

  if (0 != uCreateProcess(buf,
                          true, // inherit handles
                          NULL,
                          U_DETACHED_PROCESS,
                          &proc_h,
                          NULL,
                          &pid,
                          NULL,
                          NULL,
                          __sys_call_error
                         ))
     throw SYSTEM_EXCEPTION("Can't run smsd utility");

  res = uWaitForChildProcess(pid, proc_h, &ret_status, __sys_call_error);

  if (res != 0 || ret_status != 0)
     throw SYSTEM_EXCEPTION("Can't stop Storage Manager");

  return ret_code;
#else 
  return 0;
#endif
}

//returns 0 if logical log not exist
//returns 1 was deleted succesfully
//returns 2 if logical log can't be deleted

int delete_logical_log(const char* db_name)
{
#ifdef _WIN32

  char buf[4096];
  char *cur_dir;
  cur_dir  = uGetCurrentWorkingDirectory(buf, 4096, __sys_call_error);
  string path_to_db_files = string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/";

  if (uChangeWorkingDirectory(path_to_db_files.c_str(), __sys_call_error) != 0 )
     return 2;
  

  struct _finddata_t log_file;
  long dsc;

  if ( (dsc = _findfirst("*llog", &log_file)) == -1L)
  {
     if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
        return 2;

     return 0;
  }

  do 
  {
     if (uDeleteFile(log_file.name, __sys_call_error) == 0) 
     {
        uChangeWorkingDirectory(cur_dir, __sys_call_error);
        return 2;
     }

  } while(_findnext(dsc, &log_file) == 0);

    
  _findclose(dsc);

  if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
     return 2;

  return 1;
#else
  DIR *dir;
  struct dirent* dent;
  string path_to_db_files = string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/";

  dir = opendir(path_to_db_files.c_str());

  if (dir == NULL)
     return 2;

  dent = readdir (dir);
  if (dent == NULL) return 2; 

  string is_llog;
  bool find_log = false;  
  do 
  {
     is_llog =  dent->d_name);

     if ( is_llog.substr(is_llog.size()-4, 4) == "llog") 
     {
       find_log = true;
       if (uDeleteFile(dent->d_name, __sys_call_error) == 0) 
          return 2;
     }

  } while(NULL != (dent=readdir(dir)));

  if (0 != closedir(dir))
     return 2;

  if (!find_log) return 0;

  return 1;
#endif  

}
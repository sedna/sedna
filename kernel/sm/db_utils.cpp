/*
 * File:  db_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include "base.h"
#include "uhdd.h"
#include "cdb_globals.h"
#include "exceptions.h"

using namespace std;

//returns 0 if database not exist
//returns 1 if at least one file (or dir) was deleted
//returns 2 if database can't be deleted
int cleanup_db(const char* db_name)
{
   int res;
   bool db_exist = false;
   //delete cfg file

   if (uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str()))  
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str());
      if (res == 0)//failure
         return 2;
   }
   

   //delete data file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str())) 
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str());
      if (res == 0)
         return 2;
      
   }


   //delete tmp file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str()))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str());
      if (res == 0)
         return 2;
   }

   //delete llog file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".0llog").c_str()))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".0llog").c_str());
      if (res == 0)
         return 2;
   }


   //delete plog file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".plog").c_str()))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".plog").c_str());
      if (res == 0)
         return 2;   
   }


   //delete ph.bu file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str()))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str());
      if (res == 0)
         return 2;
   }


   //delete ph file
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str()))
   {
      db_exist = true;
      res = uDeleteFile((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str());
      if (res == 0)
         return 2;
   }


   //delete db data directory
   if (uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files").c_str()))
   {
     db_exist = true;
     res = uDelDir((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files").c_str());
      if (res == 0)
         return 2;

   }

   if (!db_exist) return 0;
   else return 1;
}

bool exist_db(const char* db_name)
{
   bool res1, res2, res3, res4, res5, res6, res7;

   res1 = uIsFileExist((string(SEDNA_DATA) + "/cfg/" + string(db_name) + "_cfg.xml").c_str());

   res2 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".data").c_str());

   res3 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".tmp").c_str());

   res4 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".llog").c_str());

   res5 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph").c_str());

   res6 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files/" + string(db_name) + ".ph.bu").c_str());

   res7 = uIsFileExist((string(SEDNA_DATA) + "/data/" + string(db_name) + "_files").c_str());


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

  run_command = uGetImageProcPath(buf) + string("/se_sm ") + db_name; 
  strcpy(buf, run_command.c_str());

  if (0 != uCreateProcess(buf,
                          true, // inherit handles
                          NULL,
                          U_NO_WINDOW,
                          &proc_h,
                          NULL,
                          &pid,
                          NULL,
                          NULL
                         ))
     throw SYSTEM_EXCEPTION("Can't stratup SM to load metadata");

  res = uWaitForChildProcess(pid, proc_h, &ret_status);

  if (res == 0 && ret_status != 0)
      throw USER_EXCEPTION2(SE4211, (string(db_name) + string(" (may be because there is no enough RAM)")).c_str());
  else if (res != 0 || ret_status != 0)
  {
     throw SYSTEM_EXCEPTION("Can't startup SM to load metadata");
  }


   //!!! Load Security Document !!!  

  run_command = uGetImageProcPath(buf) + string("/") + SESSION_EXE +
                string(" ") + db_name;


  uSetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, "1");

  strcpy(buf, run_command.c_str());
  if (0 != uCreateProcess(buf,
                          true, // inherit handles
                          NULL,
                          U_NO_WINDOW,
                          &proc_h,
                          NULL,
                          &pid,
                          NULL,
                          NULL
                         ))
     throw SYSTEM_EXCEPTION("Can't load metadata");


  res = uWaitForChildProcess(pid, proc_h, &ret_status);

  if (res != 0)
     throw SYSTEM_EXCEPTION("Can't load metadata");

  int ret_code = 0;

  if (ret_status != 0)
     ret_code = 1;//database creation failure (may be because of concurrent transactions)


  //!!! Stop SM !!!
  run_command = uGetImageProcPath(buf) + string("/se_smsd ") + db_name; 
  strcpy(buf, run_command.c_str());

  if (0 != uCreateProcess(buf,
                          true, // inherit handles
                          NULL,
                          U_NO_WINDOW,
                          &proc_h,
                          NULL,
                          &pid,
                          NULL,
                          NULL
                         ))
     throw SYSTEM_EXCEPTION("Can't run smsd utility");

  res = uWaitForChildProcess(pid, proc_h, &ret_status);

  if (res != 0 || ret_status != 0)
     throw SYSTEM_EXCEPTION("Can't stop Storage Manager");

  return ret_code;
#else 
  return 0;
#endif
}

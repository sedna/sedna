/*
 * File:  tr_debug.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "common/u/uhdd.h"
#include "common/base.h"
#include "common/u/usem.h"

using namespace std;

UFile debug_f; 
string debug_file_path;
USemaphore debug_sem;

//#define DEBUG_TR_ON

void INIT_DEBUG_LOG(const char* db_name)
{
#ifdef DEBUG_TR_ON

  debug_file_path = string(SEDNA_DATA) + "\\data\\" + string(db_name) + "_files\\" + string(db_name) + ".dlog";
  debug_f = uOpenFile(debug_file_path.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH);   

  if (debug_f == U_INVALID_FD)
     throw USER_EXCEPTION2(SE4042, debug_file_path.c_str());

  if (uSetFilePointer(debug_f, 0, NULL, U_FILE_END) == 0)
     throw USER_EXCEPTION2(SE4046, debug_file_path.c_str());

  if ( 0 != USemaphoreCreate(&debug_sem, 1, 1, CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION))
     throw USER_EXCEPTION2(SE4010, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");
#endif
}

void INIT_DEBUG_LOG2(const char* db_name)
{
#ifdef DEBUG_TR_ON

  debug_file_path = string(SEDNA_DATA) + "\\data\\" + string(db_name) + "_files\\" + string(db_name) + ".dlog";
  debug_f = uOpenFile(debug_file_path.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH);   

  if (debug_f == U_INVALID_FD)
     throw USER_EXCEPTION2(SE4042, debug_file_path.c_str());

  if (uSetFilePointer(debug_f, 0, NULL, U_FILE_END) == 0)
     throw USER_EXCEPTION2(SE4046, debug_file_path.c_str());

  if ( 0 != USemaphoreOpen(&debug_sem, CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION))
     throw USER_EXCEPTION2(SE4012, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");
#endif
}

void CREATE_DEBUG_LOG(const char* db_name)
{
#ifdef DEBUG_TR_ON
  debug_file_path = string(SEDNA_DATA) + "\\data\\" + string(db_name) + "_files\\" + string(db_name) + ".dlog";
  debug_f = uCreateFile(debug_file_path.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH);   
  if (debug_f == U_INVALID_FD)
     throw USER_EXCEPTION2(SE4040, debug_file_path.c_str()); 

  if ( 0 != USemaphoreCreate(&debug_sem, 1, 1, CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION))
     throw USER_EXCEPTION2(SE4010, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");
   
#endif
}

void RELEASE_DEBUG_LOG()
{
#ifdef DEBUG_TR_ON
   if (uCloseFile(debug_f) == 0)
      throw USER_EXCEPTION2(SE4043, debug_file_path.c_str());     

   if (USemaphoreRelease(debug_sem) != 0)
      throw USER_EXCEPTION2(SE4011, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");
#endif
}

void RELEASE_DEBUG_LOG2()
{
#ifdef DEBUG_TR_ON
   if (uCloseFile(debug_f) == 0)
      throw USER_EXCEPTION2(SE4043, debug_file_path.c_str());     

   if (USemaphoreClose(debug_sem) != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");
#endif
}

void WRITE_DEBUG_LOG(const char* msg)
{
#ifdef DEBUG_TR_ON
   int res;
   int written;
   if ( 0 != USemaphoreDown(debug_sem))
      throw USER_EXCEPTION2(SE4015, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");

   res = uWriteFile(debug_f, msg, strlen(msg), &written); 

   if ( 0 != USemaphoreUp(debug_sem))
      throw USER_EXCEPTION2(SE4014, "CHARISMA_TRANSACTIONS_DEBUG_LOG_PROTECTION");

   if (res == 0 || written != strlen(msg))
      throw USER_EXCEPTION2(SE4045, debug_file_path.c_str());
#endif
}



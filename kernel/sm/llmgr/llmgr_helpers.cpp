/*
 * File:  llmgr_helpers.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "base.h"
#include "llmgr_core.h"
#include "uhdd.h"
#include "exceptions.h"

#include <stdio.h>
#include <stdlib.h>


logical_log_file_head llmgr_core::read_log_file_header()
{
  int res;

  res = uSetFilePointer(
                   ll_file_dsc,
                   0,
                   NULL,
                   U_FILE_BEGIN
                 );

  if (res == 0)
     throw USER_EXCEPTION2(SE4046, "logical log file");

  logical_log_file_head file_head;
  int already_read;

  res = uReadFile(
              ll_file_dsc,
              &file_head,
              sizeof(logical_log_file_head),
              &already_read
             );

  if (res == 0 || sizeof(logical_log_file_head) != already_read)
     throw USER_EXCEPTION2(SE4044, "logical log file header");

  //for debug
  //char buf[50];
  //printf("::last_checkpoint_lsn=%s\n",  _i64toa(file_head.last_checkpoint_lsn, buf, 16));
  //printf("::last_commit_lsn=%s\n",  _i64toa(file_head.last_commit_lsn, buf, 16));
  //printf("::next_lsn=%s\n",  _i64toa(file_head.next_lsn, buf, 16));

  //end for debug


  return file_head;
}

void llmgr_core::flush_last_commit_lsn(LONG_LSN &commit_lsn)
{
  set_file_pointer(0);

  int res;
  int written;

  res = uWriteFile(ll_file_dsc,
                   &commit_lsn,
                   sizeof(LONG_LSN),
                   &written
                    );
  if (res == 0 || written != sizeof(LONG_LSN))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last commit lsn");
}

/*
void llmgr_core::flush_last_checkpoint_lsn(LONG_LSN &checkpoint_lsn)
{
  set_file_pointer(2*sizeof(LONG_LSN));

  int res;
  int written;

  res = uWriteFile(ll_file_dsc,
                   &checkpoint_lsn,
                   sizeof(LONG_LSN),
                   &written
                  );
  if (res == 0 || written != sizeof(LONG_LSN))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last checkpoint lsn");
}
*/

void llmgr_core::set_file_pointer(__int64 file_pos)
{
  int res;

  res = uSetFilePointer(
                    ll_file_dsc,
                    file_pos,
                    NULL,
                    U_FILE_BEGIN
                  );

  if (res == 0)
     throw SYSTEM_EXCEPTION("Can't set file pointer for logical log file");

}


const char* llmgr_core::get_record_from_disk(LONG_LSN& lsn)
{
  if (lsn == NULL_LSN) return NULL;

  set_file_pointer(lsn);
  
  int res;
  int bytes_read;

  res = uReadFile(
               ll_file_dsc,
               small_read_buf,
               sizeof(small_read_buf),
               &bytes_read
             );

  if (res == 0)
     throw USER_EXCEPTION2(SE4044, "logical log file");

  logical_log_head* log_head;

  log_head = (logical_log_head*)small_read_buf;
  int rec_len = log_head->body_len + sizeof(logical_log_head);

  if (rec_len > bytes_read && bytes_read < sizeof(small_read_buf))
     throw USER_EXCEPTION(SE4154);

  if (rec_len > sizeof(small_read_buf))
  {//not full record in buffer
     large_read_buf = new char[rec_len];
     set_file_pointer(lsn);
  
     res = uReadFile(
                 ll_file_dsc,
                 large_read_buf,
                 rec_len,
                 &bytes_read
             );  

     if( res == 0 || bytes_read != rec_len)
       throw USER_EXCEPTION2(SE4044, "logical log file");

     return large_read_buf;
  }
  else
     return small_read_buf;
}


int llmgr_core::get_record_length(const void* rec)
{
  if (rec == NULL) return 0;
  else return  sizeof(logical_log_head) + ((logical_log_head*)rec)->body_len;
}


const char* llmgr_core::get_record_from_shared_memory(int end_offs, int len)
{
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  char* mem_beg = (char*)shared_mem;

  if ((end_offs - sizeof(logical_log_sh_mem_head)) >= len)
     return mem_beg + end_offs - len;
  else
  {
    int second_portion = end_offs - sizeof(logical_log_sh_mem_head);
    int first_portion = len - second_portion;
    int rec_beg_offs =  mem_head->size - first_portion;


    if (len <= sizeof(small_read_buf) ) 
    {
       memcpy(small_read_buf, mem_beg + rec_beg_offs, first_portion);
       memcpy(small_read_buf + first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);
       return small_read_buf;
    }
    else
    {
       large_read_buf = new char[len];
       memcpy(large_read_buf, mem_beg + rec_beg_offs, first_portion);
       memcpy(large_read_buf + first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);
       return large_read_buf;
    }
  }
    
     
}

void llmgr_core::delete_large_read_buf()
{
   if (large_read_buf != NULL)
   {
      delete [] large_read_buf;
      large_read_buf = NULL;
   }
}
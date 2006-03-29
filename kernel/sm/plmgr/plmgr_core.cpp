/*
 * File:  plmgr_core.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "xptr.h"
#include "sm_vmm_data.h"
#include "bm_rcv.h"
#include "exceptions.h"
#include "plmgr_core.h"
#include "sm_globals.h"
#include "usem.h"
#include "d_printf.h"
#include "tr_debug.h"
#include <string>
#include <stdio.h>
#include "trmgr.h"

using namespace std;


int size_of_bytes_written_to_phys_log = 0;//for DEBUG
int pure_size = 0; //for DEBUG

/*****************************************************************************

            Start Up for Physical Recovery

******************************************************************************/

bool plmgr_core::create_phys_log(string DbFilesPath, int _phys_log_size_)
{
  int r;
  //init phys log protection semaphore
  r = USemaphoreCreate(&sem, 1, 1, PHYS_LOG_PROTECTION_SEMAPHORE_NAME, NULL);

  if ( r != 0)
  {
     throw USER_EXCEPTION2(SE4010, "PHYS_LOG_PROTECTION_SEMAPHORE_NAME");
  }

  if ( USemaphoreOpen(&wait_for_checkpoint_sem, CHARISMA_WAIT_FOR_CHECKPOINT) != 0) 
     throw USER_EXCEPTION2(SE4012, "CHARISMA_WAIT_FOR_CHECKPOINT");  

 
  string log_file_name;
  #ifndef PHYS_LOG_TEST 
  log_file_name = string(DbFilesPath) + string(db_name) + ".plog";
  #endif

  #ifdef PHYS_LOG_TEST
  log_file_name = "x.plog";
  #endif

  //init file handler 
  pl_file_handler = uOpenFile(
                         log_file_name.c_str(),
                         U_SHARE_READ | U_SHARE_WRITE,
                         U_READ_WRITE,
                         U_WRITE_THROUGH
                         );

  if ( pl_file_handler == U_INVALID_FD)
     throw USER_EXCEPTION2(SE4042, log_file_name.c_str());


  int res;
  
  res = uGetDiskSectorSize(&sector_size, DbFilesPath.c_str());  

  if (res == 0) 
     throw USER_EXCEPTION(SE4051);


  //init read_buf
  read_buf = new char[PHYS_LOG_READ_BUF_LENGTH];

  //init phys log header (it stored in the first sector of phys log file)
  int res3;

  res3 = uSetFilePointer(
                   pl_file_handler,
                   0,
                   NULL,
                   U_FILE_BEGIN);

  if ( res3 == 0 )
     throw USER_EXCEPTION2(SE4046, log_file_name.c_str());



  if ( PHYS_LOG_READ_BUF_LENGTH < sector_size )
     throw USER_EXCEPTION(SE4100);

  int nbytes_read;


  res3 = uReadFile(
               pl_file_handler,
               read_buf,
               sector_size,
               &nbytes_read);

  if ( res3 == 0 || nbytes_read != sector_size)
     throw USER_EXCEPTION2(SE4044, log_file_name.c_str());


  
  if (sizeof(file_head) > sector_size )
     throw USER_EXCEPTION(SE4101);
  
  pl_head_for_rcv = new file_head;

  memcpy(pl_head_for_rcv, read_buf, sizeof(file_head));
  bool _is_stopped_correctly_ = pl_head_for_rcv->is_stopped_successfully;

  pl_head_for_rcv->is_stopped_successfully = false;
  _writeFile(pl_head_for_rcv, sizeof(file_head), 0);

  log_file_size = _phys_log_size_;


  d_printf2("pl_head_fro_rcv.version=%d\n", pl_head_for_rcv->version);

  return _is_stopped_correctly_;  
}


void plmgr_core::open_phys_log(string db_phys_log_path, int _phys_log_size_)
{

  //open semaphore fpr protecting phys log
  if( USemaphoreOpen(&sem, PHYS_LOG_PROTECTION_SEMAPHORE_NAME) != 0)
     throw USER_EXCEPTION2(SE4012, "PHYS_LOG_PROTECTION_SEMAPHORE_NAME");

  //open wait for checkpoint sem
  if ( USemaphoreOpen(&wait_for_checkpoint_sem, CHARISMA_WAIT_FOR_CHECKPOINT) != 0) 
     throw USER_EXCEPTION2(SE4012, "CHARISMA_WAIT_FOR_CHECKPOINT");  

  //init file handler 
  pl_file_handler = uOpenFile(
                         db_phys_log_path.c_str(),
                         U_SHARE_READ | U_SHARE_WRITE,
                         U_READ_WRITE,
                         U_WRITE_THROUGH);

  if(pl_file_handler == U_INVALID_FD )
     throw USER_EXCEPTION2(SE4042, db_phys_log_path.c_str());


  int res;
  
  res = uGetDiskSectorSize(&sector_size, db_phys_log_path.c_str());  

  if (res == 0) 
     throw USER_EXCEPTION(SE4051);


  //init read_buf
  read_buf = new char[PHYS_LOG_READ_BUF_LENGTH];

  this->init_phys_log_open_state();

  log_file_size = _phys_log_size_; 

}

void plmgr_core::init_phys_log_open_state()
{
  pl_head_for_rcv = NULL;
}


/*****************************************************************************

                         Create Shared Memory (including initing)

******************************************************************************/


void plmgr_core::create_shared_mem(int ext_portion)
{
  //init file mapping handle
  int res;

  res = uCreateShMem(&file_mapping,
                     PHYS_LOG_SHARED_MEM_NAME,
                     sizeof(shared_mem_head)+sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM, NULL
                    );



  if ( res != 0 )
     throw USER_EXCEPTION2(SE4016, "PHYS_LOG_SHARED_MEM_NAME");

  //init shared memory pointer
  share_mem = uAttachShMem(file_mapping,
                           NULL,
                           sizeof(shared_mem_head)+sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM
                          );
  

  if (share_mem == NULL)
     throw USER_EXCEPTION2(SE4023, "PHYS_LOG_SHARED_MEM_NAME");

  //init header of the shared memory
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  mem_head->end_offs = sizeof(shared_mem_head);
  mem_head->begin_not_drbl_offs = sizeof(shared_mem_head);
  mem_head->free_bytes = sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM;
  mem_head->keep_bytes = 0;
  mem_head->size = sizeof(shared_mem_head)+sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM;
  mem_head->sect_free_bytes = 0;
  mem_head->next_lsn = pl_head_for_rcv->next_lsn;
  mem_head->prev_lsn = NULL_LSN;
  mem_head->durable_lsn = pl_head_for_rcv->prev_lsn;
  mem_head->next_durable_lsn = pl_head_for_rcv->next_lsn;
  mem_head->pl_head = *pl_head_for_rcv;
  mem_head->extend_portion_size = ext_portion;
  mem_head->checkpoint_flag = false;
  mem_head->checkpoint_on = false;
  mem_head->num_of_records_after_cp = 0;
  mem_head->num_of_records = 0;

  for (int i = 0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
     mem_head->empty_bulk_load_blk[i] = '0';//

  d_printf2("create_shared_mem next_durable_lsn=%d\n", mem_head->next_durable_lsn);

}


/*****************************************************************************

                        Open Shared Memory

******************************************************************************/

void plmgr_core::open_shared_mem()
{
  //open file mapping
  int res;

  res = uOpenShMem(&file_mapping,
                   PHYS_LOG_SHARED_MEM_NAME,
                   sizeof(shared_mem_head)+sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM
                  );


  if ( res != 0)
     throw USER_EXCEPTION2(SE4021, "PHYS_LOG_SHARED_MEM_NAME");

  //init shared_memory pointer
  share_mem = uAttachShMem(file_mapping,
                           NULL,
                           sizeof(shared_mem_head)+sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM
                          );


  if (share_mem == NULL)
     throw USER_EXCEPTION2(SE4023, "PHYS_LOG_SHARED_MEM_NAME");
  
  this->init_shared_mem_open_state();
}

void plmgr_core::init_shared_mem_open_state()
{
}


/*****************************************************************************

                         Close Shared Memory

******************************************************************************/

void plmgr_core::close_shared_mem()
{
  this->close_shared_mem_state();

  int res;

  res = uDettachShMem(file_mapping, share_mem);

  if (res != 0)
     throw USER_EXCEPTION2(SE4024, "PHYS_LOG_SHARED_MEM_NAME");

  res = uCloseShMem(file_mapping);

  if ( res != 0 )
     throw USER_EXCEPTION2(SE4022, "PHYS_LOG_SHARED_MEM_NAME");

}

void plmgr_core::close_shared_mem_state()
{
}



/*****************************************************************************

                         Close Physical Log

******************************************************************************/


void plmgr_core::close_phys_log()
{

  int res2;
  int res1;

  this->close_phys_log_state();

  res2 = USemaphoreClose(sem);

  if ( res2 != 0 )
     throw USER_EXCEPTION2(SE4013, "PHYS_LOG_PROTECTION_SEMAPHORE_NAME");

  if ( 0 != USemaphoreClose(wait_for_checkpoint_sem))
     throw USER_EXCEPTION2(SE4013, "CHARISMA_WAIT_FOR_CHECKPOINT");

  res1 = uCloseFile(pl_file_handler);

  if ( res1 == 0 )
     throw USER_EXCEPTION2(SE4043, "physical log file");

  if( pl_head_for_rcv != NULL)
    delete pl_head_for_rcv;

  delete [] read_buf;

}

void plmgr_core::close_phys_log_state()
{
}


void plmgr_core::release_phys_log(file_head& head)
{
  int res2;
  int res1;

  head.is_stopped_successfully = true;
  _writeFile(&head, sizeof(file_head), 0);


  res2 = USemaphoreRelease(sem);

  if ( res2 != 0 )
     throw USER_EXCEPTION2(SE4011, "PHYS_LOG_PROTECTION_SEMAPHORE_NAME");

  if ( 0 != USemaphoreClose(wait_for_checkpoint_sem))
     throw USER_EXCEPTION2(SE4013, "CHARISMA_WAIT_FOR_CHECKPOINT");


  res1 = uCloseFile(pl_file_handler);

  if ( res1 == 0 )
     throw USER_EXCEPTION2(SE4043, "physical log file");

  if( pl_head_for_rcv != NULL)
    delete pl_head_for_rcv;

  delete [] read_buf;

}

void plmgr_core::release_shared_mem()
{
  int res;

  res = uDettachShMem(file_mapping, share_mem);


  if (res != 0)
     throw USER_EXCEPTION2(SE4024, "PHYS_LOG_SHARED_MEM_NAME");

  res = uReleaseShMem(file_mapping);

  if ( res != 0 )
     throw USER_EXCEPTION2(SE4020, "PHYS_LOG_SHARED_MEM_NAME");

}

/*****************************************************************************
                         Checkpoint enabling function
******************************************************************************/
void plmgr_core::set_checkpoint_flag(bool flag, bool sync)
{
  DownSemaphore(sync);

  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  mem_head->checkpoint_flag = flag;
  UpSemaphore(sync);
  
}

bool plmgr_core::get_checkpoint_flag(bool sync)
{
  bool flag;
  DownSemaphore(sync);

  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  flag = mem_head->checkpoint_flag;
  UpSemaphore(sync);

  return flag;
}

/******************************************************************************

               Insert new record to the log (changes the lsn of the page)

******************************************************************************/

void plmgr_core::logInsert(const void* p, int size, __int8 op, bool sync)
{

  pure_size += size;
//  d_printf2("pure log len=%d\n", pure_size);
//  d_printf2("size in LogInsert=%d\n", size);


  DownSemaphore(sync);

  int pure_log_len = size + sizeof(phys_log_head);
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  //compute durable log len
  int additional_bytes;

  if (mem_head->sect_free_bytes > pure_log_len)
	  additional_bytes = 0;
  else
	  additional_bytes = 
               ((pure_log_len - mem_head->sect_free_bytes)/(sector_size-sizeof(sector_head)))*sizeof(sector_head) +
                (((pure_log_len - mem_head->sect_free_bytes)%(sector_size-sizeof(sector_head)))>0 ? sizeof(sector_head):0);

  int drbl_log_len = pure_log_len + additional_bytes;


///////////// FOR DEBUG //////////////////
  size_of_bytes_written_to_phys_log += drbl_log_len;

//  d_printf2("writeen to log bytes=%d\n", size_of_bytes_written_to_phys_log);
////////////////////////////////////////

  if ( drbl_log_len >= mem_head->free_bytes)
  {
    logFlush(false);
	if (drbl_log_len > mem_head->free_bytes)
		throw SYSTEM_EXCEPTION("Too long record to be inserted into phys log (phys log shared memory should be more than one data block size)");
  }

  //fill up log_head
  phys_log_head log_head;
  log_head.lsn = mem_head->next_lsn;
  log_head.prev_lsn = mem_head->prev_lsn;
  log_head.op = op;
  #ifndef PHYS_LOG_TEST
  if ( op == PL_CHANGE || op == PL_CREATE_NODE_BLK)
  {
     log_head.p = this->addr2xptr(p);
     this->get_block_hdr(p)->lsn = log_head.lsn;      
  }
  else
     log_head.p = XNULL;
  #endif
  log_head.len = size;
  log_head.drbl_len = drbl_log_len;


  sector_head old_sector_head;
  sector_head new_sector_head;

  old_sector_head.durable_lsn = mem_head->prev_lsn;
  old_sector_head.version = mem_head->pl_head.version;

  new_sector_head.durable_lsn = mem_head->next_lsn;
  new_sector_head.version = mem_head->pl_head.version;

  //write log head to the shared memory
  writeSharedMemory(&log_head, sizeof(phys_log_head), old_sector_head, old_sector_head);

  writeSharedMemory(p, size, old_sector_head, new_sector_head);

  mem_head->prev_lsn = mem_head->next_lsn;
  mem_head->next_lsn += drbl_log_len;


  int debug = mem_head->next_lsn - mem_head->next_durable_lsn;
  mem_head->num_of_records_after_cp +=1;
    mem_head->num_of_records +=1;

  //d_printf2("Inserted phys log record lsn=%d\n", mem_head->prev_lsn);
  //d_printf4("num of records after cp =%d, total num=%d, last_lsn=%d\n", mem_head->num_of_records_after_cp,  mem_head->num_of_records, log_head.lsn);
  
  //d_printf1("logInsert\n");
  
  UpSemaphore(sync);
}


/*****************************************************************************

                          Flush log Records 

******************************************************************************/

//flushes all records will lsn less or equal input param 
void plmgr_core::logFlushPortionOfRecords(LSN& lsn, bool sync)
{

  //get the semaphore
  DownSemaphore(sync);

  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  char *beg_ndrbl_sect = (char*)share_mem + mem_head->begin_not_drbl_offs;//pointer to the first byte of the log record  to be flushed
  int beg_ndrbl_sect_offs = mem_head->begin_not_drbl_offs;

  if (beg_ndrbl_sect_offs == mem_head->end_offs)
  {
    UpSemaphore(sync);
    return;//OK all data are already flushed
  }

  if (mem_head->durable_lsn >= lsn)//this record is written on hard disk already
  {
    UpSemaphore(sync);
    return;
  }

  // !!! determine the length of shared memory to be flushed
  LSN curr_lsn = NULL_LSN;
  phys_log_head log_head;
  int mem_offs = mem_head->begin_not_drbl_offs;
  int drbl_len = 0;

  //there is minimum one log record in Shared memory
  while (curr_lsn < lsn)
  {
     readPureDataFromSharedMem(mem_offs, &log_head, sizeof(phys_log_head));


     drbl_len += log_head.drbl_len;

     curr_lsn = log_head.lsn;
     if ((mem_offs + log_head.drbl_len) < mem_head->size )
        mem_offs += log_head.drbl_len;
     else
        mem_offs = sizeof (shared_mem_head) + 
                   log_head.drbl_len - (mem_head->size - mem_offs);

     if (log_head.lsn >= lsn) break;
  }

  int add_drbl_len;
  //add to drbl_len the remainder of last sector. Last sector must be flushed COMPLETELEY
  //because header of last sector may contain durable lsn great then lsn to be flushed
  add_drbl_len = drbl_len + (sector_size - (log_head.lsn + log_head.drbl_len)%sector_size);


  int last_lsn_drbl_len = log_head.drbl_len;

  //d_printf2("last durable_lsn=%d\n", log_head.lsn);

  //!!! check for debug !!!
  if (drbl_len > mem_head->keep_bytes )
     throw SYSTEM_EXCEPTION(string("unpredictable lenegth of records to be flushed in phys log") + "drbl_len=" + int2string(drbl_len) + " keep_bytes="+ int2string(mem_head->keep_bytes));

  //for debug
  //string str = string("logFlushPortionOf Records before possible extend free_size=") + int2string(getLogFileSize(false) - getPhysLogSize(false)) +"\n";
  //WRITE_DEBUG_LOG(str.c_str());
   
  //end for debug

  //extend Log File if it is needed
  int free_size;
//  if ((free_size = getLogFileSize(false) - getPhysLogSize(false)) < (0.33 * getLogFileSize(false)))
  if ((free_size = getLogFileSize(false) - getPhysLogSize(false)) < drbl_len)
  {//need the checkpoint

#ifdef CHECKPOINT_ON
     if (drbl_len > mem_head->extend_portion_size)
     {
        d_printf3("drbl_len=%d, ext_portion_size=%d\n", drbl_len, mem_head->extend_portion_size);
        throw SYSTEM_EXCEPTION("extended portion size too small to write phys log buffer: try to encrease the -phys-log-ext-portion parameter in cdb utility");
     }

     //d_printf2("free_size=%d\n", free_size);
     if (mem_head->checkpoint_flag == true && !(mem_head->checkpoint_on))
     {
        if (USemaphoreUp(wait_for_checkpoint_sem) != 0)
           throw SYSTEM_EXCEPTION("Can't up checkpoint semaphore");

        mem_head->checkpoint_on = true;
     }
#endif
//     if ( free_size < drbl_len)
//     {//need to extend phys log file since there is not enough space in phys log to write shared mememory on disk
        extendLog(false);
//     }
  }


  
  // !!! flush shared mem (drbl_len) on disk
  int rmndr_len = add_drbl_len;
  char* beg_ndrbl_mem = (char*)share_mem + mem_head->begin_not_drbl_offs;
  mem_offs = mem_head->begin_not_drbl_offs;
  
  int portion = _min3(PHYS_LOG_FLUSH_PORTION, rmndr_len, mem_head->size - mem_offs);
  int file_pos = mem_head->next_durable_lsn - mem_head->pl_head.next_lsn + sector_size;

  //flush sector_head of the first sector
  _writeFile((char*)share_mem + mem_offs- (mem_offs - sizeof(shared_mem_head))%sector_size,
	         sizeof(sector_head),
			 file_pos - (file_pos%sector_size));

  //flush all rmndr_len (it may also include sector head of the first sector)
  while ( rmndr_len >0 )
  {
     _writeFile(beg_ndrbl_mem, portion, file_pos);
     
      file_pos += portion;
      rmndr_len -= portion;
      
      if ( (mem_offs + portion) > mem_head->size )
           throw SYSTEM_EXCEPTION("Unpredictable portion of data to be flushed in phys log");

      if ( (mem_offs + portion) == mem_head->size )
           mem_offs = sizeof(shared_mem_head);
      else
           mem_offs += portion;

      beg_ndrbl_mem = (char*)share_mem + mem_offs;

      portion = _min3(PHYS_LOG_FLUSH_PORTION, rmndr_len, mem_head->size - mem_offs);
  }


  //reassign shared memory header
  if ((mem_head->size - mem_head->begin_not_drbl_offs) > drbl_len)
      mem_head->begin_not_drbl_offs +=drbl_len;
  else
      mem_head->begin_not_drbl_offs =
                   sizeof(shared_mem_head)+
                   (drbl_len - (mem_head->size - mem_head->begin_not_drbl_offs));

  mem_head->free_bytes += drbl_len;
  mem_head->keep_bytes -= drbl_len;


  if (mem_head->durable_lsn == NULL_LSN)
  {
    mem_head->durable_lsn = sector_size;
    mem_head->next_durable_lsn = mem_head->durable_lsn + drbl_len;
  }
  else
  {
	mem_head->durable_lsn = lsn;
    mem_head->next_durable_lsn = mem_head->durable_lsn + last_lsn_drbl_len;

  }

  //d_printf2("Phys log flush, last durable lsn=%d\n", mem_head->durable_lsn);

  //give the semaphore
  UpSemaphore(sync);
}



/******************************************************************************

                    Flush all log records until lsn defined as first param

*******************************************************************************/


/******************************************************************************

                         Flush All Log records from shared memory

******************************************************************************/

void plmgr_core::logFlush(bool sync)//flushes the whole phys log
{
  
  DownSemaphore(sync);
 
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  LSN last_lsn = mem_head->prev_lsn;
  
  //logFlush(last_lsn, false);
  logFlushPortionOfRecords(last_lsn, false);

  UpSemaphore(sync);

}


/******************************************************************************

                 Clear Phys Log File

******************************************************************************/


//before calling this function all log must be flushed on disk
void plmgr_core::logClear(const LONG_LSN& last_cp_lsn, bool sync)
{

  DownSemaphore(sync);

  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  (mem_head->pl_head.version)++;
  //!!!Note: need check that version is not overflowed !!!

  
  mem_head->pl_head.prev_lsn = mem_head->prev_lsn;
  //mem_head->pl_head.next_lsn = mem_head->next_lsn + sector_size;
  mem_head->pl_head.next_lsn = mem_head->next_lsn + mem_head->sect_free_bytes;
  mem_head->pl_head.ph_bu_to_ph = false;
  mem_head->pl_head.last_checkpoint_lsn = last_cp_lsn;
  (mem_head->pl_head.cp_num)++;

  d_printf3("log_clear pl_head->prev_lsn=%d, next_lsn=%d\n", mem_head->pl_head.prev_lsn, mem_head->pl_head.next_lsn);
  d_printf2("sect_free_bytes=%d\n", mem_head->sect_free_bytes);

  //flush phys log header
  d_printf2("FLUSH pl_head.version=%d", mem_head->pl_head.version);
  _writeFile(&(mem_head->pl_head), sizeof(file_head), 0);



  //compress the phys log size to the initial value (phys_log_size)
  int res;
  res = uSetFilePointer(
                    pl_file_handler,
                    log_file_size,
                    NULL,
                    U_FILE_BEGIN  
                   );

  if (res == 0)
     throw SYSTEM_EXCEPTION("Can't Set File Pointer to the begin of file");

  res = uSetEndOfFile(pl_file_handler, 0, U_FILE_CURRENT);

  if (res == 0)
      throw USER_EXCEPTION2(SE4047, "physical log");


  //reinit memory head
  //mem_head->begin_offs = sizeof(shared_mem_head);
  mem_head->end_offs = sizeof(shared_mem_head);
  mem_head->begin_not_drbl_offs = sizeof(shared_mem_head);
  mem_head->free_bytes = sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM;
  mem_head->keep_bytes = 0;
  mem_head->size = sizeof(shared_mem_head)+sector_size*PHYS_LOG_SHARED_MEM_SECTORS_NUM;
  mem_head->sect_free_bytes = 0;
  mem_head->next_lsn = mem_head->pl_head.next_lsn;
  mem_head->prev_lsn = NULL_LSN;

  mem_head->durable_lsn = mem_head->pl_head.prev_lsn;
  mem_head->next_durable_lsn = mem_head->pl_head.next_lsn;
  d_printf2("mem_head->durable_lsn=%d\n", mem_head->durable_lsn);
  d_printf2("mem_head->next_durable_lsn=%d\n", mem_head->next_durable_lsn);

  mem_head->num_of_records_after_cp = 0;  
  UpSemaphore(sync);
}


/*****************************************************************************

                !!!!  Extend Phys log File  !!!!

******************************************************************************/



void plmgr_core::extendLog(bool sync)
{

  //get semaphore
  DownSemaphore(sync);

  //get file size
  int res;
  __int64 file_size;

  res = uGetFileSize(pl_file_handler, &file_size);

  if (res == 0)
     throw SYSTEM_EXCEPTION("Can't get the phys log size");

  int pos_eof = ((int)(file_size/sector_size))*sector_size + sector_size;

  res = uSetFilePointer(
                    pl_file_handler,
                    pos_eof,
                    NULL,
                    U_FILE_BEGIN  
                   );

  if (res == 0)
     throw SYSTEM_EXCEPTION("Can't Set File Pointer to the end of the file");


//  d_printf2("extendLog file_size=%d\n", file_size);

  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  int ver = mem_head->pl_head.version;
#ifdef TEST_CHECKPOINT_ON
  char buf[512];
#else
  char *buf;//1MB
  buf = new char[0x100000];//1MB

  
#endif

  int offs = 0;



#ifdef TEST_CHECKPOINT_ON
  ((sector_head*)(buf+offs))->durable_lsn = NULL_LSN;
  ((sector_head*)(buf+offs))->version = ver - 1;

  //write extend portion to file (by 1 MB for 1 cycle iteration)
  int nbytes_written;

  res = uWriteFile(
               pl_file_handler,
               buf,
               sizeof(buf),
               &nbytes_written                 
              );

  if (res == 0 || nbytes_written != sizeof(buf))
     throw SYSTEM_EXCEPTION("Can't extend phys log file");

#else


  int nbytes_written;
  
  while( offs < 0x100000)
  {
    ((sector_head*)(buf+offs))->durable_lsn = NULL_LSN;
    ((sector_head*)(buf+offs))->version = ver - 1;
   
    offs += sector_size;
  }


  //write n times buf to phys log. n is equal to number of Mb of phys log  
  for(int i=0; i< (mem_head->extend_portion_size)/0x100000; i++)
  {
    res = uWriteFile(
                 pl_file_handler,
                 buf,
                 0x100000,
                 &nbytes_written                 
                );

    if (res == 0 || nbytes_written != 0x100000)
      throw SYSTEM_EXCEPTION("Can't write to phys log file");

  }

  delete [] buf;

#endif

  res = uSetEndOfFile(pl_file_handler, 0, U_FILE_CURRENT);

  if (res == 0)
      throw USER_EXCEPTION2(SE4047, "physical log");

//  d_printf1("extendLog file completed\n");
  //release semaphore
  UpSemaphore(sync);    

}


void plmgr_core::set_checkpoint_on_flag(bool flag)
{
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;
  mem_head->checkpoint_on = flag;
}

void plmgr_core::set_empty_bulk_load_blk(int tr_id, char value)
{
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;
  mem_head->empty_bulk_load_blk[tr_id] = value;

}

char plmgr_core::get_bulk_load_blk(int tr_id)
{
  return ((shared_mem_head*)share_mem)->empty_bulk_load_blk[tr_id];
}


int plmgr_core::get_cp_num()
{
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  return (mem_head->pl_head).cp_num;
}

bool plmgr_core::is_stopped_correctly()
{
   return pl_head_for_rcv->is_stopped_successfully;
}

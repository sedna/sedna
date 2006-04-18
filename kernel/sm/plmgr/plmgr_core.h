/*
 * File:  plmgr_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PLMGR_CORE_H
#define _PLMGR_CORE_H

#include "base.h"
#include "xptr.h"
#include "usem.h"
#include "ushm.h"
#include "uhdd.h"
//#include "trmgr.h"
#include "sm_vmm_data.h"
#include <string>

enum  {PL_CHANGE, PL_CHANGE_MASTER, PL_DECREASE, PL_CREATE_NODE_BLK};


//#define PHYS_LOG_TEST
//must be uncommented for recovery
#define PHYS_LOG

#define  PHYS_LOG_READ_BUF_LENGTH 131072 
//128kb
#define  PHYS_LOG_FLUSH_PORTION 128*1024 
// 128 Kb
#define  PHYS_LOG_SHARED_MEM_SECTORS_NUM  256
//128Kb (128Kb is minimum)

//phys log record consists of two parts
// the first one is header (defined by phys_log_head struct)
// the second one is body (data). 
struct phys_log_head
{
  LSN lsn; //log record sequence number
  LSN prev_lsn; //log sequence number of the previous record
  __int8 op; //type of logged operation
  xptr p; //address of the body in data file
  unsigned short len; //length of the body
  int drbl_len;//length of the log record in the file (more than len because of sector head and phys log header)
};



struct sector_head
{
  LSN durable_lsn;
  __int8 version;
};

//file head struct define the header of physical log file
struct file_head
{
  int version;//the current version of log records (used during recovery)
  //these LSN are needed for recovery purposes
  LSN next_lsn;//next lsn after checkpoint
  LSN prev_lsn;//last lsn before checkpoint
  bool ph_bu_to_ph;//used to recovery persistent head (details see in bm_rcv.cpp)
  LONG_LSN last_checkpoint_lsn;//lsn of last stable checkpoint record in logical log
  int cp_num;//indicates number of checkpoints
  bool is_stopped_successfully; //indicates whether database need recovery (used during sm startup)

  file_head &operator= (file_head &fh)
  { 
    version = fh.version;
    next_lsn = fh.next_lsn;
    prev_lsn = fh.prev_lsn;
    ph_bu_to_ph = fh.ph_bu_to_ph;
    last_checkpoint_lsn = fh.last_checkpoint_lsn;
    cp_num = fh.cp_num;
    is_stopped_successfully = fh.is_stopped_successfully;
    return *this;
  };
};



struct shared_mem_head
{
  //int begin_offs; //the begin byte (offset) of the log in buffer
  int end_offs; //the end byte (offset) of the log in buffer
  int begin_not_drbl_offs; //the begin byte (offset) of the not flushed buffer
  int free_bytes; //the number of free bytes in shared memory
  int keep_bytes; //the number of busy bytes (excluding header)
  int size; //the size of shared memory (including header)
  int sect_free_bytes;// the number of free bytes in last written sector
  LSN next_lsn;//lsn of next record
  LSN prev_lsn;//lsn of the last record
  LSN durable_lsn; //max lsn recorded in durable storage
  LSN next_durable_lsn; //next lsn to be recorded in durable storage
  file_head pl_head;//pointer to the phys log header
  int extend_portion_size; //the size (in MB) of extended portion
  bool checkpoint_flag;//true->enable checkpoint, false ->disable checkpoint (used during recovery)
  bool checkpoint_on; //true means that checkpoint thread is working now
  char empty_bulk_load_blk[CHARISMA_MAX_TRNS_NUMBER];//cell is 1 -> transaction must empty a list of blocks which were created (used for checkpoint correctness) 
  int num_of_records_after_cp; //used for debug
  int num_of_records;//total number of records written in phys log (used for debug)
};


class plmgr_core {

protected:
  UFile pl_file_handler;

  file_head* pl_head_for_rcv; //pointer to the header of physical log file (used only during recovery)
  int sector_size; //the size of the sector of hard disk
  char* read_buf; //buffer for reading log records during rollback or recovery (more than maximum log record size)

  USemaphore sem;//semaphore which protects the log buffer (shared memory)
  USemaphore wait_for_checkpoint_sem;//semaphore for initing checkpoint
  UShMem file_mapping;//share memory
  void* share_mem;
  int log_file_size;


public:
  bool create_phys_log(std::string db_files_path, int _log_file_size_);
  void open_phys_log(std::string db_phys_log_path, int _log_file_size_ = 100);
  void init_phys_log_open_state();
  void create_shared_mem(int ext_portion);
  void open_shared_mem();
  void init_shared_mem_open_state();
  void close_phys_log();
  void close_phys_log_state();
  void close_shared_mem();
  void close_shared_mem_state();
  void release_phys_log(file_head& head);
  void release_shared_mem();

  void activate_checkpoint(bool sync);

  void set_checkpoint_flag(bool flag, bool sync = true);
  bool get_checkpoint_flag(bool sync = true);
  int get_cp_num();//returns checkpoint's number (used to check whether we need to write blocks' part in phys log)
  bool is_stopped_correctly();

  void logInsert(const void* p, int size, __int8 op, bool sync = true);
  void logClear(const LONG_LSN& last_cp_lsn, bool sync = true);
  void logFlush(bool sync = true);
  void logFlushPortionOfRecords(LSN& lsn, bool sync = true);
  int  getLogFileSize(bool sync = true);
  int  getPhysLogSize(bool sync = true);
  void extendLog(bool sync = true);
  void DownSemaphore(bool sync);
  void UpSemaphore(bool sync);
  void set_checkpoint_on_flag(bool flag);
  void set_empty_bulk_load_blk(int tr_id, char value); //value should be '0' or '1' (tr must empty tr table)
  char get_bulk_load_blk(int tr_id);
  void set_ph_bu_to_ph(bool ph_bu_to_ph, bool sync = true);
  file_head get_file_head(bool sync = true);

protected://helpers

  void writeSector(void* p, int size, int file_pos, LSN& drbl_lsn);//writes one sector on the disk
  void _writeFile(void* p, int size, int file_pos);
  void readSector(void* p, int size);//read one sector from disk
  void readLogRecordFromDisk(char* buf, LSN& lsn);//read into buffer the whole log record (excludes sector_head)
  void readPureDataFromSharedMem(int source_offs, void* dest, int source_len);//read into dest buffer log data without sector_head
  void writeSharedMemory(const void* p, int size, sector_head& old_sect_h, sector_head& new_sect_h);//write data to the shared memory (inserts sector heads and fills them, reinits share memory head)
  int _min2(int x1, int x2);
  int _min3(int x1, int x2, int x3);
  virtual xptr addr2xptr(const void *addr) = 0;
  virtual vmm_sm_blk_hdr* get_block_hdr(const void *addr) = 0;
};

inline int plmgr_core::_min2 (int x1, int x2)
{
  if (x1<=x2) return x1;
  else return x2;
}


inline int plmgr_core::_min3 (int x1, int x2, int x3)
{
  return _min2(_min2 (x1, x2), x3);
}

#endif

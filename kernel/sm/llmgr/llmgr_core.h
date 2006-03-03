/*
 * File:  llmgr_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LLMGR_CORE_H
#define _LLMGR_CORE_H

#include <string>
#include <vector>
#include <map>

#include "base.h"
//#include "nodes.h"
#include "xptr.h"
#include "uhdd.h"
#include "usem.h"
#include "ushm.h"

#define LOGICAL_LOG
//#define LOGICAL_LOG_TEST
#define NULL_OFFS (-1)
#define LOGICAL_LOG_FLUSH_PORTION (128*1024) // 128 Kb
#define CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE (1024*1024) //10Mb
#define LOGICAL_LOG_UNDO_READ_PORTION 1024 //1Kb

enum {LL_INSERT_ELEM,
      LL_INDIR_INSERT_ELEM,
      LL_DELETE_ELEM,
      LL_INDIR_DELETE_ELEM,
      LL_INSERT_ATTR,
      LL_INDIR_INSERT_ATTR,
      LL_DELETE_ATTR,
      LL_INDIR_DELETE_ATTR,
      LL_INSERT_TEXT,
      LL_INDIR_INSERT_TEXT,
      LL_DELETE_TEXT,
      LL_INDIR_DELETE_TEXT,
      LL_INSERT_LEFT_TEXT,
      LL_DELETE_LEFT_TEXT,
      LL_INSERT_RIGHT_TEXT,
      LL_DELETE_RIGHT_TEXT,
      LL_INSERT_DOC,
      LL_INDIR_INSERT_DOC,
      LL_DELETE_DOC,
      LL_INDIR_DELETE_DOC,
      LL_INSERT_COMMENT,
      LL_INDIR_INSERT_COMMENT,
      LL_DELETE_COMMENT,
      LL_INDIR_DELETE_COMMENT,
      LL_INSERT_PI,
      LL_INDIR_INSERT_PI,
      LL_DELETE_PI,
      LL_INDIR_DELETE_PI,
      LL_INSERT_COLLECTION,
      LL_INDIR_INSERT_COLLECTION,
      LL_DELETE_COLLECTION,
      LL_INDIR_DELETE_COLLECTION,
      LL_INSERT_NS,
      LL_INDIR_INSERT_NS,
      LL_DELETE_NS,
      LL_INDIR_DELETE_NS,
      LL_INSERT_DOC_INDEX,
      LL_DELETE_DOC_INDEX,
      LL_INSERT_COL_INDEX,
      LL_DELETE_COL_INDEX,
      LL_COMMIT,
      LL_ROLLBACK,
      LL_CHECKPOINT
     };

struct logical_log_head
{
  int prev_trn_offs;
  int body_len;
};

struct logical_log_file_head
{
  LONG_LSN last_commit_lsn;//last record of the last committed trn
  LONG_LSN next_lsn; //lsn of the next record after last_commit
//  LONG_LSN last_checkpoint_lsn; //lsn of the last checkpoint record

  //new gield must be appended in the end of structure
};


struct trn_cell
{
  LONG_LSN last_lsn;
  int last_rec_mem_offs;//NULL_OFFS -> last record on disk
  bool is_ended; //true if commit or rollback record has been added in the logical log of the transaction
  int num_of_log_records; //used for debug
};


typedef trn_cell trn_tbl[CHARISMA_MAX_TRNS_NUMBER];


struct trn_cell_analysis
{
  char type;//0 -> undo, 1 -> redo
  LONG_LSN lsn;//record from which undo (type==0) or redo (type == 1)
  xptr node;
  trn_cell_analysis(char _type_, LONG_LSN _lsn_): type(_type_), lsn(_lsn_) {}; 
};

typedef std::map<transaction_id, trn_cell_analysis> trns_analysis_map;

typedef std::pair <transaction_id, trn_cell_analysis> trn_pair;


struct logical_log_sh_mem_head
{
  int end_offs; //the end byte (offset) of the log in buffer
  int begin_not_drbl_offs; //the begin byte (offset) of the not flushed buffer
  int free_bytes; //the number of free bytes in shared memory
  int keep_bytes; //the number of busy bytes (excluding header)
  int size; //the size of shared memory (including header)
  LONG_LSN next_lsn;//lsn of next record
  LONG_LSN next_durable_lsn; //next lsn to be recorded in durable storage
  trn_tbl t_tbl;//transaction table
};


class llmgr_core
{
private:
  UFile ll_file_dsc;
  UShMem shared_mem_dsc;
  USemaphore sem_dsc;
  void *shared_mem;
  char small_read_buf[LOGICAL_LOG_UNDO_READ_PORTION];
  char *large_read_buf;//this var is used when size of small_read_buf is not enough

  bool rollback_active;//used only from transaction (not used from sm)
  bool recovery_active;//used only from rcv_db process and if recovery_active == true the recovery process must not write to phys log

  char* indir_rec;//pointer to indirection log record; it must be appended to the next micro op log record and set to NULL
  int indir_rec_len;

public:
  //create and release functions; called in sm
  void ll_log_create(std::string db_files_path);
  void ll_log_create_shared_mem();
  void ll_log_release();
  void ll_log_release_shared_mem();


  //on session functions
  void ll_log_open(std::string db_files_path, bool rcv_active = false);
  void ll_log_open_shared_mem();
  void ll_log_close();
  void ll_log_close_shared_mem();

  //on transaction functions
  void ll_log_on_transaction_begin(bool rcv_active, transaction_id &trid, bool sync);
  void ll_log_on_transaction_end(transaction_id &trid, bool sync);


  //these functions are called during transaction execution
  void ll_log_element(transaction_id& trid, const xptr& self, const xptr& left, const xptr& right, const xptr& parent, const char* name, const char* uri, const char* prefix, xmlscm_type type, bool inserted, bool sync);
  void ll_log_attribute(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const  char* value,int value_size,const char* uri,const char* prefix,bool inserted, bool sync);
  void ll_log_text(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int value_size,bool inserted, bool sync);
  void ll_log_text_edit(transaction_id& trid, const xptr& self,const char* value, int data_size, bool begin, bool inserted, bool sync);
  void ll_log_document(transaction_id& trid, const xptr &self,const  char* name,const  char* collection,bool inserted, bool sync);
  void ll_log_pi(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int total_size,shft target_size,bool inserted, bool sync);
  void ll_log_comment(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted, bool sync);
  void ll_log_collection(transaction_id& trid, const  char* name,bool inserted, bool sync);
  void ll_log_ns(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted, bool sync);
  void ll_log_index(transaction_id& trid, const char *object_path, const char *key_path, xmlscm_type key_type,const char * index_title, const char* doc_name,bool is_doc,bool inserted, bool sync);

  LONG_LSN ll_log_commit(transaction_id _trid, bool sync);
  void ll_log_rollback(transaction_id _trid, bool sync);
  LONG_LSN ll_log_checkpoint(bool sync);
  void ll_log_indirection(transaction_id trid, int cl_hint, std::vector<xptr>* blocks, bool sync);


  void ll_log_flush(bool sync);
  void ll_log_flush(transaction_id trid, bool sync);
  void ll_log_flush_last_record(bool sync);
  void rollback_trn(transaction_id &trid, void (*exec_micro_op) (const char*, int, bool), bool sync);  
 #ifdef SE_ENABLE_FTSEARCH
  void recover_db_by_logical_log(void (*index_op) (trns_analysis_map&),void (*exec_micro_op) (const char*, int, bool),void(*switch_indirection)(int),void (*_rcv_allocate_blocks)(const std::vector<xptr>&), const LONG_LSN& last_cp_lsn, int undo_mode, int redo_mode, bool sync);
#else
void recover_db_by_logical_log(void (*exec_micro_op) (const char*, int, bool),void(*switch_indirection)(int),void (*_rcv_allocate_blocks)(const std::vector<xptr>&), const LONG_LSN& last_cp_lsn, int undo_mode, int redo_mode, bool sync);
#endif
  void flush_last_commit_lsn(LONG_LSN &commit_lsn);
  //void flush_last_checkpoint_lsn(LONG_LSN &checkpoint_lsn);

int  get_num_of_records_written_by_trn(transaction_id &trid);//used for debug

private:

  LONG_LSN ll_log_insert_record(const void* addr, int len, transaction_id &trid, bool sync);
  void writeSharedMemory(const void*, int len);
  logical_log_file_head read_log_file_header();
  void flush_log_file_header(const logical_log_file_head &head);
  void set_file_pointer(__int64 file_pos);
  const char* get_record_from_disk(LONG_LSN& lsn);
  int get_record_length(const void* rec);
  const char* get_record_from_shared_memory(int end_offs, int len);
  void delete_large_read_buf();
  void undo_trn(LONG_LSN& start_lsn, void (*exec_micro_op) (const char*, int, bool));
  void redo_commit_trns(trns_analysis_map& lst, LONG_LSN &start_lsn, LONG_LSN &end_lsn, void (*exec_micro_op) (const char*, int, bool));
  trns_analysis_map get_undo_redo_trns_map(LONG_LSN &start_lsn, LONG_LSN &end_lsn, std::vector<xptr>& indir_blocks);//last parameter is out

  inline void ll_log_lock(bool sync)
  {
     if(sync)
     {
        int res;
        res = USemaphoreDown(sem_dsc);
        if (res != 0)
            throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
     }

  };

  inline void ll_log_unlock(bool sync)
  {
     if(sync)
     {
        int res;
        res = USemaphoreUp(sem_dsc);
        if (res != 0)
            throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

     } 
  };

  inline void inc_mem_copy(void* dest, int &offs, const void* src, int len)
  {
    if (len == 0) return;
    memcpy((char*)dest+offs, src, len);
    offs +=len;
  };

  inline int min3(int x1, int x2, int x3)
  {
    if (x1 <= x2 && x1 <= x3) return x1;
    if (x2 <= x1 && x2 <= x3) return x2;
    else return x3;
  };	
};

#endif

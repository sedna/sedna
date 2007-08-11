/*
 * File:  llmgr_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LLMGR_CORE_H
#define _LLMGR_CORE_H

#include "common/sedna.h"
#include <string>
#include <vector>
#include <map>
#include <list>

#include "common/base.h"
#include "common/xptr.h"
#include "common/u/uhdd.h"
#include "common/u/usem.h"
#include "common/u/ushm.h"
#include "sm/plmgr/plmgr_core.h"
#include "common/mmgr/memutils.h"

#include "sm/wu/wu.h"

//#define LOGICAL_LOG
//#define LOGICAL_LOG_TEST
#define NULL_OFFS (-1)
#define LOGICAL_LOG_FLUSH_PORTION (128*1024) // 128 Kb
#define CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE (1024*1024) //10Mb
#define LOGICAL_LOG_UNDO_READ_PORTION 1024 //1Kb
#define MAX_LL_LOG_FILES_NUMBER 1000
#define MAX_LOG_FILE_NAME_LENGTH 20
#define COMMIT_LOG_RECORD_LEN (sizeof(logical_log_head) + sizeof(char) + sizeof(transaction_id))

//this parameter must be more than CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE
#define LOG_FILE_PORTION_SIZE (100 * (1024*1024)) //was 2
#define MAX_LOG_FILE_PORTIONS_NUMBER_WITHOUT_CHECKPOINTS 5 //was 3
//#define MAX_ALL_LOG_FILE_SIZE 10*LOG_FILE_PORTION_SIZE

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
      LL_CHECKPOINT,
      LL_INSERT_DOC_FTS_INDEX,
      LL_DELETE_DOC_FTS_INDEX,
      LL_INSERT_COL_FTS_INDEX,
      LL_DELETE_COL_FTS_INDEX
     };

enum transaction_mode {NORMAL_MODE, ROLLBACK_MODE};

struct logical_log_head
{
  int prev_trn_offs;
  int body_len;
};

struct logical_log_file_head
{
  LONG_LSN last_lsn;//last record of the log
  LONG_LSN next_lsn; //lsn of next record to be recorded
  int prev_file_number;//-1 indicates NULL
  __int64 base_addr;//this addr is used to calculate phys addr of lsn (it is equal to Record_lsn - base_addr)
  int valid_number;//identifier of log file from which the needed log records are begin. It is consistent with base addr

  LONG_LSN last_checkpoint_lsn; //lsn of the last checkpoint record
  LONG_LSN last_chain_lsn; // lsn of the last record in physical records chain
  TIMESTAMP ts; // timestamp of the last persistent snapshot
//  __int64 ph_cp_counter; // last checkpoint counter of the ph file
//  __int64 ph_cur_counter; // current counter of the ph file
  bool is_stopped_successfully; // true, if the database was stopped correctly
  int sedna_db_version;
  //new gield must be appended in the end of structure
};

struct trn_cell
{
  LONG_LSN last_lsn;
  LONG_LSN first_lsn;//lsn of first transaction's record in log
  int last_rec_mem_offs;//NULL_OFFS -> last record on disk
  transaction_mode mode;//indicates transaction mode
  LONG_LSN prev_rollback_lsn;//indicates next record to be rollbacked in ROLLBACK_MODE
                             //prev_rollback_lsn == NULL_LSN in NORMAL_MODE
  LONG_LSN hint_lsn;//this lsn must be assigned to prev_rollback_lsn in microop
  bool is_ended; //true if commit or rollback record has been added in the logical log of the transaction
  int num_of_log_records; //used for debug
};


typedef trn_cell trn_tbl[CHARISMA_MAX_TRNS_NUMBER];


enum trn_analysis_enum {TRN_NOT_FINISHED, TRN_ROLLBACK_FINISHED, TRN_COMMIT_FINISHED};

struct trn_cell_analysis_redo
{
  transaction_id trid;
  LONG_LSN trn_start_rcv_lsn;//first lsn of transaction or lsn of next record after recovery checkpoint 
  LONG_LSN trn_end_lsn;
  trn_analysis_enum finish_status;//used only during analysis phase;//0->not finished, 1->rollback record find, 2 ->commit record find
//  char type;//0 -> undo, 1 -> redo
//  LONG_LSN lsn;//record from which undo (type==0) or redo (type == 1)
  xptr node;//!!!what is it ?
//  trn_cell_analysis(char _type_, LONG_LSN _lsn_): type(_type_), lsn(_lsn_) {}; 
  trn_cell_analysis_redo(transaction_id _trid_, LONG_LSN _start_, LONG_LSN _end_): trid(_trid_), trn_start_rcv_lsn(_start_), trn_end_lsn(_end_), finish_status(TRN_NOT_FINISHED) {};
  trn_cell_analysis_redo(transaction_id _trid_, LONG_LSN _start_, LONG_LSN _end_, trn_analysis_enum _finish_status_): trid(_trid_), trn_start_rcv_lsn(_start_), trn_end_lsn(_end_), finish_status(_finish_status_) {};
};

struct trn_cell_analysis_undo
{
  transaction_id trid;
  LONG_LSN trn_undo_rcv_lsn;//lsn from which to execute undo during recovery
  LONG_LSN first_lsn_after_cp;//is needed if we find that transaction will be committed and we need to do redo for it from this lsn
  trn_analysis_enum finish_status;//used only during analysis phase;//0->not finished, 1->rollback record find, 2 ->commit record find
  xptr node;//!!!what is it ?
  trn_cell_analysis_undo(transaction_id _trid_, LONG_LSN _lsn_): trid(_trid_), trn_undo_rcv_lsn(_lsn_), first_lsn_after_cp(NULL_LSN),  finish_status(TRN_NOT_FINISHED) {};
};

//typedef std::map<transaction_id, trn_cell_analysis> trns_analysis_map;

//typedef std::pair <transaction_id, trn_cell_analysis> trn_pair;

typedef std::list<trn_cell_analysis_redo> trns_redo_analysis_list;
typedef std::list<trn_cell_analysis_redo>::iterator trns_redo_analysis_list_iterator;
typedef std::list<trn_cell_analysis_redo>::reverse_iterator trns_redo_analysis_list_reverse_iterator;

typedef std::list<trn_cell_analysis_undo> trns_undo_analysis_list;
typedef std::list<trn_cell_analysis_undo>::iterator trns_undo_analysis_list_iterator;



struct logical_log_sh_mem_head
{
  int end_offs; //the end byte (offset) of the log in buffer
  int begin_not_drbl_offs; //the begin byte (offset) of the not flushed buffer
  int free_bytes; //the number of free bytes in shared memory
  int keep_bytes; //the number of busy bytes (excluding header)
  int size; //the size of shared memory (including header)
  LONG_LSN next_lsn;//lsn of next record
  LONG_LSN next_durable_lsn; //next lsn to be recorded in durable storage
  LONG_LSN last_checkpoint_lsn; // lsn of the last checkpoint record
  LONG_LSN min_rcv_lsn; // lsn of the start record of logical recovery
  LONG_LSN last_chain_lsn; // lsn of the last record in LL_CHECKPOINT-LL_PERS_SNAPSHOT_ADD chain
  LONG_LSN last_lsn;  // lsnof the last record in log
//  int number_of_cp_records; // number of continous checkpoint records
  TIMESTAMP ts; // timestamp of the last persistent snapshot
  trn_tbl t_tbl;//transaction table
  int ll_files_arr[MAX_LL_LOG_FILES_NUMBER];//list of logical log files numbers (last element in the list is a tail of log)
  int ll_files_num;//indicates number of elements in ll_files_array
  int ll_free_files_arr[MAX_LL_LOG_FILES_NUMBER];
  int ll_free_files_num;
  __int64 base_addr;//this addr is used to calculate phys addr of lsn (it is equal to Record_lsn - base_addr)
  bool checkpoint_flag; // true, if checkpoint is enabled
  bool checkpoint_on;   // true, if checkpoint is currently in progress
};

struct log_file_dsc
{
  UFile dsc;
  int name_number;
};


class llmgr_core
{
protected:
  UFile ll_curr_file_dsc;
  UShMem shared_mem_dsc;
  USemaphore sem_dsc;

  USemaphore wait_for_checkpoint_sem;//semaphore for initing checkpoint
  
  void *shared_mem;
  char* read_buf;//
  int read_buf_size;
//  char small_read_buf[LOGICAL_LOG_UNDO_READ_PORTION];
//  char *large_read_buf;//this var is used when size of small_read_buf is not enough

  bool rollback_active;//used only from transaction (not used from sm)
  bool recovery_active;//used only from rcv_db process and if recovery_active == true the recovery process must not write to phys log

  char* indir_rec;//pointer to indirection log record; it must be appended to the next micro op log record and set to NULL
  int indir_rec_len;
  int indir_rec_buf_size;
  std::vector<log_file_dsc> ll_open_files;//this structure is ordered in sm and is unordered in transaction (may contain duplicates)

//  plmgr_core* _phys_log_mgr_; //used to activate checkpoint
//  stack<int> ll_free_file_name_numbers;//used for creating next log file name (prefix + number)
//  !!!int log_file_portion_size;//size of one physical log file (must be ininted via input papram)

  std::string db_files_path;
  std::string db_name;

  //for mm
  int internal_buf_size;
  char* internal_buf;

public:
  //create and release functions; called in sm
  bool ll_log_create(std::string _db_files_path_, std::string _db_name_, int &sedna_db_version/*, plmgr_core* _phys_log_mgr_*/);
  void ll_log_create_shared_mem();
  void ll_log_release();
  void ll_log_release_shared_mem();


  //on session functions
  void ll_log_open(std::string db_files_path, std::string db_name, /*plmgr_core* _phys_log_mgr_,*/ bool rcv_active = false);
  void ll_log_open_shared_mem();
  void ll_log_close();
  void ll_log_close_shared_mem();

  //on transaction functions
  void ll_log_on_transaction_begin(bool rcv_active, transaction_id &trid, bool sync);
  void ll_log_on_transaction_end(transaction_id &trid, bool sync);
  LONG_LSN ll_get_lsn_of_first_record_in_logical_log(transaction_id &trid, bool sync);


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
  void ll_log_ft_index(transaction_id& trid, const char *object_path, int itconst, const char* index_title, const char *doc_name, bool is_doc, char* custom_tree_buf, int custom_tree_size, bool inserted, bool sync);
  void ll_log_trigger(transaction_id& trid, int tr_time, int tr_event, const char *trigger_path, int tr_gran, char *tr_action_buf, int tr_action_size, const char *in_name, int in_type, const char *path_to_parent, const char *trigger_titile, const char *doc_name, bool is_doc, bool inserted, bool sync);  

  LONG_LSN ll_log_commit(transaction_id _trid, bool sync);
  void ll_log_rollback(transaction_id _trid, bool sync);
//  LONG_LSN ll_log_checkpoint(bool sync);
//  void ll_log_checkpoint(void *userData, SnapshotsVersionInfo *buf, size_t count);
  void ll_log_indirection(transaction_id trid, int cl_hint, std::vector<xptr>* blocks, bool sync);

  void ll_log_free_blocks(XPTR phys_xptr, void *block, int size, bool sync);
  void ll_log_pers_snapshot_add(WuVersionEntry *blk_info, int isGarbage, bool sync);
  void ll_log_decrease(__int64 old_size, bool sync);
  
  void set_hint_lsn_for_prev_rollback_record(transaction_id &trid, LONG_LSN lsn);
  void set_prev_rollback_lsn(transaction_id &trid, bool sync);//this function must be called inside microop

  void ll_log_flush_all_last_records(bool sync);
  void ll_log_flush(bool sync);
  void ll_log_flush(transaction_id trid, bool sync);
  void ll_log_flush_all_last_records(bool sync);
  void ll_log_flush_last_record(bool sync);
  void ll_truncate_log(bool sync);

  void commit_trn(transaction_id& trid, bool sync);
//  void rollback_trn(transaction_id &trid, void (*exec_micro_op) (const char*, int, bool), bool sync);  
#ifdef SE_ENABLE_FTSEARCH
void recover_db_by_logical_log(void (*index_op) (const trns_undo_analysis_list&, const trns_redo_analysis_list&, const LONG_LSN&),
                               void (*exec_micro_op) (const char*, int, bool),
                               void(*switch_indirection)(int),
                               void (*_vmm_rcv_add_to_indir_block_set_)(xptr p),
                               void (*_vmm_rcv_clear_indir_block_set_)(),
                               void (*_sync_indirection_table_)(),
                               const LONG_LSN& last_cp_lsn,
                               int undo_indir_mode,
                               int redo_indir_mode,
                               bool sync);
#else
void recover_db_by_logical_log(void (*exec_micro_op) (const char*, int, bool),void(*switch_indirection)(int),void (*_vmm_rcv_add_to_indir_block_set_)(xptr p), void (*_vmm_rcv_clear_indir_block_set_)(),void (*_sync_indirection_table_)(), const LONG_LSN& last_cp_lsn, int undo_mode, int redo_mode, bool sync);

#endif

//  void freePrevCheckpointBlocks(LONG_LSN last_lsn, bool sync);

  TIMESTAMP returnTimestampOfPersSnapshot(bool sync);

  void flush_last_commit_lsn(LONG_LSN &commit_lsn);//flushes to header last commit lsn
  //void flush_last_checkpoint_lsn(LONG_LSN &checkpoint_lsn);
  void flush_file_head(bool sync);
  void writeIsStoppedCorrectly(bool is_stopped_correctly);

  int  get_num_of_records_written_by_trn(transaction_id &trid);//used for debug

  void open_all_log_files();//fills ll_open_files structure
  void close_all_log_files();//close file descriptors of all log files
  void extend_logical_log(bool sync);

//  __int64 getNewPhCounter(); // returns file counter for the new ph-file
//  __int64 getCurPhCounter(); // returns file counter for the last ph-file

  void updateMinRcvLSN();

  void activate_checkpoint(bool sync); // moved from private

  void set_checkpoint_on_flag(bool flag); // set flag determing if checkpoint thread is active

  void set_checkpoint_flag(bool flag, bool sync); // set flag to enable/disable checkpoint

protected:

  LONG_LSN ll_log_insert_record(const void* addr, int len, transaction_id &trid, bool sync);
  void writeSharedMemory(const void*, int len);
//  logical_log_file_head read_log_file_header();
//  logical_log_file_head read_tail_log_file_header();
  logical_log_file_head read_log_file_header(UFile file_dsc);
  UFile get_log_file_descriptor(int log_file_number);

//  void flush_log_file_header(const logical_log_file_head &head);
  void set_file_pointer(LONG_LSN &lsn);
  const char* get_record_from_disk(LONG_LSN& lsn);
  int get_record_length(const void* rec);
  const char* get_record_from_shared_memory(int end_offs, int len);
//  void undo_trn(LONG_LSN& start_lsn, void (*exec_micro_op) (const char*, int, bool));
  void redo_commit_trns(trns_redo_analysis_list& redo_list, LONG_LSN &start_lsn, LONG_LSN &end_lsn, void (*exec_micro_op) (const char*, int, bool));
  void get_undo_redo_trns_list(LONG_LSN &start_lsn, LONG_LSN &end_lsn, /*trns_undo_analysis_list& undo_list,*/ /*out*/trns_redo_analysis_list& redo_list /*out*/, void (*_vmm_rcv_add_to_indir_block_set_)(xptr p));

  bool find_redo_trn_cell(transaction_id trid,
                          trns_redo_analysis_list& redo_list,
                          LONG_LSN lsn,
                          trn_cell_analysis_redo& redo_trn_cell/*out*/);

//  bool find_undo_trn_cell(transaction_id trid, trns_undo_analysis_list& undo_list, trn_cell_analysis_undo& undo_trn_cell/*out*/);
//  void set_undo_trn_cell(transaction_id trid, trns_undo_analysis_list& undo_list, trn_cell_analysis_undo& undo_trn_cell/*in*/);

  bool find_last_redo_trn_cell(transaction_id trid, trns_redo_analysis_list& redo_list, trn_cell_analysis_redo& redo_trn_cell/*out*/);
  void set_last_redo_trn_cell(transaction_id trid, trns_redo_analysis_list& redo_list,trn_cell_analysis_redo& redo_trn_cell/*in*/);

  LONG_LSN getFirstCheckpointLSN(LONG_LSN lastCheckpointLSN); //returns lsn of the first checkpoint record in a bundle

  inline void ll_log_lock(bool sync)
  {
     if(sync)
     {
        int res;
        res = USemaphoreDown(sem_dsc, __sys_call_error);
        if (res != 0)
            throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
     }

  };

  inline void ll_log_unlock(bool sync)
  {
     if(sync)
     {
        int res;
        res = USemaphoreUp(sem_dsc, __sys_call_error);
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

  inline char* ll_log_malloc(int size)
  {
     if (internal_buf_size < size)
     {
        se_delete(internal_buf);
        internal_buf = se_new_cxt(TransactionContext) char[size];
     }

     return internal_buf;
  };
};

UFile create_logical_log(const char* log_file_name,
                         int valid_file_number,
                         LONG_LSN base_addr,
                         int _prev_file_number_,
                         LONG_LSN commit_lsn,
                         LONG_LSN next_after_commit_lsn,

  						 LONG_LSN last_checkpoint_lsn, 
  						 LONG_LSN last_chain_lsn, 
  						 TIMESTAMP ts,
//                         __int64 ph_cp_counter,

                         bool is_close_file = false 
                        );


#endif

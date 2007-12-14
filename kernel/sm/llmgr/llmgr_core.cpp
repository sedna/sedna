/*
 * File:  llmgr_core.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "common/base.h"
#include "common/u/ushm.h"
#include "common/xptr.h"
#include "sm/llmgr/llmgr_core.h"
#include "sm/sm_globals.h"
#include "common/utils.h"
#include "common/tr_debug.h"
#include "sm/plmgr/plmgr_core.h"
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"
#include "sm/trmgr.h"
#include "tr/vmm/vmm.h"
#include "tr/structures/indirection.h"

using namespace std;


/*****************************************************************************
                          Init and Close Functions
******************************************************************************/

/*                 !!!These Functions called on sm !!!                         */
void llmgr_core::ll_log_create(string _db_files_path_, string _db_name_, plmgr_core* phys_log_mgr_)
{

  int res;
  //!!!init logical log protection semaphore!!!
  res = USemaphoreCreate(&sem_dsc, 1, 1, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME, NULL, __sys_call_error);  

  if ( res != 0 )
     throw USER_EXCEPTION2(SE4010, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

  db_files_path = _db_files_path_;
  db_name = _db_name_;


  //this functio opens existing log files and the order in vector corresponds to the order of files
  open_all_log_files();

  if (ll_open_files.size() < 1) throw SYSTEM_EXCEPTION("There is no logical log files");

  logical_log_file_head file_head = read_log_file_header(ll_open_files[ll_open_files.size() - 1].dsc);
  int valid_number = file_head.valid_number;


  //delete unnessary files
  int i,j;
  char buf[20];
  std::string log_file_name;
  for (i=0; i< ll_open_files.size(); i++)
  {
      if (ll_open_files[i].name_number == valid_number) break;

      log_file_name = _db_files_path_ + _db_name_ + "." + u_itoa(ll_open_files[i].name_number, buf, 10) + "llog";
      if ( uDeleteFile(log_file_name.c_str(), __sys_call_error) == 0)
         throw USER_EXCEPTION2(SE4041, log_file_name.c_str());
  }

  //shift files to left
  for (j=0; j <i; j++)
       ll_open_files.erase(ll_open_files.begin());
  
//  if (sizeof(small_read_buf) < sizeof(logical_log_head))
//     throw USER_EXCEPTION(SE4150);

  rollback_active = false;
  recovery_active = false;

  checkpoint_active = false;

  indir_rec = NULL;
  indir_rec_len = 0;
  
  internal_buf = se_new_cxt(TopMemoryContext) char[2*PAGE_SIZE];
  internal_buf_size = 2*PAGE_SIZE;
  indir_rec_buf_size = 0;

  read_buf = se_new_cxt(TopMemoryContext) char[LOGICAL_LOG_UNDO_READ_PORTION];
  read_buf_size = LOGICAL_LOG_UNDO_READ_PORTION;

  this->_phys_log_mgr_ = phys_log_mgr_;
}




void llmgr_core::ll_log_release()
{
  int res;

  res = USemaphoreRelease(sem_dsc, __sys_call_error);  

  if (res != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

  close_all_log_files();
}



void llmgr_core::ll_log_create_shared_mem()
{
   //create shared memory

   int res;

   if (CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE <= sizeof(logical_log_sh_mem_head))
      throw USER_EXCEPTION(SE4151);

   res = uCreateShMem(&shared_mem_dsc,
                      CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME,
                      CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE,
                      NULL,
                      __sys_call_error
                     );

   if (res != 0)
      throw USER_EXCEPTION2(SE4016, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");


   //init shared memory pointer
   shared_mem = uAttachShMem(shared_mem_dsc,
                             NULL,
                             CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE,
                             __sys_call_error
                            );

   if (shared_mem == NULL)
      throw USER_EXCEPTION2(SE4023, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

   //init header of shared memory
   logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;

   mem_head->end_offs = sizeof(logical_log_sh_mem_head);
   mem_head->begin_not_drbl_offs = sizeof(logical_log_sh_mem_head);
   mem_head->free_bytes = CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE - sizeof(logical_log_sh_mem_head); 
   mem_head->keep_bytes = 0;
   mem_head->size = (int)CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE;
      
   logical_log_file_head file_head = read_log_file_header(ll_open_files[ll_open_files.size() - 1].dsc);
   mem_head->next_lsn =  file_head.next_lsn;
   mem_head->next_durable_lsn = mem_head->next_lsn;
   
   int i;
   for (i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
   {
       mem_head->t_tbl[i].last_lsn = NULL_LSN; 
       mem_head->t_tbl[i].first_lsn = NULL_LSN; 
       mem_head->t_tbl[i].last_rec_mem_offs = NULL_OFFS;
       mem_head->t_tbl[i].is_ended = false;
       mem_head->t_tbl[i].num_of_log_records = 0;

   }

   for (i=0; i< ll_open_files.size(); i++)
       mem_head->ll_files_arr[i] = ll_open_files[i].name_number;

   mem_head->ll_files_num = ll_open_files.size();

   //!!! init ll_free_files_arr and ll_free_files_num
   int j, ind = 0;
   bool is_exist; 

   for (i = MAX_LL_LOG_FILES_NUMBER -1 ; i>=0; i--)
   {
      is_exist = false;

      for(j=0; j< ll_open_files.size(); j++)
	  {	   
         if (ll_open_files[j].name_number == i) 
         {
            is_exist = true;
            break;
         }
	  }

      if (!is_exist)
         mem_head->ll_free_files_arr[ind++]=i; 
  }

  mem_head->ll_free_files_num = ind;

  mem_head->base_addr = file_head.base_addr;//here base addr of tail log file

  close_all_log_files();

}


void llmgr_core::ll_log_release_shared_mem()
{
  int res = uDettachShMem(shared_mem_dsc, shared_mem, __sys_call_error);

  if (res != 0)
     throw USER_EXCEPTION2(SE4024, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

  res = uReleaseShMem(shared_mem_dsc, __sys_call_error);

  if (res != 0)
     throw USER_EXCEPTION2(SE4020, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

}


/*               !!! These fuctions are called on session !!!                */ 

void llmgr_core::ll_log_open(string _db_files_path_, string _db_name_, plmgr_core* phys_log_mgr_,  bool rcv_active)
{
  int res;

  res = USemaphoreOpen(&sem_dsc, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME, __sys_call_error);

  if ( res != 0 )
     throw USER_EXCEPTION2(SE4012, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");


  db_files_path = _db_files_path_;
  db_name = _db_name_;


  this->_phys_log_mgr_ = phys_log_mgr_;
/*
  ll_file_curr_dsc = uOpenFile(
                       db_files_path.c_str(),
                       U_SHARE_READ | U_SHARE_WRITE,
                       U_READ_WRITE,
                       U_WRITE_THROUGH 
                     );

  if ( ll_file_curr_dsc == U_INVALID_FD )
     throw USER_EXCEPTION2(SE4042, db_files_path.c_str());  
*/
}


void llmgr_core::ll_log_open_shared_mem()
{
   //open shared memory
   int res;

   res = uOpenShMem(&shared_mem_dsc,
                    CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME,
                    CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE,
                    __sys_call_error
                   );

   if (res != 0)
      throw USER_EXCEPTION2(SE4021, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

   //init shared memory pointer
   shared_mem = uAttachShMem(shared_mem_dsc, NULL, CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE, __sys_call_error);

   if ( shared_mem == NULL)
      throw USER_EXCEPTION2(SE4023, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

}

void llmgr_core::ll_log_close_shared_mem()
{
  int res = uDettachShMem(shared_mem_dsc, shared_mem, __sys_call_error);

  if (res != 0)
     throw USER_EXCEPTION2(SE4024, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

  res = uCloseShMem(shared_mem_dsc, __sys_call_error);

  if (res != 0)
     throw USER_EXCEPTION2(SE4022, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
}


void llmgr_core::ll_log_close()
{
  int res;

  res = USemaphoreClose(sem_dsc, __sys_call_error);  

  if (res != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");
/*
  res = uCloseFile(ll_file_curr_dsc);
  
  if (res == 0)
     throw USER_EXCEPTION2(SE4043, "logical log file");
*/

}


/*            !!! These functions are called on transaction !!!                 */


void llmgr_core::ll_log_on_transaction_begin(bool rcv_active, transaction_id &trid, bool sync)
{
  rollback_active = false;
  recovery_active = rcv_active;

  indir_rec = NULL;
  indir_rec_len = 0;

  internal_buf = se_new_cxt(TransactionContext) char[2*PAGE_SIZE];
  internal_buf_size = 2*PAGE_SIZE;
  indir_rec_buf_size = 0;

  read_buf = se_new_cxt(TransactionContext) char[LOGICAL_LOG_UNDO_READ_PORTION];
  read_buf_size = LOGICAL_LOG_UNDO_READ_PORTION;



  ll_log_lock(sync);
   
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  if (trid > CHARISMA_MAX_TRNS_NUMBER || trid < 0)
     throw USER_EXCEPTION2(SE4155, int2string(trid).c_str());

  mem_head->t_tbl[trid].last_lsn = NULL_LSN;
  mem_head->t_tbl[trid].first_lsn = NULL_LSN;
  mem_head->t_tbl[trid].last_rec_mem_offs  = NULL_OFFS;
  mem_head->t_tbl[trid].is_ended = false;
  mem_head->t_tbl[trid].num_of_log_records = 0;
  mem_head->t_tbl[trid].mode = NORMAL_MODE;
  mem_head->t_tbl[trid].prev_rollback_lsn = NULL_LSN;
  mem_head->t_tbl[trid].hint_lsn = NULL_LSN;
  

  ll_log_unlock(sync);

}


void llmgr_core::ll_log_on_transaction_end(transaction_id &trid, bool sync)
{
  ll_log_lock(sync);
   
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  if (trid > CHARISMA_MAX_TRNS_NUMBER || trid < 0)
     throw USER_EXCEPTION2(SE4155, int2string(trid).c_str());

  mem_head->t_tbl[trid].last_lsn = NULL_LSN;
  mem_head->t_tbl[trid].first_lsn = NULL_LSN;
  mem_head->t_tbl[trid].last_rec_mem_offs  = NULL_OFFS;
  mem_head->t_tbl[trid].is_ended = false;
  mem_head->t_tbl[trid].num_of_log_records = 0;

  close_all_log_files();

  if (internal_buf_size > 0) se_delete(internal_buf);
  if (indir_rec_buf_size > 0) se_delete(indir_rec);
  if (read_buf_size > 0) se_delete(read_buf);

  ll_log_unlock(sync);
}


/*****************************************************************************
                         Main Functions
******************************************************************************/

/*element record body format:
  op_type(1 byte),
  trid (2byte)
  indir_table info
  name ('\0' terminated string)
  uri ('\0' terminated string)
  prefix ('\0' terminated prefix)
  xmlscm_type (2byte)
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
*/
void llmgr_core::ll_log_element(transaction_id& trid, const xptr& self,const xptr& left, const xptr& right, const xptr& parent, const char* name, const char* uri, const char* prefix, xmlscm_type type, bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;
  int rec_len;
  int uri_len = (uri != NULL) ? strlen(uri)+1 : 1;
  int prefix_len = (prefix != NULL) ? strlen(prefix)+1 : 1;

  rec_len = sizeof(char) +
            sizeof(transaction_id) + 
            indir_rec_len +
            strlen(name)+1 +
            uri_len +
            prefix_len +
            sizeof(xmlscm_type) +                           
            4*sizeof(xptr); 

  tmp_rec = ll_log_malloc(rec_len);

  //char op = (inserted) ? LL_INSERT_ELEM: LL_DELETE_ELEM;
  char op;

  if (indir_rec_len == 0)
     op = (inserted) ? LL_INSERT_ELEM : LL_DELETE_ELEM;
  else
     op = (inserted) ? LL_INDIR_INSERT_ELEM : LL_INDIR_DELETE_ELEM;
  
  int offs=0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, name, strlen(name)+1);
  inc_mem_copy(tmp_rec, offs, ((uri != NULL ) ? uri: ""), uri_len);
  inc_mem_copy(tmp_rec, offs, ((prefix != NULL ) ? prefix: ""), prefix_len);
  inc_mem_copy(tmp_rec, offs, &type, sizeof(xmlscm_type));
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &left, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &right, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &parent, sizeof(xptr));

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);

  indir_rec_len = 0;
}

/* attribute record body format:
  op-type (1byte)
  trid (2byte)
  indirection table info
  name ('\0' terminated string)
  uri ('\0' terminated string)
  prefix ('\0' terminated string)
  value_size (4 bytes)
  value (without '\0')
  xmlscm_type (2byte)
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
*/

void llmgr_core::ll_log_attribute(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const char* value,int value_size,const char* uri,const char* prefix,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;
  int rec_len;
  int uri_len = (uri != NULL) ? strlen(uri)+1 : 1;
  int prefix_len = (prefix != NULL) ? strlen(prefix)+1 : 1;


  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            strlen(name)+1 +
            uri_len +
            prefix_len +
            sizeof(int) +
            value_size +
            sizeof(xmlscm_type) +
            4*sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (indir_rec_len == 0)
     op = (inserted) ? LL_INSERT_ATTR: LL_DELETE_ATTR;
  else
     op = (inserted) ? LL_INDIR_INSERT_ATTR : LL_INDIR_DELETE_ATTR;
  int offs=0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);


  inc_mem_copy(tmp_rec, offs, name, strlen(name)+1);
  inc_mem_copy(tmp_rec, offs, ((uri != NULL ) ? uri: ""), uri_len);
  inc_mem_copy(tmp_rec, offs, ((prefix != NULL ) ? prefix: ""), prefix_len);
  inc_mem_copy(tmp_rec, offs, &value_size, sizeof(int));
  inc_mem_copy(tmp_rec, offs, value, value_size);
  inc_mem_copy(tmp_rec, offs, &type, sizeof(xmlscm_type));
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &left, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &right, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &parent, sizeof(xptr));    

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);

  indir_rec_len = 0;
}

/*
 text log record format:
 op (1byte)
 trid (2byte)
 indirection table info
 value_size (4bytes)
 value (without '\0')
 self-xptr(8byte)
 left-xptr(8byte)
 right-xptr(8byte)
 parent-xptr(8byte) 
*/

void llmgr_core::ll_log_text(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int value_size,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;
  int rec_len;

  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            sizeof(int) +
            value_size +
            4*sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (indir_rec_len == 0)
     op = (inserted) ? LL_INSERT_TEXT: LL_DELETE_TEXT;
  else
     op = (inserted) ? LL_INDIR_INSERT_TEXT: LL_INDIR_DELETE_TEXT;
  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, &value_size, sizeof(int));
  inc_mem_copy(tmp_rec, offs, value, value_size);
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &left, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &right, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &parent, sizeof(xptr));

  //inert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);

  indir_rec_len = 0;
}


/*
 text_edit log record format:
 op (1byte)
 trid (2byte)
 data_size (4bytes)
 value (without '\0')
 self-xptr(8byte)
*/
void llmgr_core::ll_log_text_edit(transaction_id& trid, const xptr& self,const char* value, int data_size, bool begin, bool inserted, bool sync)
{//!!! this operation does not need indirection information
  if (rollback_active || recovery_active) return;

  char *tmp_rec;
  int rec_len;

  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            sizeof(int) +
            data_size +
            sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (begin)
     op = (inserted) ? LL_INSERT_LEFT_TEXT: LL_DELETE_LEFT_TEXT;
  else
     op = (inserted) ? LL_INSERT_RIGHT_TEXT: LL_DELETE_RIGHT_TEXT;

  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, &data_size, sizeof(int));
  inc_mem_copy(tmp_rec, offs, value, data_size);
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));

  //inert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);

}


/*
 document log record format:
 op (1byte)
 trid (2 bytes)
 name ('\0' terminated string) 
 collection ('\0' terminated string)
 self (xptr)
*/

void llmgr_core::ll_log_document(transaction_id& trid, const xptr &self,const  char* name,const  char* collection,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;
  int rec_len;
  int col_len = (collection != NULL) ? (strlen(collection) + 1) : 1;

  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            strlen(name)+1 +
            col_len +
            sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (indir_rec_len == 0)
     op = inserted ? LL_INSERT_DOC : LL_DELETE_DOC;
  else
     op = inserted ? LL_INDIR_INSERT_DOC : LL_INDIR_DELETE_DOC;

  int offs =0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char)); 
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, name, strlen(name)+1);
  inc_mem_copy(tmp_rec, offs, (collection != NULL) ? collection : "", col_len);
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);

  indir_rec_len = 0;
}

/*
  pi log record format:
  op (1 byte)
  trid (transaction_id)
  indirection table info
  total_size (4 bytes)
  target_size (2 bytes)
  value (without '\0')
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
 
*/
void llmgr_core::ll_log_pi(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int total_size,shft target_size,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;  
  int rec_len;

  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            sizeof(int) +
            sizeof(shft) +
            total_size +
            4*sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;
   
  if (indir_rec_len == 0)
     op = inserted ? LL_INSERT_PI : LL_DELETE_PI;
  else
     op = inserted ? LL_INDIR_INSERT_PI : LL_INDIR_DELETE_PI;

  int offs=0;
  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, &total_size, sizeof(int));
  inc_mem_copy(tmp_rec, offs, &target_size, sizeof(shft));
  inc_mem_copy(tmp_rec, offs, value, total_size);
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &left, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &right, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &parent, sizeof(xptr));

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);


  indir_rec_len = 0;
}

/*
  comment log record format:
  op (1 byte)
  trid (transaction_id)
  indirection table info
  value_size (4 bytes)
  value (without '\0')
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
*/
void llmgr_core::ll_log_comment(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int value_size,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;  
  int rec_len;

  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            sizeof(int) +
            value_size +
            4*sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (indir_rec_len == 0)
     op = inserted ? LL_INSERT_COMMENT : LL_DELETE_COMMENT;
  else
     op = inserted ? LL_INDIR_INSERT_COMMENT : LL_INDIR_DELETE_COMMENT;


  int offs =0;
  
  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, &value_size, sizeof(int));
  inc_mem_copy(tmp_rec, offs, value, value_size);
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &left, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &right, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &parent, sizeof(xptr));

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);


  indir_rec_len = 0;
}

/*
 collection log record format:
  op (1 byte)
  trid (transaction_id)
  indirection table info
  name ('\0' terminated string)
*/

void llmgr_core::ll_log_collection(transaction_id& trid, const  char* name,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;  
  int rec_len;

  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            strlen(name)+1;
  
  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (indir_rec_len == 0)
     op = inserted ? LL_INSERT_COLLECTION : LL_DELETE_COLLECTION;
  else
     op = inserted ? LL_INDIR_INSERT_COLLECTION : LL_INDIR_DELETE_COLLECTION;


  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, name, strlen(name)+1);
  
  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);


  indir_rec_len = 0;
}

/*
 namespace log record format:
 op (1 byte)
 trid (transaction_id)
 indirection table info
 uri ('\0' terminated string)
 prefix ('\0' terminated string)
 self-xptr(8byte)
 left-xptr(8byte)
 right-xptr(8byte)
 parent-xptr(8byte) 
*/
void llmgr_core::ll_log_ns(transaction_id& trid, const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  char *tmp_rec;  
  int rec_len;
  int prefix_len = (prefix != NULL) ? (strlen(prefix) + 1) : 1;

 
  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            indir_rec_len +
            strlen(uri)+1 +
            prefix_len +
            4*sizeof(xptr);

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (indir_rec_len == 0)
     op = inserted ? LL_INSERT_NS : LL_DELETE_NS;
  else
     op = inserted ? LL_INDIR_INSERT_NS : LL_INDIR_DELETE_NS;


  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  inc_mem_copy(tmp_rec, offs, indir_rec, indir_rec_len);

  inc_mem_copy(tmp_rec, offs, uri, strlen(uri)+1);
  inc_mem_copy(tmp_rec, offs, (prefix != NULL) ? prefix : "", prefix_len);
  inc_mem_copy(tmp_rec, offs, &self, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &left, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &right, sizeof(xptr));
  inc_mem_copy(tmp_rec, offs, &parent, sizeof(xptr));

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);

  indir_rec_len = 0;
}


/*
 index log record format:
 op (1 byte)
 trid (transaction_id)
 object_path ('\0' terminated string)
 key_path ('\0' terminated string)
 key_type (2 bytes)
 index_title ('\0' terminated string)
 doc_name or collection_name ('\0' terminated string)
*/
void llmgr_core::ll_log_index(transaction_id& trid, const char* object_path, const char* key_path, xmlscm_type key_type,const char * index_title, const char* doc_name,bool is_doc,bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  //d_printf2("written in log obj_path=%s\n", object_path);
  //d_printf2("written in log key_path=%s\n", key_path);


  char *tmp_rec;  
  int rec_len;
  int obj_path_len = (object_path != NULL) ? (strlen(object_path)+1) : 1;
  int key_path_len = (key_path != NULL) ? (strlen(key_path)+1) : 1;
  int ind_title_len = (index_title != NULL) ? (strlen(index_title)+1) : 1;
  int doc_name_len = (doc_name != NULL) ? (strlen(doc_name)+1) : 1;

 
  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            obj_path_len +
            key_path_len +
            sizeof(xmlscm_type) +
            ind_title_len +
            doc_name_len;

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (is_doc)
     op = inserted ? LL_INSERT_DOC_INDEX : LL_DELETE_DOC_INDEX;
  else
     op = inserted ? LL_INSERT_COL_INDEX : LL_DELETE_COL_INDEX;


  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));


  inc_mem_copy(tmp_rec, offs, (object_path != NULL) ? object_path : "", obj_path_len);
  inc_mem_copy(tmp_rec, offs, (key_path != NULL) ? key_path : "", key_path_len);

  inc_mem_copy(tmp_rec, offs, &key_type, sizeof(xmlscm_type));

  inc_mem_copy(tmp_rec, offs, (index_title != NULL) ? index_title : "", ind_title_len);
  inc_mem_copy(tmp_rec, offs, (doc_name != NULL) ? doc_name : "", doc_name_len);


  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);
}

/*
 Full-text index log record format:
 op (1 byte)
 trid (transaction_id)
 object_path ('\0' terminated string)
 ft_index_type (int) 
 index_title ('\0' terminated string)
 doc_name ('\0' terminated string)
 custom_tree_size (int)
 custom_tree_buf (custom_tree_size bytes)
*/

void llmgr_core::ll_log_ft_index(transaction_id& trid, const char *object_path, int itconst, const char* index_title, const char *doc_name, bool is_doc, char* custom_tree_buf, int custom_tree_size, bool inserted, bool sync)
{
  if (rollback_active || recovery_active) return;

  //d_printf2("written in log obj_path=%s\n", object_path);
  //d_printf2("written in log key_path=%s\n", key_path);


  char *tmp_rec;  
  int rec_len;
  int obj_path_len = (object_path != NULL) ? (strlen(object_path)+1) : 1;
  int ind_title_len = (index_title != NULL) ? (strlen(index_title)+1) : 1;
  int doc_name_len = (doc_name != NULL) ? (strlen(doc_name)+1) : 1;

 
  rec_len = sizeof(char) +
            sizeof(transaction_id) +
            obj_path_len +
            sizeof(int) +
            ind_title_len +
            doc_name_len +
            sizeof(int) + //is_doc
            custom_tree_size;

  tmp_rec = ll_log_malloc(rec_len);

  char op;

  if (is_doc)
     op = inserted ? LL_INSERT_DOC_FTS_INDEX : LL_DELETE_DOC_FTS_INDEX;
  else
     op = inserted ? LL_INSERT_COL_FTS_INDEX : LL_DELETE_COL_FTS_INDEX;


  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
  inc_mem_copy(tmp_rec, offs, (object_path != NULL) ? object_path : "", obj_path_len);
  inc_mem_copy(tmp_rec, offs, &itconst, sizeof(int));

  inc_mem_copy(tmp_rec, offs, (index_title != NULL) ? index_title : "", ind_title_len);
  inc_mem_copy(tmp_rec, offs, (doc_name != NULL) ? doc_name : "", doc_name_len);

  inc_mem_copy(tmp_rec, offs, &custom_tree_size, sizeof(int));
  inc_mem_copy(tmp_rec, offs, custom_tree_buf, custom_tree_size);

  //insert record
  ll_log_insert_record(tmp_rec, rec_len, trid, sync);
}

/*
 commit log record format:
 op (1 byte)
 trid (transaction_id)
*/
LONG_LSN llmgr_core::ll_log_commit(transaction_id trid, bool sync)
{
  char *tmp_rec;  
  int rec_len;
  LONG_LSN ret_lsn;
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;


  ll_log_lock(sync);

  rec_len = sizeof(char) + sizeof(transaction_id);

  tmp_rec = ll_log_malloc(rec_len);
  char op = LL_COMMIT;
  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  //insert record in shared memory
  ret_lsn = ll_log_insert_record(tmp_rec, rec_len, trid, false);

  mem_head->t_tbl[trid].is_ended = true;  

  ll_log_unlock(sync);

  return ret_lsn;
}


/*
 rollback log record format:
 op (1 byte)
 trid (transaction_id)
*/
void llmgr_core::ll_log_rollback(transaction_id trid, bool sync)
{
  char *tmp_rec;  
  int rec_len;
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  ll_log_lock(sync);

  rec_len = sizeof(char) + sizeof(transaction_id);

  tmp_rec = ll_log_malloc(rec_len);
  char op = LL_ROLLBACK;
  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  //insert record in shared memory
  ll_log_insert_record(tmp_rec, rec_len, trid, false);

  mem_head->t_tbl[trid].is_ended = true;

  ll_log_unlock(sync);
}

/*
 checkpoint log record format:
 op (1 byte)
 num (int) number of active transactions (including aborting)
 trid (4byte)     | num times
 LONG_LSN (8byte) |
*/
LONG_LSN llmgr_core::ll_log_checkpoint(bool sync)
{
  char *tmp_rec;  
  int rec_len;
  char op = LL_CHECKPOINT;
  int num = CHARISMA_MAX_TRNS_NUMBER;
  int offs = 0;
  LONG_LSN ret_lsn;

  ll_log_lock(sync);  

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;


  rec_len = sizeof(char) + sizeof(int) + CHARISMA_MAX_TRNS_NUMBER*(sizeof(LONG_LSN) + sizeof(transaction_id));
  //check that log file will not be overflowed

  if (LOG_FILE_PORTION_SIZE - (mem_head->next_lsn - (mem_head->base_addr + (mem_head->ll_files_num -1)*LOG_FILE_PORTION_SIZE)) <
	  (sizeof(logical_log_head) + rec_len))
  {//current log must be flushed and new file created
     ll_log_flush(false);
     extend_logical_log(false);
  }


  tmp_rec = ll_log_malloc(rec_len);
  ret_lsn = mem_head->next_lsn;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &num, sizeof(int));
  for (int i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
  {
    inc_mem_copy(tmp_rec, offs, &i, sizeof(transaction_id));

    if (mem_head->t_tbl[i].is_ended)
    {
       LONG_LSN last_lsn = NULL_LSN;
       inc_mem_copy(tmp_rec, offs, &last_lsn, sizeof(LONG_LSN));
    }
    else
    {
       if (mem_head->t_tbl[i].mode == ROLLBACK_MODE)
          inc_mem_copy(tmp_rec, offs, &(mem_head->t_tbl[i].prev_rollback_lsn), sizeof(LONG_LSN));
       else
          inc_mem_copy(tmp_rec, offs, &(mem_head->t_tbl[i].last_lsn), sizeof(LONG_LSN));
    }
  }

  //insert record in shared memory
  logical_log_head log_head;
  log_head.prev_trn_offs = NULL_OFFS;
  log_head.body_len = rec_len;



  //insert log record into shared memory
  writeSharedMemory(&log_head, sizeof(logical_log_head));
  writeSharedMemory(tmp_rec, rec_len);

  mem_head->next_lsn += sizeof(logical_log_head) + rec_len;

  checkpoint_active = true;

  ll_log_flush(false);

  ll_log_unlock(sync);

  //std::cout << "ll_log_checkpoint ret_lsn=" << ret_lsn << endl;;

  return ret_lsn;
/*
  char *tmp_rec;  
  int rec_len;
  char op = LL_CHECKPOINT;
  int num = CHARISMA_MAX_TRNS_NUMBER;
  int offs = 0;
  int ret_lsn;
  transaction_id _trid = -1;

  ll_log_lock(sync);  

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  rec_len = sizeof(char) + sizeof(transaction_id) + sizeof(int) + CHARISMA_MAX_TRNS_NUMBER*(sizeof(LONG_LSN) + sizeof(transaction_id));
  tmp_rec = new char[rec_len];
  ret_lsn = mem_head->next_lsn;

  //obtain identifier of active transaction
  int i;
  for (i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
      if (mem_head->t_tbl[i].last_lsn != NULL_LSN)
      {
         _trid = i;
         d_printf2("transaction id of checkpoint=%d\n", i);
         break;
      }
 
  if (_trid == -1)
     throw SYSTEM_EXCEPTION("there is no any active transaction during checkpoint");     

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &_trid, sizeof(transaction_id));
  inc_mem_copy(tmp_rec, offs, &num, sizeof(int));
  for (i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
  {
    inc_mem_copy(tmp_rec, offs, &i, sizeof(transaction_id));
    inc_mem_copy(tmp_rec, offs, &(mem_head->t_tbl[i].last_lsn), sizeof(LONG_LSN));
  }

  //insert record in shared memory
  ll_log_insert_record(tmp_rec, rec_len, _trid, false);

  delete [] tmp_rec;

  ll_log_unlock(sync);

  return ret_lsn;
*/
}

/*
 indirection log record format: !!! this record appended to the next micro operation
 cl_hint (int)
 num (int) - number of indirection table blocks
 xptr | several blocks, size derived from vector
*/
void llmgr_core::ll_log_indirection(transaction_id trid, int cl_hint, std::vector<xptr>* blocks, bool sync)
{
  int _indir_rec_len;
  int offs = 0;

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  int blocks_len;

  if (blocks == NULL)
     blocks_len = 0;
  else
     blocks_len = sizeof(xptr)* blocks->size();

  _indir_rec_len = sizeof(int) +
                   sizeof(int) +
                   blocks_len;

  if (_indir_rec_len > indir_rec_buf_size)
  {
    if (indir_rec_buf_size > 0) se_delete(indir_rec);
    indir_rec = se_new_cxt(TransactionContext) char[_indir_rec_len];
    indir_rec_buf_size = _indir_rec_len;
  }
//  indir_rec = new char[_indir_rec_len];  
  indir_rec_len = _indir_rec_len;

  //create record body
  int blocks_num = (blocks == NULL) ? 0: blocks-> size();

  inc_mem_copy(indir_rec, offs, &cl_hint, sizeof(int));
  inc_mem_copy(indir_rec, offs, &blocks_num, sizeof(int));

  if (blocks != NULL)
     for (int i=blocks->size() - 1; i >=0; i--)
         inc_mem_copy(indir_rec, offs, &(blocks->at(i)), sizeof(xptr));
}


/*****************************************************************************
                         Helpers (internal functions)
******************************************************************************/


LONG_LSN llmgr_core::ll_log_insert_record(const void* addr, int len, transaction_id &trid, bool sync)
{
  LONG_LSN ret_lsn;
  ll_log_lock(sync);


  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  logical_log_head log_head;
  U_ASSERT(mem_head->keep_bytes >= 0);
  U_ASSERT(mem_head->keep_bytes + sizeof(logical_log_sh_mem_head) <= mem_head->size);

  //check that log file will not be overflowed
  if (LOG_FILE_PORTION_SIZE - (mem_head->next_lsn - (mem_head->base_addr + (mem_head->ll_files_num -1)*LOG_FILE_PORTION_SIZE)) <
	  (sizeof(logical_log_head) + len))

//  if ((mem_head->next_lsn - (mem_head->next_lsn/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE)) < (sizeof(logical_log_head) + len)
  {//current log must be flushed and new file created
     ll_log_flush(false);
     extend_logical_log(false);
  }


  //fill header of the record
  if (mem_head->t_tbl[trid].last_lsn != NULL_LSN)
     log_head.prev_trn_offs = mem_head->next_lsn - mem_head->t_tbl[trid].last_lsn;
  else
     log_head.prev_trn_offs = NULL_OFFS;
  log_head.body_len = len;

  int rec_offs = mem_head->end_offs;

  //insert log record into shared memory
  writeSharedMemory(&log_head, sizeof(logical_log_head));
  writeSharedMemory(addr, len);

  
  //reinit shared memory header (only lsns)
  mem_head->t_tbl[trid].last_rec_mem_offs = rec_offs;
  mem_head->t_tbl[trid].last_lsn = mem_head->next_lsn;

  if (mem_head->t_tbl[trid].first_lsn == NULL_LSN) 
     mem_head->t_tbl[trid].first_lsn = mem_head->next_lsn;

  mem_head->next_lsn+= sizeof(logical_log_head) + len;
  mem_head->t_tbl[trid].num_of_log_records +=1;
  
  ret_lsn = mem_head->t_tbl[trid].last_lsn;

  U_ASSERT(mem_head->keep_bytes >= 0);
  U_ASSERT(mem_head->keep_bytes + sizeof(logical_log_sh_mem_head) <= mem_head->size);

  ll_log_unlock(sync);


//  d_printf4("log record inserted, trid=%d, num=%d\n len=%d\n", trid, mem_head->t_tbl[trid].num_of_log_records, len);

  return ret_lsn;
}


void llmgr_core::writeSharedMemory(const void* rec_addr, int rec_len)
{
  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;

  if ( rec_len > mem_head->free_bytes)
     ll_log_flush(false);

  //now there is enough space of free sh memory
  if ((mem_head->size - mem_head->end_offs) < rec_len)  
  {//record is not contiguous
     memcpy((char*)shared_mem + mem_head->end_offs,
            rec_addr,
            mem_head->size - mem_head->end_offs);

     memcpy((char*)shared_mem + sizeof(logical_log_sh_mem_head),
            (char*)rec_addr + mem_head->size - mem_head->end_offs,
            rec_len - (mem_head->size - mem_head->end_offs));

     //change shmem header
     mem_head->end_offs = sizeof(logical_log_sh_mem_head) +
                          rec_len - (mem_head->size - mem_head->end_offs);
     
     mem_head->free_bytes -= rec_len;
     mem_head->keep_bytes += rec_len;
  }
  else
  {//record is contiguous
     memcpy((char*)shared_mem + mem_head->end_offs,
            rec_addr,
            rec_len);

     //change shmem header
     if ((mem_head->end_offs + rec_len) < mem_head->size)
        mem_head->end_offs += rec_len;
     else
        mem_head->end_offs = sizeof(logical_log_sh_mem_head);

     mem_head->free_bytes -= rec_len;
     mem_head->keep_bytes += rec_len;
  }
}


/*****************************************************************************
                   Flush functions
******************************************************************************/
// this funtion flushes all remaining records on disk
// !!! transaction-owned records should be flushed using ll_log_flush functions !!!
void llmgr_core::ll_log_flush_all_last_records(bool sync)
{
  //d_printf1("flushing of logical log started\n");
  ll_log_lock(sync);

  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;
  char* mem_beg = (char*)shared_mem;

  int rmndr_len;

  // all records have been flushed
  if (mem_head->begin_not_drbl_offs == mem_head->end_offs)
  {
	  ll_log_unlock(sync);
	  return;
  }
  	
  //compute number of bytes to be flushed 
  if (mem_head->begin_not_drbl_offs <= mem_head->end_offs)
     rmndr_len = mem_head->end_offs - mem_head->begin_not_drbl_offs;
  else
     rmndr_len = mem_head->size - sizeof(logical_log_sh_mem_head) - 
                 (mem_head->begin_not_drbl_offs - mem_head->end_offs);
  
  // records are written in logical log in nonatomic manner (first header, then the record itself)
  // here, we can have a situation when we've got only header to flush
  // in this case we must leave it there
  if (rmndr_len == sizeof(logical_log_head))
  {
	  ll_log_unlock(sync);
	  return;
  }
  
  U_ASSERT(mem_head->keep_bytes >= rmndr_len);

  int res;
  int bytes_to_flush = rmndr_len;
  int offs = mem_head->begin_not_drbl_offs;
  int to_write = min3(LOGICAL_LOG_FLUSH_PORTION,
                      rmndr_len,
                      mem_head->size - offs);
  int written, i;

  //set file pointer to the end  
  set_file_pointer(mem_head->next_durable_lsn);


  //d_printf2("need to write bytes=%d\n", rmndr_len);
  //flush needed records;
  while (rmndr_len > 0)
  {
     res = uWriteFile(ll_curr_file_dsc,
                      (char*)shared_mem + offs,
                      to_write,
                      &written,
                      __sys_call_error
                    );

     U_ASSERT(res != 0 && to_write == written);
     if (res == 0 || to_write != written)
       throw SYSTEM_EXCEPTION("Can't write to logical log");

     if ( (offs + to_write) > mem_head->size)
        throw SYSTEM_EXCEPTION("Internal Error in Logical Log");


     if(( offs + to_write) == mem_head->size) 
        offs =  sizeof(logical_log_sh_mem_head);
     else
        offs += to_write;

     rmndr_len-= to_write;
     to_write = min3(LOGICAL_LOG_FLUSH_PORTION,
                     rmndr_len,
                     mem_head->size - offs);
     
  }

  //change sh memory header
  if ((mem_head->size - mem_head->begin_not_drbl_offs) > bytes_to_flush )
    mem_head->begin_not_drbl_offs += bytes_to_flush;
  else
    mem_head->begin_not_drbl_offs = sizeof(logical_log_sh_mem_head) +
                                    bytes_to_flush - 
                                    (mem_head->size - mem_head->begin_not_drbl_offs);

  mem_head->free_bytes += bytes_to_flush;
  mem_head->keep_bytes -= bytes_to_flush;

  mem_head->next_durable_lsn += bytes_to_flush;

  if (checkpoint_active)
  {
        UFile fd = get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]);

  		res = uSetFilePointer(
        		           fd,
                		   sizeof(LONG_LSN),
		                   NULL,
        		           U_FILE_BEGIN,
                		   __sys_call_error
		                 );

		if (res == 0)
		   throw USER_EXCEPTION2(SE4046, "logical log file");

		res = uWriteFile(fd,
                         &(mem_head->next_durable_lsn),
                 		 sizeof(LONG_LSN),
		                 &written,
        		         __sys_call_error
                		);
  
  		if (res == 0 || written != sizeof(LONG_LSN))
   		   throw SYSTEM_EXCEPTION("Can't write to logical log file last commit lsn");

        checkpoint_active = false;
  }
  
  ll_log_unlock(sync);

  //d_printf1("flush of logical log finished\n");
}

void llmgr_core::ll_log_flush(transaction_id trid, bool sync)
{
  //d_printf1("flushing of logical log started\n");
  ll_log_lock(sync);
 

  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;
  char* mem_beg = (char*)shared_mem;

  int rmndr_len;

  if( mem_head->t_tbl[trid].last_rec_mem_offs == NULL_OFFS)
  {
     ll_log_unlock(sync);
     return; //ok all trid records on disk
  }

  //d_printf2("last_rec_mem_offs=%d\n", mem_head->t_tbl[trid].last_rec_mem_offs);
  int last_rec_len;
  //obtaqin last rec len
  if ((mem_head->t_tbl[trid].last_rec_mem_offs + sizeof(logical_log_head)) <= mem_head->size) 
  {
     logical_log_head hd;
     memcpy(&hd, (mem_beg + mem_head->t_tbl[trid].last_rec_mem_offs), sizeof(logical_log_head));
     last_rec_len = sizeof(logical_log_head) + hd.body_len;
  }
  else
  {//record header is not contiguous
   //1. obtain record header
      logical_log_head rec_head;
      int first_portion = mem_head->size - mem_head->t_tbl[trid].last_rec_mem_offs;
      int second_portion = sizeof(logical_log_head) - first_portion;

      memcpy(&rec_head, mem_beg + mem_head->t_tbl[trid].last_rec_mem_offs, first_portion);
      memcpy(((char*)(&rec_head))+first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);   

      last_rec_len = sizeof(logical_log_head) + rec_head.body_len; 
  }

  //check that there are records of the transaction which are not fluhed
  if ( (mem_head->t_tbl[trid].last_lsn + last_rec_len) <= mem_head->next_durable_lsn)
  {//case when all records of the transaction already flushed by means of flush calls for another transactions
      mem_head->t_tbl[trid].last_rec_mem_offs = NULL_OFFS;
	  ll_log_unlock(sync);
	  return;
  }


  //compute number of bytes to be flushed 
  if (mem_head->begin_not_drbl_offs < (mem_head->t_tbl[trid].last_rec_mem_offs + last_rec_len))
     rmndr_len = mem_head->t_tbl[trid].last_rec_mem_offs + last_rec_len - 
                 mem_head->begin_not_drbl_offs;
  else
     rmndr_len = mem_head->size - sizeof(logical_log_sh_mem_head) - 
                 (mem_head->begin_not_drbl_offs - (mem_head->t_tbl[trid].last_rec_mem_offs + last_rec_len));

  
  int res;
  int bytes_to_flush = rmndr_len;
  int offs = mem_head->begin_not_drbl_offs;
  int to_write = min3(LOGICAL_LOG_FLUSH_PORTION,
                      rmndr_len,
                      mem_head->size - offs);
  int written, i;

  //set file pointer to the end  
  set_file_pointer(mem_head->next_durable_lsn);


  //d_printf2("need to write bytes=%d\n", rmndr_len);
  //flush needed records;
  while (rmndr_len > 0)
  {
     res = uWriteFile(ll_curr_file_dsc,
                      (char*)shared_mem + offs,
                      to_write,
                      &written,
                      __sys_call_error
                    );

     U_ASSERT(res != 0 && to_write == written);
     if (res == 0 || to_write != written)
       throw SYSTEM_EXCEPTION("Can't write to logical log");

     if ( (offs + to_write) > mem_head->size)
        throw SYSTEM_EXCEPTION("Internal Error in Logical Log");


     if(( offs + to_write) == mem_head->size) 
        offs =  sizeof(logical_log_sh_mem_head);
     else
        offs += to_write;

     rmndr_len-= to_write;
     to_write = min3(LOGICAL_LOG_FLUSH_PORTION,
                     rmndr_len,
                     mem_head->size - offs);
     
  }

  //change sh memory header
  if ((mem_head->size - mem_head->begin_not_drbl_offs) > bytes_to_flush )
    mem_head->begin_not_drbl_offs += bytes_to_flush;
  else
    mem_head->begin_not_drbl_offs = sizeof(logical_log_sh_mem_head) +
                                    bytes_to_flush - 
                                    (mem_head->size - mem_head->begin_not_drbl_offs);

  mem_head->free_bytes += bytes_to_flush;
  mem_head->keep_bytes -= bytes_to_flush;
  mem_head->t_tbl[trid].last_rec_mem_offs = NULL_OFFS;

  for (i=0; i<CHARISMA_MAX_TRNS_NUMBER; i++)
  {
     if (i != trid && mem_head->t_tbl[i].last_lsn <= mem_head->t_tbl[trid].last_lsn)
        mem_head->t_tbl[i].last_rec_mem_offs = NULL_OFFS;
        
  }
  mem_head->next_durable_lsn += bytes_to_flush;

  ll_log_unlock(sync);

  //d_printf1("flush of logical log finished\n");
}


//this function flushes last record in bufer
//it is only used when all records excepting last one are flushed on disk
//we use it to flush checkpoint record which does not correspond to any transaction. Yhus it can'be 
//flushed by ll_log_flush() method
void llmgr_core::ll_log_flush_last_record(bool sync)
{
  ll_log_lock(sync);
  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;
  char* mem_beg = (char*)shared_mem;

  int rmndr_len;


  //d_printf2("last_rec_mem_offs=%d\n", mem_head->t_tbl[trid].last_rec_mem_offs);
  int last_rec_len;

  if ((mem_head->begin_not_drbl_offs + sizeof(logical_log_head)) <= mem_head->size) 
      last_rec_len = sizeof(logical_log_head) +
                     ((logical_log_head*)(mem_beg + mem_head->begin_not_drbl_offs))->body_len;
  else
  {//record header is not contiguous
   //1. obtain record header
      logical_log_head rec_head;
      int first_portion = mem_head->size - mem_head->begin_not_drbl_offs;
      int second_portion = sizeof(logical_log_head) - first_portion;

      memcpy(&rec_head, mem_beg + mem_head->begin_not_drbl_offs, first_portion);
      memcpy(((char*)(&rec_head))+first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);   

      last_rec_len = sizeof(logical_log_head) + rec_head.body_len; 
  }


  rmndr_len = last_rec_len;

  
  int res;
  int bytes_to_flush = rmndr_len;
  int offs = mem_head->begin_not_drbl_offs;
  int to_write = min3(LOGICAL_LOG_FLUSH_PORTION,
                      rmndr_len,
                      mem_head->size - offs);
  int written;

  //set file pointer to the end  
  set_file_pointer(mem_head->next_durable_lsn);


  //d_printf2("need to write bytes=%d\n", rmndr_len);
  //flush needed records;
  while (rmndr_len > 0)
  {
     res = uWriteFile(ll_curr_file_dsc,
                      (char*)shared_mem + offs,
                      to_write,
                      &written,
                      __sys_call_error
                    );
     if (res == 0 || to_write != written)
       throw SYSTEM_EXCEPTION("Can't write to logical log");

     if ( (offs + to_write) > mem_head->size)
        throw SYSTEM_EXCEPTION("Internal Error in Logical Log");


     if(( offs + to_write) == mem_head->size) 
        offs =  sizeof(logical_log_sh_mem_head);
     else
        offs += to_write;

     rmndr_len-= to_write;
     to_write = min3(LOGICAL_LOG_FLUSH_PORTION,
                     rmndr_len,
                     mem_head->size - offs);
     
  }

  //change sh memory header
  if ((mem_head->size - mem_head->begin_not_drbl_offs) > bytes_to_flush )
    mem_head->begin_not_drbl_offs += bytes_to_flush;
  else
    mem_head->begin_not_drbl_offs = sizeof(logical_log_sh_mem_head) +
                                    bytes_to_flush - 
                                    (mem_head->size - mem_head->begin_not_drbl_offs);
                   
  mem_head->free_bytes += bytes_to_flush;
  mem_head->keep_bytes -= bytes_to_flush;
  mem_head->next_durable_lsn += bytes_to_flush;


  ///!!! Here flush next lsn field in header 
  UFile fd = get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]);

  res = uSetFilePointer(
                   fd,
                   sizeof(LONG_LSN),
                   NULL,
                   U_FILE_BEGIN,
                   __sys_call_error
                 );

  if (res == 0)
     throw USER_EXCEPTION2(SE4046, "logical log file");

  res = uWriteFile(fd,
                   &(mem_head->next_durable_lsn),
                   sizeof(LONG_LSN),
                   &written,
                   __sys_call_error
                    );
  if (res == 0 || written != sizeof(LONG_LSN))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last commit lsn");

  

  ll_log_unlock(sync);
}

void llmgr_core::ll_log_flush(bool sync)
{//the implementation of this method is not correct, because
 //in logical log can be a last not completed record, it will
 // not be flushed
  ll_log_lock(sync);

  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;  

  for (int i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
  {
      ll_log_flush(i, false);
      mem_head->t_tbl[i].last_rec_mem_offs = NULL_OFFS;
  }

  //there was a bug here, because we could get buffer filled with rogue rollback records
  //the above logic doesn't flush this records since corresponding transactions are not in t_tbl anymore
  //another possible bug: flushing only transaction-owned records might be not enough to free the needed space
  //fixed: implemented "true" flushing function, which flushes all last independent records also
  ll_log_flush_all_last_records(false); // flush all last records (not transaction-owned)

  ll_log_unlock(sync);
}



/*****************************************************************************
                    UNDO, REDO functions
******************************************************************************/
//function is used for onlne rollback

void llmgr_core::rollback_trn(transaction_id &trid, void (*exec_micro_op_func) (const char*, int, bool), bool sync)
{//before this call all log records are completed
  ll_log_lock(sync);

  rollback_active = true;

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  char * mem_beg = (char*)shared_mem;
  const char * rec_beg; 
  logical_log_head* log_head; 

  mem_head->t_tbl[trid].mode = ROLLBACK_MODE;

  if ( mem_head->t_tbl[trid].last_lsn == NULL_LSN )
  {//ok such transaction already rolled back
     rollback_active = false;
     ll_log_unlock(sync);
     return;
  }

  ll_log_flush(trid, false);
  ll_log_unlock(sync);

  //there is at least one record for tr to be rolled back
  LONG_LSN lsn;

  lsn = mem_head->t_tbl[trid].last_lsn;
  rec_beg = get_record_from_disk(lsn);
  log_head = (logical_log_head*)rec_beg;
  //cout << "rollback record lsn=" << lsn << endl;

//#ifdef LOGICAL_LOG_TEST
  int i=1;
//#endif
  while(true)
  {
//#ifdef LOGICAL_LOG_TEST
	//  d_printf3("record number=%d\n",i);
//	 cout << "rollback lsn=" << lsn << endl;
	 i++;
//#endif
     if (log_head->prev_trn_offs == NULL_LSN)
        set_hint_lsn_for_prev_rollback_record(trid, (LONG_LSN)NULL_LSN);
     else
        set_hint_lsn_for_prev_rollback_record(trid, (LONG_LSN)(lsn - log_head->prev_trn_offs));

     exec_micro_op_func(rec_beg + sizeof(logical_log_head),
                        ((logical_log_head*)rec_beg)->body_len,

						true);
     if (log_head->prev_trn_offs == NULL_OFFS)
        break;//all operations rolled back



     lsn -= log_head->prev_trn_offs;
     rec_beg = get_record_from_disk(lsn);
     log_head = (logical_log_head*)rec_beg;

  }

  rollback_active = false;
//  ll_log_unlock(sync);
}

/*
//function is used for onlne rollback
void llmgr_core::rollback_trn(transaction_id &trid, void (*exec_micro_op_func) (const char*, int, bool), bool sync)
{//before this call all log records are completed
  ll_log_lock(sync);

  rollback_active = true;

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  char * mem_beg = (char*)shared_mem;
  const char * rec_beg; 
  logical_log_head* log_head; 


  if ( mem_head->t_tbl[trid].last_lsn == NULL_LSN )
  {//ok such transaction already rolled back
     rollback_active = false;
     ll_log_unlock(sync);
     return;
  }


  //there is at least one record for tr to be rolled back
  LONG_LSN lsn;
  int offs;
  bool InShMem;
  int rmndr_mem_len;
 
  if (mem_head->t_tbl[trid].last_rec_mem_offs != NULL_OFFS)
  {
     offs = mem_head->t_tbl[trid].last_rec_mem_offs;
     rec_beg = mem_beg + offs;
     InShMem = true;
     log_head = (logical_log_head*)rec_beg;
     if(mem_head->begin_not_drbl_offs <= mem_head->t_tbl[trid].last_rec_mem_offs)
        rmndr_mem_len = mem_head->t_tbl[trid].last_rec_mem_offs - mem_head->begin_not_drbl_offs;
	 else
        rmndr_mem_len = mem_head->size - sizeof(logical_log_sh_mem_head) -
                        (mem_head->begin_not_drbl_offs - mem_head->t_tbl[trid].last_rec_mem_offs);	    
  }
  else
  {
     rec_beg = get_record_from_disk(lsn);
     InShMem = false;
     log_head = (logical_log_head*)rec_beg;
	 rmndr_mem_len = 0;
  }

  lsn = mem_head->t_tbl[trid].last_lsn;
  //cout << "rollback record lsn=" << lsn << endl;

//#ifdef LOGICAL_LOG_TEST
  int i=1;
//#endif
  while(true)
  {
//#ifdef LOGICAL_LOG_TEST
	  //d_printf2("record number=%d\n",i);
	  i++;
//#endif
     exec_micro_op_func(rec_beg + sizeof(logical_log_head),
                        ((logical_log_head*)rec_beg)->body_len,

						true);
     if (log_head->prev_trn_offs == NULL_OFFS)
        break;//all operations rolled back

     if( InShMem && rmndr_mem_len >= log_head->prev_trn_offs)
     {//next record in shared memory
        if ( (offs - log_head->prev_trn_offs) >= (int)sizeof(logical_log_sh_mem_head))//next record is contiguous
		{
           offs -= log_head->prev_trn_offs;
           rec_beg = mem_beg + offs;
		}
		else//next record is not contiguous
		{
		   rec_beg = get_record_from_shared_memory(offs, log_head->prev_trn_offs);

           offs = mem_head->size - (log_head->prev_trn_offs -(offs - sizeof(logical_log_sh_mem_head)));

		}			 
        rmndr_mem_len -= log_head->prev_trn_offs;
		lsn -= log_head->prev_trn_offs;
        InShMem = true;
        log_head = (logical_log_head*)rec_beg;
	 }
     else
     {//next record on disk

        lsn -= log_head->prev_trn_offs;
        rec_beg = get_record_from_disk(lsn);
        InShMem = false;
        log_head = (logical_log_head*)rec_beg;
        rmndr_mem_len = 0;
     }

//     cout << "rollback record lsn=" << lsn << endl;

  }

  rollback_active = false;
  ll_log_unlock(sync);
}
*/

//this function is run from the special recovery process
#ifdef SE_ENABLE_FTSEARCH
void llmgr_core::recover_db_by_logical_log(
void (*index_op) (const trns_undo_analysis_list&, const trns_redo_analysis_list&, const LONG_LSN&),
					   void (*exec_micro_op) (const char*, int, bool),
                                           void(*switch_indirection)(int),
                                           void (*_vmm_rcv_add_to_indir_block_set_)(xptr p),
                                           void (*_vmm_rcv_clear_indir_block_set_)(),
                                           void (*_sync_indirection_table_)(),
                                           const LONG_LSN& last_cp_lsn,
                                           int undo_indir_mode,
                                           int redo_indir_mode,
                                           bool sync)
#else
void llmgr_core::recover_db_by_logical_log(void (*exec_micro_op) (const char*, int, bool),
                                           void(*switch_indirection)(int),
                                           void (*_vmm_rcv_add_to_indir_block_set_)(xptr p),
                                           void (*_vmm_rcv_clear_indir_block_set_)(),
                                           void (*_sync_indirection_table_)(),
                                           const LONG_LSN& last_cp_lsn,
                                           int undo_indir_mode,
                                           int redo_indir_mode,
                                           bool sync)
#endif
{
  ll_log_lock(sync);
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  logical_log_file_head file_head =
                  read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  LONG_LSN last_checkpoint_lsn = last_cp_lsn;
  LONG_LSN last_commit_lsn = file_head.last_commit_lsn;

  //cout << "last_checkpoint_lsn=" << last_checkpoint_lsn << "\n";
  //cout << "last_commit_lsn=" << last_commit_lsn << "\n";

//  d_printf2("last_checkpoint_lsn=%d, last_commit_lsn=%d\n", last_checkpoint_lsn, last_commit_lsn);
  string str = string("llmgr_core::recover_db_by_logical_log last_checkpoint_lsn=") + int2string(last_checkpoint_lsn) + string("\n");
  str += string("llmgr_core::recover_db_by_logical_log last_commit_lsn=") + int2string(last_commit_lsn) + string("\n");
  WRITE_DEBUG_LOG(str.c_str());


  if (last_checkpoint_lsn == NULL_LSN && last_commit_lsn == NULL_LSN) 
  { 
     //none transactions have been commited and there was not checkpoints
     //then recovery by physical log quite enough
     close_all_log_files();
     ll_log_unlock(sync);
     return;//all already recovered
  }

//  trns_analysis_map undo_redo_trns_map;


  trns_undo_analysis_list undo_list;
  trns_redo_analysis_list redo_list;
 
#ifdef EL_DEBUG
#if (EL_DEBUG == 1)
  cout << "last_cp_lsn=" << last_checkpoint_lsn << "last_commit_lsn=" << last_commit_lsn << endl;
#endif
#endif

//  d_printf3("get_undo_redo_trns_map last_cp_lsn=%lld, last_commit_lsn=%lld\n", last_checkpoint_lsn, last_commit_lsn);
  get_undo_redo_trns_list(last_checkpoint_lsn,
                          last_commit_lsn,
                          undo_list, /*out*/
                          redo_list, /*out*/
                          _vmm_rcv_add_to_indir_block_set_
                         );


  trns_undo_analysis_list_iterator it;

#ifdef EL_DEBUG
#if (EL_DEBUG == 1)
  for (it = undo_list.begin(); it != undo_list.end(); it++)
  {
//       std::cout << "trid=" << it->trid << " trn_undo_rcv_lsn=" << it->trn_undo_rcv_lsn;     
     std::cout << "trid=" << it->trid <<  " trn_undo_rcv_lsn=" << it->trn_undo_rcv_lsn << "first_lsn_after_cp=" << it->first_lsn_after_cp << " finish_status=" << it->finish_status << endl;
  }
#endif
#endif


  trns_redo_analysis_list_iterator it2;

#ifdef EL_DEBUG
#if (EL_DEBUG == 1)
  for (it2 = redo_list.begin(); it2 != redo_list.end(); it2++)
  {
     std::cout << "trid=" << it2->trid << ", " << " trn_start_rcv_lsn=" << it2->trn_start_rcv_lsn << "trn_end_lsn=" << it2->trn_end_lsn <<  " finish_status=" << it2->finish_status << endl;
  }
#endif
#endif


  switch_indirection(undo_indir_mode);

  d_printf2("undo loseres num=%d\n", undo_list.size());
  //undo losers
  trns_undo_analysis_list_iterator it1;
  for(it1=undo_list.begin(); it1 != undo_list.end(); it1++)
     undo_trn(it1->trn_undo_rcv_lsn, exec_micro_op);

  _sync_indirection_table_();

  //redo committed transactions
  LONG_LSN start_redo_lsn;
  const char* checkpoint_rec = get_record_from_disk(last_checkpoint_lsn);

  get_record_length(checkpoint_rec);
  start_redo_lsn = (last_checkpoint_lsn != NULL_LSN) ? last_checkpoint_lsn + get_record_length(checkpoint_rec) :
                                                       sizeof(logical_log_file_head);


  switch_indirection(redo_indir_mode);

  d_printf2("redo list size=%d\n", redo_list.size());

  redo_commit_trns(redo_list, 
                   start_redo_lsn,
                   last_commit_lsn,
                   exec_micro_op);
  
  
#ifdef SE_ENABLE_FTSEARCH
  index_op(undo_list, redo_list, last_checkpoint_lsn);
#endif

  _vmm_rcv_clear_indir_block_set_();

  //close all open log files
  close_all_log_files();
  ll_log_unlock(sync);
}
void llmgr_core::undo_trn(LONG_LSN& start_undo_lsn, void (*_exec_micro_op_) (const char*, int, bool))
{
  LONG_LSN lsn = start_undo_lsn;
  const char* rec;

  if (start_undo_lsn == NULL_LSN)
	  return; 

  do
  {
     rec = get_record_from_disk(lsn);

#ifdef RECOVERY_EXEC_MICRO_OP
     _exec_micro_op_(rec + sizeof(logical_log_head), get_record_length(rec) - sizeof(logical_log_head), true);
#endif
   
     lsn-= ((logical_log_head*)rec)->prev_trn_offs;

  } while(((logical_log_head*)rec)->prev_trn_offs != NULL_OFFS);

}

void llmgr_core::redo_commit_trns(trns_redo_analysis_list& redo_list, LONG_LSN &start_lsn, LONG_LSN &end_lsn, void (*_exec_micro_op_) (const char*, int, bool))
{
  if (redo_list.empty()) return;


  if (start_lsn > end_lsn )
     throw USER_EXCEPTION(SE4152);

  LONG_LSN lsn = start_lsn;
  const char *rec;
  int body_len;
//  trns_analysis_map::iterator it; 
 
  int i=1;
  __int64 file_size;
  trn_cell_analysis_redo redo_trn_cell(-1, NULL_LSN, NULL_LSN);
  bool res;

  do
  {
    set_file_pointer(lsn);
    if( uGetFileSize(ll_curr_file_dsc, &file_size, __sys_call_error) == 0)
       throw SYSTEM_EXCEPTION("Can't get file size");

	if ((lsn%LOG_FILE_PORTION_SIZE) == file_size)//here we must reinit lsn
      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);

    rec = get_record_from_disk(lsn);
    body_len = ((logical_log_head*)rec)->body_len;

    if ((rec + sizeof(logical_log_head))[0] != LL_COMMIT &&
        (rec + sizeof(logical_log_head))[0] != LL_ROLLBACK &&
        (rec + sizeof(logical_log_head))[0] != LL_CHECKPOINT)
    {

      //this function tries to find transaction which start_lsn <=lsn <=end_lsn
      res = find_redo_trn_cell(*((transaction_id*)((rec + sizeof(logical_log_head))+sizeof(char))),
                               redo_list,
                               lsn,
                               redo_trn_cell/*out*/);

      if (res)
      {

         if (redo_trn_cell.finish_status != TRN_COMMIT_FINISHED ||
             redo_trn_cell.trn_end_lsn == NULL_LSN || redo_trn_cell.trn_start_rcv_lsn== NULL_LSN ||
             redo_trn_cell.trn_start_rcv_lsn > redo_trn_cell.trn_end_lsn) 
            throw SYSTEM_EXCEPTION("Incorrect status|end_lsn|start_lsn of transaction to be redone");



#ifdef RECOVERY_EXEC_MICRO_OP
         _exec_micro_op_(rec + sizeof(logical_log_head), body_len, false);
#else 
        ;
#endif
      }
	  // FOR DEBUG
      //d_printf4("exec log record num=%d len=%d, trid=%d\n", i, body_len, *((transaction_id*)((rec + sizeof(logical_log_head))+sizeof(char))));
    }
        
    lsn += sizeof(logical_log_head) + body_len;
	i++;

  } while(lsn <= end_lsn);
}

void llmgr_core::get_undo_redo_trns_list(LONG_LSN &start_lsn,
                                        LONG_LSN &end_lsn,
                                        trns_undo_analysis_list& undo_list, /*out*/
                                        trns_redo_analysis_list& redo_list, /*out*/
                                        void (*_vmm_rcv_add_to_indir_block_set_)(xptr p)
                                       )
{
  LONG_LSN lsn;
  const char *rec;
  const char *body_beg;
  trns_undo_analysis_list _undo_list_;
  trns_redo_analysis_list _redo_list_;
  LONG_LSN next_lsn_after_cp;


  //init trns_map from checkpoint record

  if (start_lsn == NULL_LSN)//there is no checkpoint -> pass from log begin
     lsn = sizeof(logical_log_file_head);
  else
  {
    rec = get_record_from_disk(start_lsn);
    body_beg = rec + sizeof(logical_log_head);
    
    if (body_beg[0] != LL_CHECKPOINT)
       throw USER_EXCEPTION(SE4153);

    //init map from checkpoint
    int num = *((int*)(body_beg + sizeof(char)));

    for (int i=0; i< num; i++)
    {
       transaction_id *trid;
       LONG_LSN *last_lsn;

       trid = (transaction_id*)(body_beg + sizeof(char) + sizeof(int) + i*(sizeof(transaction_id) + sizeof(LONG_LSN)));
       last_lsn = (LONG_LSN*)(body_beg + sizeof(char) + sizeof(int) + i*(sizeof(transaction_id) + sizeof(LONG_LSN)) + sizeof(transaction_id));
       if (*last_lsn != NULL_LSN)_undo_list_.push_back(trn_cell_analysis_undo(*trid, *last_lsn));

//       trns_map.insert(trn_pair(*trid, trn_cell_analysis(0, *last_lsn)));
    }

    lsn = start_lsn + get_record_length(rec);    
  }

  next_lsn_after_cp = lsn;

  //pass the log
//  trns_analysis_map trns_map_after_checkpoint;//map of transactions began after checkpoint
//  trns_analysis_map::iterator it;
  __int64 file_size;

  while (lsn <= end_lsn)
  {
    //obtain lsn of next record (it must be not contiguous if next record is located in next log file)
    set_file_pointer(lsn);
    if( uGetFileSize(ll_curr_file_dsc, &file_size, __sys_call_error) == 0)
       throw SYSTEM_EXCEPTION("Can't get file size");

	if ((lsn%LOG_FILE_PORTION_SIZE) == file_size)//here we must reinit lsn
      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);

 
    rec = get_record_from_disk(lsn);
    body_beg = rec + sizeof(logical_log_head);

    if (body_beg[0] == LL_CHECKPOINT)
       throw USER_EXCEPTION(SE4153);

    transaction_id *trid;
    trid = (transaction_id*)(body_beg + sizeof(char));
    trn_cell_analysis_undo undo_trn_cell(-1, NULL_LSN);
    trn_cell_analysis_redo redo_trn_cell(-1, NULL_LSN, NULL_LSN);
    bool res1, res2;


    if (body_beg[0] == LL_INSERT_ELEM || body_beg[0] == LL_DELETE_ELEM ||
        body_beg[0] == LL_INDIR_INSERT_ELEM || body_beg[0] == LL_INDIR_DELETE_ELEM ||
        body_beg[0] == LL_INSERT_ATTR || body_beg[0] == LL_DELETE_ATTR ||
        body_beg[0] == LL_INDIR_INSERT_ATTR || body_beg[0] == LL_INDIR_DELETE_ATTR ||
        body_beg[0] == LL_INSERT_TEXT || body_beg[0] == LL_DELETE_TEXT ||
        body_beg[0] == LL_INDIR_INSERT_TEXT || body_beg[0] == LL_INDIR_DELETE_TEXT ||
        body_beg[0] == LL_INSERT_LEFT_TEXT || body_beg[0] == LL_DELETE_LEFT_TEXT ||
        body_beg[0] == LL_INSERT_RIGHT_TEXT || body_beg[0] == LL_DELETE_RIGHT_TEXT ||
        body_beg[0] == LL_INSERT_DOC  || body_beg[0] == LL_DELETE_DOC  ||
        body_beg[0] == LL_INDIR_INSERT_DOC  || body_beg[0] == LL_INDIR_DELETE_DOC  ||
        body_beg[0] == LL_INSERT_COMMENT || body_beg[0] == LL_DELETE_COMMENT ||
        body_beg[0] == LL_INDIR_INSERT_COMMENT || body_beg[0] == LL_INDIR_DELETE_COMMENT ||
        body_beg[0] == LL_INSERT_PI   || body_beg[0] == LL_DELETE_PI ||
        body_beg[0] == LL_INDIR_INSERT_PI   || body_beg[0] == LL_INDIR_DELETE_PI ||
        body_beg[0] == LL_INSERT_COLLECTION || body_beg[0] == LL_DELETE_COLLECTION ||
        body_beg[0] == LL_INDIR_INSERT_COLLECTION || body_beg[0] == LL_INDIR_DELETE_COLLECTION ||
        body_beg[0] == LL_INSERT_NS   || body_beg[0] == LL_DELETE_NS ||
        body_beg[0] == LL_INDIR_INSERT_NS   || body_beg[0] == LL_INDIR_DELETE_NS ||
        body_beg[0] == LL_INSERT_DOC_INDEX  || body_beg[0] == LL_DELETE_DOC_INDEX ||
        body_beg[0] == LL_INSERT_COL_INDEX  || body_beg[0] == LL_DELETE_COL_INDEX ||
        body_beg[0] == LL_INSERT_DOC_FTS_INDEX  || body_beg[0] == LL_DELETE_DOC_FTS_INDEX ||
        body_beg[0] == LL_INSERT_COL_FTS_INDEX  || body_beg[0] == LL_DELETE_COL_FTS_INDEX)

    {
      res1 = find_undo_trn_cell(*trid, _undo_list_, undo_trn_cell/*out*/);
      if (res1 && undo_trn_cell.finish_status == TRN_NOT_FINISHED)
      {
         if (undo_trn_cell.first_lsn_after_cp == NULL_LSN)
         {
            undo_trn_cell.first_lsn_after_cp = lsn;
            set_undo_trn_cell(*trid, _undo_list_, undo_trn_cell);
         }
      }
      else
      {//need to check in redo list 
         
         res2 = find_last_redo_trn_cell(*trid, _redo_list_, redo_trn_cell/*out*/);
         if (!res2 || redo_trn_cell.finish_status != TRN_NOT_FINISHED)
         {//first record of transaction
            _redo_list_.push_back(trn_cell_analysis_redo(*trid, lsn, NULL_LSN));
         
         }
         
      }

     
      if (
          body_beg[0] == LL_INDIR_INSERT_ELEM || body_beg[0] == LL_INDIR_DELETE_ELEM ||
          body_beg[0] == LL_INDIR_INSERT_ATTR || body_beg[0] == LL_INDIR_DELETE_ATTR ||
          body_beg[0] == LL_INDIR_INSERT_TEXT || body_beg[0] == LL_INDIR_DELETE_TEXT ||
          body_beg[0] == LL_INDIR_INSERT_DOC  || body_beg[0] == LL_INDIR_DELETE_DOC  ||
          body_beg[0] == LL_INDIR_INSERT_COMMENT || body_beg[0] == LL_INDIR_DELETE_COMMENT ||
          body_beg[0] == LL_INDIR_INSERT_PI   || body_beg[0] == LL_INDIR_DELETE_PI ||
          body_beg[0] == LL_INDIR_INSERT_COLLECTION || body_beg[0] == LL_INDIR_DELETE_COLLECTION ||
          body_beg[0] == LL_INDIR_INSERT_NS   || body_beg[0] == LL_INDIR_DELETE_NS)
       //append new indirection blocks
      {
          int offs = sizeof(char) + sizeof(transaction_id);
          int *cl_hint;
          int *blocks_num;

          cl_hint = (int*)(body_beg + offs);
          offs += sizeof(int);
          blocks_num = (int*)(body_beg + offs);
          offs += sizeof(int);

          for (int i = 0; i< *blocks_num; i++)
          {
              _vmm_rcv_add_to_indir_block_set_(*((xptr*)(body_beg + offs)));
              offs += sizeof(xptr);
          }
      }

    }
    else
    if (body_beg[0] == LL_COMMIT)//may be transaction with only one commit record in te log
    {
      res1 = find_undo_trn_cell(*trid, _undo_list_, undo_trn_cell/*out*/);
      
      if (res1 && undo_trn_cell.finish_status == TRN_NOT_FINISHED)
      {

         undo_trn_cell.finish_status = TRN_COMMIT_FINISHED;
         set_undo_trn_cell(*trid, _undo_list_, undo_trn_cell); 

         if (undo_trn_cell.first_lsn_after_cp != NULL_LSN)//we need to redo at least one record for this transaction after checkpoint
            _redo_list_.push_front(trn_cell_analysis_redo(*trid, next_lsn_after_cp, lsn, TRN_COMMIT_FINISHED));

      }
      else
      {
         res2 = find_last_redo_trn_cell(*trid, _redo_list_, redo_trn_cell/*out*/);
         if (res2 && redo_trn_cell.finish_status == TRN_NOT_FINISHED)
         {
             redo_trn_cell.finish_status =  TRN_COMMIT_FINISHED;
             redo_trn_cell.trn_end_lsn = lsn;
             set_last_redo_trn_cell(*trid, _redo_list_, redo_trn_cell);
         }
      }
    }
    else 
    if (body_beg[0] == LL_ROLLBACK)
    {
      res1 = find_undo_trn_cell(*trid, _undo_list_, undo_trn_cell/*out*/);
      
      if (res1 && undo_trn_cell.finish_status == TRN_NOT_FINISHED)
      {
         undo_trn_cell.finish_status = TRN_ROLLBACK_FINISHED;
         set_undo_trn_cell(*trid, _undo_list_, undo_trn_cell); 
      }
      else
      {
         res2 = find_last_redo_trn_cell(*trid, _redo_list_, redo_trn_cell/*out*/);
         if (res2 && redo_trn_cell.finish_status == TRN_NOT_FINISHED)
         {
             redo_trn_cell.finish_status = TRN_ROLLBACK_FINISHED;
             redo_trn_cell.trn_end_lsn = lsn;
             set_last_redo_trn_cell(*trid, _redo_list_, redo_trn_cell);
         }
      }

    }
    else
      throw USER_EXCEPTION(SE4154);

    lsn += get_record_length(rec);
  }


  //init undo list (out paprameter)
  trns_undo_analysis_list_iterator it1;
  for (it1 = _undo_list_.begin(); it1 != _undo_list_.end(); it1++)
  {
    if (it1->finish_status != TRN_COMMIT_FINISHED) undo_list.push_back(*it1);
  }

  //init redo list (out paprameter)
  trns_redo_analysis_list_iterator it2;
  for (it2 = _redo_list_.begin(); it2 != _redo_list_.end(); it2++)
  {
    if (it2->finish_status == TRN_NOT_FINISHED || it2->finish_status == TRN_ROLLBACK_FINISHED) continue;
    else if (it2->finish_status == TRN_COMMIT_FINISHED)  redo_list.push_back(*it2);
    else throw SYSTEM_EXCEPTION("Unpredictable finish status of transaction");
  }
}



int llmgr_core::get_num_of_records_written_by_trn(transaction_id &trid)
{
   //synchronization is not needed
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  return mem_head->t_tbl[trid].num_of_log_records; 

}

void llmgr_core::commit_trn(transaction_id& trid, bool sync)
{
  ll_log_lock(sync);

  LONG_LSN commit_lsn = ll_log_commit(trid, false);
  ll_log_flush(trid, false);
  flush_last_commit_lsn(commit_lsn);//writes to the logical log file header lsn of last commited function

  ll_log_unlock(sync);
}

void llmgr_core::ll_truncate_log(bool sync)
{
  ll_log_lock(sync);

  //execute minLSN
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  LONG_LSN minLSN = NULL_LSN;
  int i;

  for(i = 0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
  {
     if (minLSN == NULL_LSN) 
        minLSN = mem_head->t_tbl[i].first_lsn;
     else
     {
        if (mem_head->t_tbl[i].first_lsn != NULL_LSN && mem_head->t_tbl[i].first_lsn < minLSN)
           minLSN = mem_head->t_tbl[i].first_lsn;
     }
  }

  //determine files to be trancated
  int num_files_to_truncate;
  if (minLSN == NULL_LSN)
     num_files_to_truncate = mem_head->ll_files_num - 1;
  else
    num_files_to_truncate = (minLSN - mem_head->base_addr)/LOG_FILE_PORTION_SIZE;

  if (num_files_to_truncate  == 0)
  {
	  ll_log_unlock(sync);
	  return;
  }

  if (num_files_to_truncate < 0) throw SYSTEM_EXCEPTION("Incorrect number of files to truncate");
  //!!!!rewrite base_addr and valid_number atomically
  __int64 new_base_addr = mem_head->base_addr + num_files_to_truncate*LOG_FILE_PORTION_SIZE;
  int valid_number = mem_head->ll_files_arr[num_files_to_truncate];
  
  //set file pointer to header
  LONG_LSN lsn = ((mem_head->next_lsn / LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE);

  set_file_pointer(lsn);

  logical_log_file_head file_head = read_log_file_header(ll_curr_file_dsc);
  set_file_pointer(lsn);//pos to the begin of file
  file_head.base_addr = new_base_addr;
  file_head.valid_number = valid_number;
  file_head.next_lsn = mem_head->next_lsn;


  int res;
  int written;

  //write file header
  res = uWriteFile(ll_curr_file_dsc,
                   &file_head,
                   sizeof(logical_log_file_head),
                   &written,
                    __sys_call_error
                    );
  if (res == 0 || written != sizeof(logical_log_file_head))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last commit lsn");


  //!!!Delete unnecceary files
  close_all_log_files();
  std::string log_file_name;
  char buf2[20];
  for (i=0; i< num_files_to_truncate; i++)
  {
      log_file_name = db_files_path + db_name + "." + u_itoa(mem_head->ll_files_arr[i], buf2, 10) + "llog";
      if (uDeleteFile(log_file_name.c_str(), __sys_call_error) == 0)
      {
//         d_printf3("Delete File=%s, Error=%d\n", log_file_name.c_str(), GetLastError());
         throw USER_EXCEPTION2(SE4041, log_file_name.c_str());
      }
  }

  //reinit memory header
  memcpy((char*)mem_head->ll_free_files_arr + mem_head->ll_free_files_num * sizeof(int),
         (char*)mem_head->ll_files_arr,
         num_files_to_truncate*sizeof(int));

  memcpy((char*)mem_head->ll_files_arr,
         (char*)mem_head->ll_files_arr+num_files_to_truncate*sizeof(int),
         (mem_head->ll_files_num - num_files_to_truncate)*sizeof(int));

  mem_head->ll_files_num -= num_files_to_truncate;
  mem_head->ll_free_files_num += num_files_to_truncate;
  mem_head->base_addr = new_base_addr;

  close_all_log_files();
  

  ll_log_unlock(sync);
}


void llmgr_core::activate_checkpoint()
{
    //here phys_log_mgr is a global variable
    _phys_log_mgr_->activate_checkpoint(true);
}

void llmgr_core::set_prev_rollback_lsn(transaction_id &trid, bool sync)
{
  //!!!The acquire of sem is not needed since I don't read here shared variables!!!
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  
  if ( mem_head->t_tbl[trid].mode == ROLLBACK_MODE)
      mem_head->t_tbl[trid].prev_rollback_lsn = mem_head->t_tbl[trid].hint_lsn;
}

void llmgr_core::set_hint_lsn_for_prev_rollback_record(transaction_id &trid, LONG_LSN lsn)
{
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  
  if ( mem_head->t_tbl[trid].mode == ROLLBACK_MODE)
      mem_head->t_tbl[trid].hint_lsn = lsn;
}

/*
 * File:  llmgr_core.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#include <iostream>
#include "base.h"
#include "exceptions.h"
#include "ushm.h"
#include "xptr.h"
#include "llmgr_core.h"
#include "sm_globals.h"
#include "utils.h"
#include "tr_debug.h"
#include "trmgr.h"

using namespace std;

/*****************************************************************************
                          Init and Close Functions
******************************************************************************/

/*                 !!!These Functions called on sm !!!                         */
void llmgr_core::ll_log_create(string db_files_path)
{
  int res;
  //!!!init logical log protection semaphore!!!
  res = USemaphoreCreate(&sem_dsc, 1, 1, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);  

  if ( res != 0 )
     throw USER_EXCEPTION2(SE4010, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

#ifndef LOGICAL_LOG_TEST
  string log_file_name = string(db_files_path) + string(db_name) + ".llog";
#else
  string log_file_name = "x.llog";
#endif

  //!!!init logical log file descriptor; !!!

  ll_file_dsc = uOpenFile(
                      log_file_name.c_str(),
                      U_SHARE_READ | U_SHARE_WRITE,
                      U_READ_WRITE,
                      U_WRITE_THROUGH 
                     );
  if ( ll_file_dsc == U_INVALID_FD )
     throw USER_EXCEPTION2(SE4042, log_file_name.c_str());

  if (sizeof(small_read_buf) < sizeof(logical_log_head))
     throw USER_EXCEPTION(SE4150);

  large_read_buf = NULL;

  rollback_active = false;
  recovery_active = false;

  indir_rec = NULL;
  indir_rec_len = 0;
}




void llmgr_core::ll_log_release()
{
  int res;

  res = USemaphoreRelease(sem_dsc);  

  if (res != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

  res = uCloseFile(ll_file_dsc);
  
  if (res == 0)
     throw USER_EXCEPTION2(SE4043, "logical log file");

}



void llmgr_core::ll_log_create_shared_mem()
{
   //create shared memory

   int res;

   if (CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE <= sizeof(logical_log_sh_mem_head))
      throw USER_EXCEPTION(SE4151);

   res = uCreateShMem(&shared_mem_dsc,
                      CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME,
                      CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE
                     );

   if (res != 0)
      throw USER_EXCEPTION2(SE4016, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");


   //init shared memory pointer
   shared_mem = uAttachShMem(shared_mem_dsc,
                             NULL,
                             CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE
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
      
   logical_log_file_head file_head = read_log_file_header();
   mem_head->next_lsn =  file_head.next_lsn;
   mem_head->next_durable_lsn = file_head.next_lsn;
   
   for (int i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
   {
       mem_head->t_tbl[i].last_lsn = NULL_LSN; 
       mem_head->t_tbl[i].last_rec_mem_offs = NULL_OFFS;
       mem_head->t_tbl[i].is_ended = false;
       mem_head->t_tbl[i].num_of_log_records = 0;

   }

}


void llmgr_core::ll_log_release_shared_mem()
{
  int res = uDettachShMem(shared_mem_dsc, shared_mem);

  if (res != 0)
     throw USER_EXCEPTION2(SE4024, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

  res = uReleaseShMem(shared_mem_dsc);

  if (res != 0)
     throw USER_EXCEPTION2(SE4020, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

}


/*               !!! These fuctions are called on session !!!                */ 

void llmgr_core::ll_log_open(string db_files_path, bool rcv_active)
{
  int res;

  res = USemaphoreOpen(&sem_dsc, CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME);

  if ( res != 0 )
     throw USER_EXCEPTION2(SE4012, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

  ll_file_dsc = uOpenFile(
                       db_files_path.c_str(),
                       U_SHARE_READ | U_SHARE_WRITE,
                       U_READ_WRITE,
                       U_WRITE_THROUGH 
                     );

  if ( ll_file_dsc == U_INVALID_FD )
     throw USER_EXCEPTION2(SE4042, db_files_path.c_str());  
}


void llmgr_core::ll_log_open_shared_mem()
{
   //open shared memory
   int res;

   res = uOpenShMem(&shared_mem_dsc,
                    CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME,
                    CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE
                   );

   if (res != 0)
      throw USER_EXCEPTION2(SE4021, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

   //init shared memory pointer
   shared_mem = uAttachShMem(shared_mem_dsc, NULL, CHARISMA_LOGICAL_LOG_SHARED_MEM_SIZE);

   if ( shared_mem == NULL)
      throw USER_EXCEPTION2(SE4023, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

}

void llmgr_core::ll_log_close_shared_mem()
{
  int res = uDettachShMem(shared_mem_dsc, shared_mem);

  if (res != 0)
     throw USER_EXCEPTION2(SE4024, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");

  res = uCloseShMem(shared_mem_dsc);

  if (res != 0)
     throw USER_EXCEPTION2(SE4022, "CHARISMA_LOGICAL_LOG_SHARED_MEM_NAME");
}


void llmgr_core::ll_log_close()
{
  int res;

  res = USemaphoreClose(sem_dsc);  

  if (res != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_LOG_PROTECTION_SEM_NAME");

  res = uCloseFile(ll_file_dsc);
  
  if (res == 0)
     throw USER_EXCEPTION2(SE4043, "logical log file");


}


/*            !!! These functions are called on transaction !!!                 */


void llmgr_core::ll_log_on_transaction_begin(bool rcv_active, transaction_id &trid, bool sync)
{
  rollback_active = false;
  recovery_active = rcv_active;

  indir_rec = NULL;
  indir_rec_len = 0;

  ll_log_lock(sync);
   
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  if (trid > CHARISMA_MAX_TRNS_NUMBER || trid < 0)
     throw USER_EXCEPTION2(SE4155, int2string(trid));

  mem_head->t_tbl[trid].last_lsn = NULL_LSN;
  mem_head->t_tbl[trid].last_rec_mem_offs  = NULL_OFFS;
  mem_head->t_tbl[trid].is_ended = false;
  mem_head->t_tbl[trid].num_of_log_records = 0;

  ll_log_unlock(sync);

}


void llmgr_core::ll_log_on_transaction_end(transaction_id &trid, bool sync)
{
  ll_log_lock(sync);
   
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  if (trid > CHARISMA_MAX_TRNS_NUMBER || trid < 0)
     throw USER_EXCEPTION2(SE4155, int2string(trid));

  mem_head->t_tbl[trid].last_lsn = NULL_LSN;
  mem_head->t_tbl[trid].last_rec_mem_offs  = NULL_OFFS;
  mem_head->t_tbl[trid].is_ended = false;
  mem_head->t_tbl[trid].num_of_log_records = 0;

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

  tmp_rec = new char[rec_len];

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

  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec; 
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

  tmp_rec = new char[rec_len];

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

  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec; 
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

  tmp_rec = new char[rec_len];

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

  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec;
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

  tmp_rec = new char[rec_len];

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

  delete [] tmp_rec;
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

  tmp_rec = new char[rec_len];

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

  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec;  
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

  tmp_rec = new char[rec_len];

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


  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec;  
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

  tmp_rec = new char[rec_len];

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


  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec;  
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
  
  tmp_rec = new char[rec_len];

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


  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec;  
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

  tmp_rec = new char[rec_len];

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

  delete [] indir_rec;
  indir_rec = NULL;
  indir_rec_len = 0;
  delete [] tmp_rec;  
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

  tmp_rec = new char[rec_len];

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

  delete [] tmp_rec;  
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

  tmp_rec = new char[rec_len];
  char op = LL_COMMIT;
  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  //insert record in shared memory
  ret_lsn = ll_log_insert_record(tmp_rec, rec_len, trid, false);

  mem_head->t_tbl[trid].is_ended = true;  

  delete [] tmp_rec;  

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

  tmp_rec = new char[rec_len];
  char op = LL_ROLLBACK;
  int offs = 0;

  //create record body
  inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
  inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

  //insert record in shared memory
  ll_log_insert_record(tmp_rec, rec_len, trid, false);

  mem_head->t_tbl[trid].is_ended = true;

  delete [] tmp_rec;  

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
  tmp_rec = new char[rec_len];
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
       inc_mem_copy(tmp_rec, offs, &(mem_head->t_tbl[i].last_lsn), sizeof(LONG_LSN));
  }

  //insert record in shared memory
  logical_log_head log_head;
  log_head.prev_trn_offs = NULL_OFFS;
  log_head.body_len = rec_len;

  //insert log record into shared memory
  writeSharedMemory(&log_head, sizeof(logical_log_head));
  writeSharedMemory(tmp_rec, rec_len);

  mem_head->next_lsn += sizeof(logical_log_head) + rec_len;

  delete [] tmp_rec;

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


  indir_rec = new char[_indir_rec_len];  
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
  mem_head->next_lsn+= sizeof(logical_log_head) + len;
  mem_head->t_tbl[trid].num_of_log_records +=1;
  
  ret_lsn = mem_head->t_tbl[trid].last_lsn;

  ll_log_unlock(sync);

  //d_printf4("log record inserted, trid=%d, num=%d\n len=%d\n", trid, mem_head->t_tbl[trid].num_of_log_records, len);

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
  int written;

  //set file pointer to the end  
  set_file_pointer(mem_head->next_durable_lsn);


  //d_printf2("need to write bytes=%d\n", rmndr_len);
  //flush needed records;
  while (rmndr_len > 0)
  {
     res = uWriteFile(ll_file_dsc,
                      (char*)shared_mem + offs,
                      to_write,
                      &written
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
  mem_head->t_tbl[trid].last_rec_mem_offs = NULL_OFFS;
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
     res = uWriteFile(ll_file_dsc,
                      (char*)shared_mem + offs,
                      to_write,
                      &written
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

  ll_log_unlock(sync);
}

void llmgr_core::ll_log_flush(bool sync)
{//the implementation of this method is not correct, because
 //in logical log can be a last not completed record, it will
 // not be flushed
  ll_log_lock(sync);

  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;  

  for (int i=0; i< CHARISMA_MAX_TRNS_NUMBER; i++)
      ll_log_flush(i, false);

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
           delete_large_read_buf();
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
        delete_large_read_buf();
        rec_beg = get_record_from_disk(lsn);
        InShMem = false;
        log_head = (logical_log_head*)rec_beg;
        rmndr_mem_len = 0;
     }


  }

  rollback_active = false;
  ll_log_unlock(sync);
}


//this function is run from the special recovery process
#ifdef SE_ENABLE_FTSEARCH
void llmgr_core::recover_db_by_logical_log(void (*index_op) (trns_analysis_map&),
										   void (*exec_micro_op) (const char*, int, bool),
                                           void(*switch_indirection)(int),
                                           void (*_rcv_allocate_blocks)(const std::vector<xptr>&),
                                           const LONG_LSN& last_cp_lsn,
                                           int undo_indir_mode,
                                           int redo_indir_mode,
                                           bool sync)
#else
void llmgr_core::recover_db_by_logical_log(void (*exec_micro_op) (const char*, int, bool),
                                           void(*switch_indirection)(int),
                                           void (*_rcv_allocate_blocks)(const std::vector<xptr>&),
                                           const LONG_LSN& last_cp_lsn,
                                           int undo_indir_mode,
                                           int redo_indir_mode,
                                           bool sync)
#endif
{
  ll_log_lock(sync);

  logical_log_file_head file_head = read_log_file_header();

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
     ll_log_unlock(sync);
     return;//all already recovered
  }

  trns_analysis_map undo_redo_trns_map;


  std::vector<xptr> indir_blocks;
 
  d_printf1("get_undo_redo_trns_map\n");
  undo_redo_trns_map =
         get_undo_redo_trns_map(last_checkpoint_lsn,
                                last_commit_lsn,
                                indir_blocks);

  d_printf2("rcv_allocate_blocks num=%d\n", indir_blocks.size());
  for (int i=0; i< indir_blocks.size(); i++)
      indir_blocks[i].print();

  _rcv_allocate_blocks(indir_blocks);

  switch_indirection(undo_indir_mode);

  d_printf1("undo loseres\n");
  //undo losers
  trns_analysis_map::iterator it;
  for(it=undo_redo_trns_map.begin(); it != undo_redo_trns_map.end(); it++)
     if ((it->second).type == 0)//undo case
        undo_trn((it->second).lsn, exec_micro_op);


  //redo committed transactions
  LONG_LSN start_redo_lsn;
  const char* checkpoint_rec = get_record_from_disk(last_checkpoint_lsn);

  get_record_length(checkpoint_rec);
  start_redo_lsn = (last_checkpoint_lsn != NULL_LSN) ? last_checkpoint_lsn + get_record_length(checkpoint_rec) :
                                                       sizeof(logical_log_file_head);


  switch_indirection(redo_indir_mode);

  //check that there is at least one redo transaction
  bool exist_redo = false;
  for(it=undo_redo_trns_map.begin(); it != undo_redo_trns_map.end(); it++)
     if ((it->second).type == 1)//redo case
         exist_redo = true;        
 
  if (exist_redo)
  {
      d_printf1("redo commited transactions\n");
      redo_commit_trns(undo_redo_trns_map, 
                       start_redo_lsn,
                       last_commit_lsn,
                       exec_micro_op);
  }
#ifdef SE_ENABLE_FTSEARCH
  index_op(undo_redo_trns_map);
#endif
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

void llmgr_core::redo_commit_trns(trns_analysis_map& trns_map, LONG_LSN &start_lsn, LONG_LSN &end_lsn, void (*_exec_micro_op_) (const char*, int, bool))
{
  if (start_lsn > end_lsn )
     throw USER_EXCEPTION(SE4152);

  LONG_LSN lsn = start_lsn;
  const char *rec;
  int body_len;
  trns_analysis_map::iterator it; 
 
  int i=1;

  do
  {
    rec = get_record_from_disk(lsn);
    body_len = ((logical_log_head*)rec)->body_len;

    if ((rec + sizeof(logical_log_head))[0] != LL_COMMIT &&
        (rec + sizeof(logical_log_head))[0] != LL_ROLLBACK &&
        (rec + sizeof(logical_log_head))[0] != LL_CHECKPOINT)
    {
      it = trns_map.find(*((transaction_id*)((rec + sizeof(logical_log_head))+sizeof(char))));


      if (it != trns_map.end() && (it->second).type == 1)//redo record
#ifdef RECOVERY_EXEC_MICRO_OP
         _exec_micro_op_(rec + sizeof(logical_log_head), body_len, false);
#else
          ;
#endif
	  // FOR DEBUG
      //d_printf4("exec log record num=%d len=%d, trid=%d\n", i, body_len, *((transaction_id*)((rec + sizeof(logical_log_head))+sizeof(char))));
/////////////////////////////
    }
	else//branch for debug
	{
		d_printf4("commit or rollback or cp num=%d, len=%d, trid=%d\n", i, body_len, *((transaction_id*)((rec + sizeof(logical_log_head))+sizeof(char))));
	}
        
    lsn += sizeof(logical_log_head) + body_len;
	i++;

  } while(lsn <= end_lsn);
}

trns_analysis_map llmgr_core::get_undo_redo_trns_map(LONG_LSN &start_lsn, LONG_LSN &end_lsn, vector<xptr>& indir_blocks)
{
  LONG_LSN lsn;
  const char *rec;
  const char *body_beg;
  trns_analysis_map trns_map;


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

       trns_map.insert(trn_pair(*trid, trn_cell_analysis(0, *last_lsn)));
    }

    lsn = start_lsn + get_record_length(rec);    
  }

  //pass the log
  trns_analysis_map trns_map_after_checkpoint;//map of transactions began after checkpoint
  trns_analysis_map::iterator it;

  while (lsn <= end_lsn)
  {
    rec = get_record_from_disk(lsn);
    body_beg = rec + sizeof(logical_log_head);

    if (body_beg[0] == LL_CHECKPOINT)
       throw USER_EXCEPTION(SE4153);

    transaction_id *trid;
    trid = (transaction_id*)(body_beg + sizeof(char));

    if (body_beg[0] == LL_COMMIT)
    {
       if ((it = trns_map.find(*trid)) != trns_map.end())
          it->second.type = 1;//change status to redo
       else
       {
          if ((it = trns_map_after_checkpoint.find(*trid)) != trns_map_after_checkpoint.end())
             it->second.type = 1;
          else
			  if (((logical_log_head*)rec)->prev_trn_offs != NULL_LSN)//it means that commit record is not the only record of transaction
                 throw USER_EXCEPTION(SE4154);
       }
          
    }
    else
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
        body_beg[0] == LL_INSERT_COL_INDEX  || body_beg[0] == LL_DELETE_COL_INDEX)
    {
      if(trns_map.find(*trid) == trns_map.end() &&
         trns_map_after_checkpoint.find(*trid) == trns_map_after_checkpoint.end())
      {
         //first record of new transaction
         trns_map_after_checkpoint.insert(trn_pair(*trid, trn_cell_analysis(0, lsn)));
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
              indir_blocks.push_back(*((xptr*)(body_beg + offs)));
              offs += sizeof(xptr);
          }
      }

    }
    else
    if (body_beg[0] != LL_ROLLBACK)
      throw USER_EXCEPTION(SE4154);

    lsn += get_record_length(rec);
  }


  //add commit transaction which where bagan after checkpoint to the result map  
  for (it = trns_map_after_checkpoint.begin(); it != trns_map_after_checkpoint.end(); it++)
  {
    if (it->second.type == 1)//redo case
       trns_map.insert(trn_pair(it->first, trn_cell_analysis(1, it->second.lsn)));
  }

  return trns_map;
}



int llmgr_core::get_num_of_records_written_by_trn(transaction_id &trid)
{
   //synchronization is not needed
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  return mem_head->t_tbl[trid].num_of_log_records; 

}

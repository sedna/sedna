/*
 * File:  logiclog.cpp - Logical logging on tr
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This functions should be used to write logical information in logical log.
 *
 */

#include "tr/log/logiclog.h"
#include "common/llcommon/llMain.h"

// mem copy with offset increment
inline
static
void inc_mem_copy(void* dest, int &offs, const void* src, int len)
{
	if (len == 0) return;
	memcpy((char*)dest + offs, src, len);
	offs += len;
}

/*
  element record body format:
  op_type(1 byte),
  trid (2byte)
  name ('\0' terminated string)
  uri ('\0' terminated string)
  prefix ('\0' terminated prefix)
  xmlscm_type (2byte)
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
*/
void llLogElement(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, 
    			  const xptr *parent, const char* name, const char* uri, const char* prefix, xmlscm_type type, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;
	int rec_len;
	int uri_len = (uri != NULL) ? strlen(uri) + 1 : 1;
	int prefix_len = (prefix != NULL) ? strlen(prefix) + 1 : 1;

	rec_len = sizeof(char) +
            sizeof(transaction_id) + 
            strlen(name) + 1 +
            uri_len +
            prefix_len +
            sizeof(xmlscm_type) +                           
            4 * sizeof(xptr); 

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = (inserted) ? LL_INSERT_ELEM : LL_DELETE_ELEM;
  
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, name, strlen(name) + 1);
	inc_mem_copy(tmp_rec, offs, ((uri != NULL ) ? uri: ""), uri_len);
	inc_mem_copy(tmp_rec, offs, ((prefix != NULL ) ? prefix: ""), prefix_len);
	inc_mem_copy(tmp_rec, offs, &type, sizeof(xmlscm_type));
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, left, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, right, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, parent, sizeof(xptr));

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/* attribute record body format:
  op-type (1byte)
  trid (2byte)
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

void llLogAttribute(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
						const char* name, xmlscm_type type, const char* value, int value_size, const char* uri,
						const char* prefix, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;
	int rec_len;
	int uri_len = (uri != NULL) ? strlen(uri) + 1 : 1;
	int prefix_len = (prefix != NULL) ? strlen(prefix) + 1 : 1;


	rec_len = sizeof(char) +
    	        sizeof(transaction_id) +
        	    strlen(name) + 1 +
            	uri_len +
	            prefix_len +
    	        sizeof(int) +
        	    value_size +
            	sizeof(xmlscm_type) +
	            4 * sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = (inserted) ? LL_INSERT_ATTR: LL_DELETE_ATTR;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, name, strlen(name) + 1);
	inc_mem_copy(tmp_rec, offs, ((uri != NULL ) ? uri: ""), uri_len);
	inc_mem_copy(tmp_rec, offs, ((prefix != NULL ) ? prefix: ""), prefix_len);
	inc_mem_copy(tmp_rec, offs, &value_size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, value, value_size);
	inc_mem_copy(tmp_rec, offs, &type, sizeof(xmlscm_type));
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, left, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, right, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, parent, sizeof(xptr));    

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
 text log record format:
 op (1byte)
 trid (2byte)
 value_size (4bytes)
 value (without '\0')
 self-xptr(8byte)
 left-xptr(8byte)
 right-xptr(8byte)
 parent-xptr(8byte) 
*/

void llLogText(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
				const char* value, int value_size, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;
	int rec_len;

	rec_len = sizeof(char) +
    	      sizeof(transaction_id) +
        	  sizeof(int) +
              value_size +
              4 * sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = (inserted) ? LL_INSERT_TEXT: LL_DELETE_TEXT;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, &value_size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, value, value_size);
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, left, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, right, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, parent, sizeof(xptr));

	//inert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}


/*
 text_edit log record format:
 op (1byte)
 trid (2byte)
 data_size (4bytes)
 value (without '\0')
 self-xptr(8byte)
*/
void llLogTextEdit(transaction_id trid, const xptr *self, const char* value, int data_size, bool begin, 
						bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;
	int rec_len;

	rec_len = sizeof(char) +
    	      sizeof(transaction_id) +
              sizeof(int) +
              data_size +
              sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

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
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}


/*
 document log record format:
 op (1byte)
 trid (2 bytes)
 name ('\0' terminated string) 
 collection ('\0' terminated string)
 self (xptr)
*/
void llLogDocument(transaction_id trid, const xptr *self, const char *name, const char *collection, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;
	int rec_len;
	int col_len = (collection != NULL) ? (strlen(collection) + 1) : 1;

	rec_len = sizeof(char) +
              sizeof(transaction_id) +
              strlen(name) + 1 +
              col_len +
              sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = inserted ? LL_INSERT_DOC : LL_DELETE_DOC;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char)); 
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, name, strlen(name) + 1);
	inc_mem_copy(tmp_rec, offs, (collection != NULL) ? collection : "", col_len);
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
  pi log record format:
  op (1 byte)
  trid (transaction_id)
  total_size (4 bytes)
  target_size (2 bytes)
  value (without '\0')
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
 
*/
void llLogPI(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
				const char* value, int total_size, shft target_size, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;

	rec_len = sizeof(char) +
    	        sizeof(transaction_id) +
        	    sizeof(int) +
            	sizeof(shft) +
            	total_size +
             	4 * sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;
   
	op = inserted ? LL_INSERT_PI : LL_DELETE_PI;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, &total_size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, &target_size, sizeof(shft));
	inc_mem_copy(tmp_rec, offs, value, total_size);
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, left, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, right, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, parent, sizeof(xptr));

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
  comment log record format:
  op (1 byte)
  trid (transaction_id)
  value_size (4 bytes)
  value (without '\0')
  self-xptr(8byte)
  left-xptr(8byte)
  right-xptr(8byte)
  parent-xptr(8byte) 
*/
void llLogComment(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent, 
					const char *value, int value_size, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;

	rec_len = sizeof(char) +
              sizeof(transaction_id) +
              sizeof(int) +
              value_size +
              4 * sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = inserted ? LL_INSERT_COMMENT : LL_DELETE_COMMENT;

	int offs = 0;
  
	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, &value_size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, value, value_size);
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, left, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, right, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, parent, sizeof(xptr));

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
  rename collection log record format:
  op (1 byte)
  trid (transaction_id)
  old_name ('\0' terminated string)
  new_name ('\0' terminated string)
*/
void llLogRenameCollection(transaction_id trid, const char* old_name, const char* new_name)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;

	rec_len = sizeof(char) +
    		  sizeof(transaction_id) +
              strlen(old_name) + 1 +
              strlen(new_name) + 1;
  
	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = LL_RENAME_COLLECTION;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, old_name, strlen(old_name) + 1);
	inc_mem_copy(tmp_rec, offs, new_name, strlen(new_name) + 1);
  
	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
 collection log record format:
  op (1 byte)
  trid (transaction_id)
  name ('\0' terminated string)
*/
void llLogCollection(transaction_id trid, const char* name, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;

	rec_len = sizeof(char) +
    		  sizeof(transaction_id) +
              strlen(name) + 1;
  
	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = inserted ? LL_INSERT_COLLECTION : LL_DELETE_COLLECTION;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, name, strlen(name) + 1);
  
	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
 namespace log record format:
 op (1 byte)
 trid (transaction_id)
 uri ('\0' terminated string)
 prefix ('\0' terminated string)
 self-xptr(8byte)
 left-xptr(8byte)
 right-xptr(8byte)
 parent-xptr(8byte) 
*/
void llLogNS(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
				const char *uri, const char *prefix, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;
	int prefix_len = (prefix != NULL) ? (strlen(prefix) + 1) : 1;

 
	rec_len = sizeof(char) +
              sizeof(transaction_id) +
              strlen(uri) + 1 +
              prefix_len +
              4 * sizeof(xptr);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	op = inserted ? LL_INSERT_NS : LL_DELETE_NS;

	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, uri, strlen(uri) + 1);
	inc_mem_copy(tmp_rec, offs, (prefix != NULL) ? prefix : "", prefix_len);
	inc_mem_copy(tmp_rec, offs, self, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, left, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, right, sizeof(xptr));
	inc_mem_copy(tmp_rec, offs, parent, sizeof(xptr));

	//insert record
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
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
void llLogIndex(transaction_id trid, const char *object_path, const char *key_path, xmlscm_type key_type, 
					const char *index_title, const char *doc_name, bool is_doc, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;
	int obj_path_len = (object_path != NULL) ? (strlen(object_path) + 1) : 1;
	int key_path_len = (key_path != NULL) ? (strlen(key_path) + 1) : 1;
	int ind_title_len = (index_title != NULL) ? (strlen(index_title) + 1) : 1;
	int doc_name_len = (doc_name != NULL) ? (strlen(doc_name) + 1) : 1;

 
	rec_len = sizeof(char) +
              sizeof(transaction_id) +
              obj_path_len +
              key_path_len +
              sizeof(xmlscm_type) +
              ind_title_len +
              doc_name_len;

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

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
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
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

void llLogFtIndex(transaction_id trid, const char *object_path, int itconst, const char *index_title, 
					const char *doc_name, bool is_doc, char *custom_tree_buf, int custom_tree_size, bool inserted)
{
	RECOVERY_CRASH;

	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;
	int obj_path_len  = (object_path != NULL) ? (strlen(object_path) + 1) : 1;
	int ind_title_len = (index_title != NULL) ? (strlen(index_title) + 1) : 1;
	int doc_name_len  = (doc_name != NULL) ? (strlen(doc_name) + 1) : 1;

 
	rec_len = sizeof(char) +
              sizeof(transaction_id) +
              obj_path_len +
              sizeof(int) +
              ind_title_len +
              doc_name_len +
              sizeof(int) +
              custom_tree_size;

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

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
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/* 
Trigger log record format:
	op (1 byte)
	trid(transaction_id)
	tr_time(int)
	tr_event(int)
	trigger_path(null-terminated string)
	tr_gran(int)
	tr_action_size(int)
	tr_action_buf(tr_action_size bytes)
	in_name(null-terminated string)
	in_type(int)
	path_to_parent(null-terminated string)
	trigger_title(null-terminated string)
	doc_name(null-terminated string)
*/
void llLogTrigger(transaction_id trid, int tr_time, int tr_event, const char *trigger_path, int tr_gran,
						char *tr_action_buf, int tr_action_size, const char *in_name, int in_type, 
						const char *path_to_parent, const char *trigger_title, const char *doc_name, 
						bool is_doc, bool inserted)
{
	if (rollback_active || recovery_active) return;

	char *tmp_rec;  
	int rec_len;
	int tr_path_len = (trigger_path != NULL) ? (strlen(trigger_path) + 1) : 1;
	int in_name_len = (in_name != NULL) ? (strlen(in_name) + 1) : 1;
	int path_to_par_len = (path_to_parent != NULL) ? (strlen(path_to_parent) + 1) : 1;
	int tr_title_len = (trigger_title != NULL) ? (strlen(trigger_title) + 1) : 1;
	int doc_name_len = (doc_name != NULL) ? (strlen(doc_name) + 1) : 1;

	rec_len = sizeof(char) + sizeof(transaction_id) + 5 * sizeof(int) + tr_path_len + tr_action_size + in_name_len +
  				path_to_par_len + tr_title_len + doc_name_len;

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op;

	if (is_doc)
		op = inserted ? LL_INSERT_DOC_TRG : LL_DELETE_DOC_TRG;
	else
		op = inserted ? LL_INSERT_COL_TRG : LL_DELETE_COL_TRG;

	int offs = 0;

	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));
	inc_mem_copy(tmp_rec, offs, &tr_time, sizeof(int));
	inc_mem_copy(tmp_rec, offs, &tr_event, sizeof(int));
	inc_mem_copy(tmp_rec, offs, (trigger_path != NULL) ? trigger_path : "", tr_path_len);
	inc_mem_copy(tmp_rec, offs, &tr_gran, sizeof(int));
	inc_mem_copy(tmp_rec, offs, &tr_action_size, sizeof(int));
	inc_mem_copy(tmp_rec, offs, tr_action_buf, tr_action_size);
	inc_mem_copy(tmp_rec, offs, (in_name != NULL) ? in_name : "", in_name_len);
	inc_mem_copy(tmp_rec, offs, &in_type, sizeof(int));
	inc_mem_copy(tmp_rec, offs, (path_to_parent != NULL) ? path_to_parent : "", path_to_par_len);
	inc_mem_copy(tmp_rec, offs, (trigger_title != NULL) ? trigger_title : "", tr_title_len);
	inc_mem_copy(tmp_rec, offs, (doc_name != NULL) ? doc_name : "", doc_name_len);

	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

/*
 commit log record format:
 op (1 byte)
 trid (transaction_id)
*/
void llLogCommit(transaction_id trid)
{
	RECOVERY_CRASH;

	char *tmp_rec;  
	int rec_len;

	// don't need to log empty transaction
	if (llInfo->llTransInfoTable[trid].first_lsn == LFS_INVALID_LSN)
		return;

	rec_len = sizeof(char) + sizeof(transaction_id);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");
	
	char op = LL_COMMIT;
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

	//insert record in shared memory
	llInsertRecord(tmp_rec, rec_len, trid);

	// flush all transaction records
	llFlushTransRecs(trid);

	free(tmp_rec);
}


/*
 rollback log record format:
 op (1 byte)
 trid (transaction_id)
*/
void llLogRollback(transaction_id trid)
{
	RECOVERY_CRASH;

	char *tmp_rec;  
	int rec_len;

	// don't need to log empty transaction
	if (llInfo->llTransInfoTable[trid].first_lsn == LFS_INVALID_LSN)
		return;

	rec_len = sizeof(char) + sizeof(transaction_id);

	if ((tmp_rec = (char *)malloc(rec_len)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");

	char op = LL_ROLLBACK;
	int offs = 0;

	//create record body
	inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
	inc_mem_copy(tmp_rec, offs, &trid, sizeof(transaction_id));

	//insert record in shared memory
	llInsertRecord(tmp_rec, rec_len, trid);

	free(tmp_rec);
}

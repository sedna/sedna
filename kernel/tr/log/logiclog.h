/*
 * File:  logiclog.h - Logical logging on tr
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to logicallog.cpp file.
 *
 */

#ifndef _LL_LOGICAL_LOG_
#define _LL_LOGICAL_LOG_

#include "common/base.h"
#include "common/xptr/xptr.h"
#include "common/lfsGlobals.h"
#include "common/llcommon/llMain.h"
#include "tr/tr_base.h"

// Commit log record
void llLogCommit(transaction_id trid);

// Rollback log record
void llLogRollback(transaction_id trid);


/*******************************************
 * Here follows logical log record formats.*
 *******************************************/

/*
 * element record body format:
 * op_type(1 byte),
 * trid (2byte)
 * name ('\0' terminated string)
 * uri ('\0' terminated string)
 * prefix ('\0' terminated prefix)
 * xmlscm_type (2byte)
 * self-xptr(8byte)
 * left-xptr(8byte)
 * right-xptr(8byte)
 * parent-xptr(8byte)
 */

/*
 * attribute record body format:
 * op-type (1byte)
 * trid (2byte)
 * name ('\0' terminated string)
 * uri ('\0' terminated string)
 * prefix ('\0' terminated string)
 * value_size (4 bytes)
 * value (without '\0')
 * xmlscm_type (2byte)
 * self-xptr(8byte)
 * left-xptr(8byte)
 * right-xptr(8byte)
 * parent-xptr(8byte)
 */

/*
 * text log record format:
 * op (1byte)
 * trid (2byte)
 * value_size (4bytes)
 * value (without '\0')
 * self-xptr(8byte)
 * left-xptr(8byte)
 * right-xptr(8byte)
 * parent-xptr(8byte)
 */

/*
 * text_edit log record format:
 * op (1byte)
 * trid (2byte)
 * data_size (4bytes)
 * value (without '\0')
 * self-xptr(8byte)
 */

/*
 * document log record format:
 * op (1byte)
 * trid (2 bytes)
 * name ('\0' terminated string)
 * collection ('\0' terminated string)
 * self (xptr)
 */

/*
 * pi log record format:
 * op (1 byte)
 * trid (transaction_id)
 * total_size (4 bytes)
 * target_size (2 bytes)
 * value (without '\0')
 * self-xptr(8byte)
 * left-xptr(8byte)
 * right-xptr(8byte)
 * parent-xptr(8byte)
 */

/*
 * comment log record format:
 * op (1 byte)
 * trid (transaction_id)
 * value_size (4 bytes)
 * value (without '\0')
 * self-xptr(8byte)
 * left-xptr(8byte)
 * right-xptr(8byte)
 * parent-xptr(8byte)
 */

/*
 *rename collection log record format:
 * op (1 byte)
 * trid (transaction_id)
 * old_name ('\0' terminated string)
 * new_name ('\0' terminated string)
 */

/*
 * collection log record format:
 * op (1 byte)
 * trid (transaction_id)
 * name ('\0' terminated string)
 */

/*
 * namespace log record format:
 * op (1 byte)
 * trid (transaction_id)
 * uri ('\0' terminated string)
 * prefix ('\0' terminated string)
 * self-xptr(8byte)
 * left-xptr(8byte)
 * right-xptr(8byte)
 * parent-xptr(8byte)
 */

/*
 * index log record format:
 * op (1 byte)
 * trid (transaction_id)
 * object_path ('\0' terminated string)
 * key_path ('\0' terminated string)
 * key_type (2 bytes)
 * index_title ('\0' terminated string)
 * doc_name or collection_name ('\0' terminated string)
 */

/*
 * Full-text index log record format:
 * op (1 byte)
 * trid (transaction_id)
 * object_path ('\0' terminated string)
 * ft_index_type (int)
 * index_title ('\0' terminated string)
 * doc_name ('\0' terminated string)
 * custom_tree_size (int)
 * custom_tree_buf (custom_tree_size bytes)
 */

/*
 * Trigger log record format:
 *  op (1 byte)
 *  trid(transaction_id)
 *  tr_time(int)
 *  tr_event(int)
 *  trigger_path(null-terminated string)
 *  tr_gran(int)
 *  tr_action_size(int)
 *  tr_action_buf(tr_action_size bytes)
 *  in_name(null-terminated string)
 *  in_type(int)
 *  path_to_parent(null-terminated string)
 *  trigger_title(null-terminated string)
 *  doc_name(null-terminated string)
 */

/*
 * commit log record format:
 * op (1 byte)
 * trid (transaction_id)
 */

/*
 * rollback log record format:
 * op (1 byte)
 * trid (transaction_id)
 */

#endif

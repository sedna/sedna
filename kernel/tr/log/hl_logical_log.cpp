/*
 * File:  hl_logical_log.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <vector>
#include <sstream>

#include "common/sedna.h"

#include "tr/log/log.h"
#include "tr/log/logiclog.h"
#include "common/llcommon/llMain.h"
#include "tr/tr_globals.h"
#include "common/errdbg/d_printf.h"
#include "common/tr_debug.h"
#include "sm/trmgr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/mo/indirection.h"
#include "tr/mo/boundaries.h"
#include "tr/structures/metadata.h"

#include "tr/structures/nodeinterface.h"

#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTindex.h"
#endif

static bool is_ll_on_session_initialized = false;
static bool is_ll_on_transaction_initialized = false;

// concurrent semaphore already was released by commit function
// because of checkpoint
static bool sem_released = false;

static USemaphore concurrent_trns_sem;

///// FOR DEBUG ////
static int number_of_records = 0;
static int down_up_counter = 0;
////////////////////

/*
 * Function that reports to the version engine that we're done.
 *
 * Needed here, because of checkpoint-beforte-commit logic.
 * See also comment below on hl_logical_log_commit.
 */
void reportToWu(bool rcv_active, bool is_commit);

#ifdef LOG_TRACE
static void elog_log_trace(const char *str, transaction_id trid, const xptr &self,
        const xptr &left, const xptr &right, const xptr &parent, bool inserted)
{
    elog(EL_LOG, ("LOG_TRACE: %s is %s: trid=%d, self=%08x%08u, left=%08x%08u, "
            "right=%08x%08u, parent=%08x%08u", str, inserted ? "inserted" : "deleted",
            tr_globals::trid, self.layer, self.getOffs(), left.layer,
            left.getOffs(), right.layer, right.getOffs(), parent.layer, parent.getOffs()));
}

static void elog_log_trace_te(transaction_id trid, const xptr &self,
        bool begin, bool inserted)
{
    elog(EL_LOG, ("LOG_TRACE:  Text edited (%s) to the %s: trid=%d, self=%08x%08u",
            inserted ? "inserted" : "deleted", begin ? "left" : "right",
            tr_globals::trid, self.layer, self.getOffs()));
}

#else
static void elog_log_trace(const char *str, transaction_id trid, const xptr &self,
        const xptr &left, const xptr &right, const xptr &parent, bool inserted)
{
}

static void elog_log_trace_te(transaction_id trid, const xptr &self,
        bool begin, bool inserted)
{
}
#endif

static bool enable_log = true;

void hl_logical_log_on_session_begin(std::string logical_log_path, bool rcv_active)
{
	if ( 0 != USemaphoreOpen(&concurrent_trns_sem, SEDNA_TRNS_FINISHED, __sys_call_error))
		throw USER_EXCEPTION2(SE4012, "SEDNA_TRNS_FINISHED");

	llOpen(logical_log_path.c_str(), tr_globals::db_name, rcv_active);

	is_ll_on_session_initialized = true;
}

void hl_logical_log_on_transaction_begin(bool rcv_active, bool tr_ro_mode)
{
    enable_log = !tr_ro_mode && !rcv_active;

	//Here trid is a global variable inited before
	if (tr_ro_mode) // we don't need log in RO-mode
	{
		tr_globals::is_need_checkpoint_on_transaction_commit = false;
		is_ll_on_transaction_initialized = false;
		return;
	}

	llOnTransBegin(tr_globals::trid);
	tr_globals::is_need_checkpoint_on_transaction_commit = tr_globals::is_log_less_mode;
	is_ll_on_transaction_initialized = true;
#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Transaction is started: trid=%d", tr_globals::trid));
#endif
}

void hl_logical_log_on_session_end()
{
	if (is_ll_on_session_initialized)
	{
		if (USemaphoreClose(concurrent_trns_sem, __sys_call_error) != 0)
			throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_OPERATION_ATOMICITY");

		llClose();
	}

	is_ll_on_session_initialized = false;
}

void hl_logical_log_on_transaction_end(bool is_commit, bool rcv_active)
{
	if (is_ll_on_transaction_initialized)
	{
		if (!rcv_active && is_commit)
		{
#ifdef SE_ENABLE_DTSEARCH
            SednaIndexJob::start_commit();
#endif
            hl_logical_log_commit(tr_globals::trid);
#ifdef SE_ENABLE_DTSEARCH
            SednaIndexJob::fix_commit();
#endif
            //Here trid is a global variable inited before
            llOnTransEnd(tr_globals::trid);
		}
	}

	is_ll_on_transaction_initialized = false;
}

/*
 * Calling this function during transaction processing (before commit/rollback)
 * will result in deadlock between this transaction and checkpoint thread since
 * checkpoint cannot begin without all transactions being in finished states.
 */
void activate_and_wait_for_end_checkpoint(bool force)
{
    int res;

    while ((res = llActivateCheckpoint()) == -2 && force)
        uSleep(1, __sys_call_error);

    if (res == -2 && !force)
        return;
    else if (res != 0)
        throw SYSTEM_EXCEPTION("logic error in waiting on checkpoint!");

    U_ASSERT(res == 0);

    // wait for checkpoint to finish
    // checkpoint thread should awake this transaction
    llOnCheckpointWait();
}

void down_concurrent_micro_ops_number()
{
	down_up_counter++;
}

void up_concurrent_micro_ops_number()
{
	down_up_counter--;
}

void up_transaction_block_sems()
{
    if (sem_released)
    {
        sem_released = false;
        return;
    }

	if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_LOGICAL_OPERATION_ATOMICITY");
}

void down_transaction_block_sems()
{
	if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_LOGICAL_OPERATION_ATOMICITY");
}

LSN get_lsn_of_first_record_in_logical_log()
{
    return llGetFirstTranLsn(tr_globals::trid);
}

void hl_logical_log_element(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const char* uri,const char* prefix,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

    size_t uri_len = (uri != NULL) ? strlen(uri) + 1 : 1;
    size_t prefix_len = (prefix != NULL) ? strlen(prefix) + 1 : 1;
    llOperations op = (inserted) ? LL_INSERT_ELEM : LL_DELETE_ELEM;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 8, name, strlen(name) + 1, uri ? uri : "",
            uri_len, prefix ? prefix : "", prefix_len, &type, sizeof(xmlscm_type),
            &self, sizeof(xptr), &left, sizeof(xptr),
            &right, sizeof(xptr), &parent, sizeof(xptr));

    elog_log_trace("Element", tr_globals::trid, self, left, right, parent, inserted);
}

void hl_logical_log_attribute(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const  char* value,unsigned data_size,const char* uri,const char* prefix,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

	size_t uri_len = (uri != NULL) ? strlen(uri) + 1 : 1;
	size_t prefix_len = (prefix != NULL) ? strlen(prefix) + 1 : 1;
    llOperations op = (inserted) ? LL_INSERT_ATTR : LL_DELETE_ATTR;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 10, name, strlen(name) + 1, uri ? uri : "",
            uri_len, prefix ? prefix : "", prefix_len, &data_size, sizeof(unsigned),
            value, (size_t)data_size, &type, sizeof(xmlscm_type),
            &self, sizeof(xptr), &left, sizeof(xptr),
            &right, sizeof(xptr), &parent, sizeof(xptr));

    elog_log_trace("Attribute", tr_globals::trid, self, left, right, parent, inserted);
}

void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,unsigned data_size,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

    llOperations op = (inserted) ? LL_INSERT_TEXT : LL_DELETE_TEXT;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 6, &data_size, sizeof(unsigned),
            value, (size_t)data_size, &self, sizeof(xptr), &left, sizeof(xptr),
            &right, sizeof(xptr), &parent, sizeof(xptr));

    elog_log_trace("Text", tr_globals::trid, self, left, right, parent, inserted);
}

/*
 * Main text-edit logging function. Other related functions should write via it.
 */
void hl_logical_log_text_edit(const xptr &self,const  char* value,unsigned data_size,bool begin,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

    llOperations op;
    if (begin)
        op = (inserted) ? LL_INSERT_LEFT_TEXT: LL_DELETE_LEFT_TEXT;
    else
        op = (inserted) ? LL_INSERT_RIGHT_TEXT: LL_DELETE_RIGHT_TEXT;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 3, &data_size, sizeof(unsigned),
            value, (size_t)data_size, &self, sizeof(xptr));

    elog_log_trace_te(tr_globals::trid, self, begin, inserted);
}

void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,xptr& value,bool inserted )
{
	if (!enable_log) return;

	if (inserted)
	{
		pstr_long_cursor cur(value);
		char *ptr;
		int len = cur.get_blk(&ptr);
		ASSERT(len > 0);
		hl_logical_log_text(self, left, right, parent, ptr, len, true);
		while ((len = cur.get_blk(&ptr)) > 0)
		{
			hl_logical_log_text_edit(self, ptr, len, false, true);
		}
	}
	else
	{
		pstr_long_cursor cur(value, true);
		char *ptr1, *ptr2;
		xptr p1, p2;
		int len1 = cur.get_blk_rev(&ptr1);
		p1 = addr2xptr(ptr1);
		int len2 = cur.get_blk_rev(&ptr2);
        p2 = addr2xptr(ptr2);
		ASSERT(len1 > 0);
		while (len2 > 0)
		{
		    CHECKP(p1);
			hl_logical_log_text_edit(self, ptr1, len1, false, false);
			len1 = len2;
			ptr1 = ptr2;
			p1 = p2;
			len2 = cur.get_blk_rev(&ptr2);
	        p2 = addr2xptr(ptr2);
		}
        CHECKP(p1);
		hl_logical_log_text(self, left, right, parent, ptr1, len1, false);
	}
}

void hl_logical_log_text_edit(const xptr &self,unsigned data_size,bool begin,bool inserted)
{
	if (!enable_log) return;
	ASSERT(inserted);
	xptr desc = indirectionDereferenceCP(self);
	CHECKP(desc);
    strsize_t str_len = CommonTextNode(desc).getTextSize();
	xptr str_ptr = CommonTextNode(desc).getTextPointer();
	CHECKP(desc);

	/* "small" string -- within block */
	if (str_len <= PSTRMAXSIZE)
	{
		if (inserted)
		{
			CHECKP(str_ptr);
			if (begin)
				hl_logical_log_text_edit(self, (char*)XADDR(str_ptr), data_size, begin, inserted);
			else
				hl_logical_log_text_edit(self, (char*)XADDR(str_ptr) + str_len - data_size, data_size, begin, inserted);
		}
		return;
	}

	/* here we've got "big" string value */
	if (!begin)
	{
		if (inserted)
		{
			pstr_long_iterator cur(str_ptr, str_len);
			cur -= data_size;
			char *ptr;
			int len;
			while ( (len = cur.get_blk(&ptr)) > 0)
				hl_logical_log_text_edit(self, ptr, len, false, true);
		}
	}
	else
	{
		if (inserted)
		{
			pstr_long_iterator cur(str_ptr);
			char *ptr;
			int len;
			cur += data_size;
			while ( (len = cur.get_blk_rev(&ptr)) > 0)
				hl_logical_log_text_edit(self, ptr, len, true, true);
		}
	}
}

void hl_logical_log_comment(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,unsigned data_size,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

    llOperations op = (inserted) ? LL_INSERT_COMMENT : LL_DELETE_COMMENT;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 6, &data_size, sizeof(unsigned),
            value, (size_t)data_size, &self, sizeof(xptr), &left, sizeof(xptr),
            &right, sizeof(xptr), &parent, sizeof(xptr));

    elog_log_trace("Comment", tr_globals::trid, self, left, right, parent, inserted);
}

void hl_logical_log_document(const xptr &self,const  char* name,const  char* collection,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

	size_t coll_len = (collection != NULL) ? strlen(collection) + 1 : 1;
    llOperations op = (inserted) ? LL_INSERT_DOC : LL_DELETE_DOC;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 3, name, strlen(name) + 1,
            collection ? collection : "", coll_len, &self, sizeof(xptr));
}

void hl_logical_log_rename_collection(const char *old_name, const char *new_name)
{
	if (!enable_log) return;
	number_of_records++;

    llOperations op = LL_RENAME_COLLECTION;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 2, old_name, strlen(old_name) + 1,
            new_name, strlen(new_name) + 1);
}

void hl_logical_log_collection(const  char* name,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

    llOperations op = (inserted) ? LL_INSERT_COLLECTION : LL_DELETE_COLLECTION;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 1, name, strlen(name) + 1);
}

void hl_logical_log_namespace(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;

	size_t prefix_len = (prefix != NULL) ? strlen(prefix) + 1 : 1;
    llOperations op = (inserted) ? LL_INSERT_NS : LL_DELETE_NS;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 6, uri, strlen(uri) + 1,
            prefix ? prefix : "", prefix_len,
            &self, sizeof(xptr), &left, sizeof(xptr),
            &right, sizeof(xptr), &parent, sizeof(xptr));

    elog_log_trace("Namespace", tr_globals::trid, self, left, right, parent, inserted);
}

void hl_logical_log_pi(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,unsigned total_size,shft target_size,bool inserted)
{
    if (!enable_log) return;
    number_of_records++;

    llOperations op = (inserted) ? LL_INSERT_PI : LL_DELETE_PI;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 7, &total_size, sizeof(unsigned),
            &target_size, sizeof(shft), value, (size_t)total_size,
            &self, sizeof(xptr), &left, sizeof(xptr),
            &right, sizeof(xptr), &parent, sizeof(xptr));

    elog_log_trace("Processing-instruction", tr_globals::trid, self, left, right, parent, inserted);
}

void hl_logical_log_commit(transaction_id _trid)
{
	number_of_records++;

    if (tr_globals::is_need_checkpoint_on_transaction_commit)
    {
        /*
         * Here, we need to report to the version engine about commit
         * since we cannot make checkpoint with active transactions
         */
        storage_on_transaction_end();
        reportToWu(false /* not recovery */, true /* commit */);
        catalog_on_transaction_end(true);

        /*
         * dirty hack here!
         * we don't want this transaction to be redone ever
         * so we pretend it've been rolled back
         */
        llLogRollback(_trid);

        up_transaction_block_sems();
        sem_released = true;

        /*
         * do the checkpoint
         */
        activate_and_wait_for_end_checkpoint(true);
    }
    else
    {
        llLogCommit(_trid);
    }

#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Transaction is committed: trid=%d", tr_globals::trid));
#endif
}

void hl_logical_log_rollback(transaction_id _trid)
{
	llLogRollback(_trid);
#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Transaction is rolled back: trid=%d", tr_globals::trid));
#endif
}

void hl_enable_log()
{
	enable_log = true;
}

void hl_disable_log()
{
	enable_log = false;
}

void hl_logical_log_index(index_descriptor_t *dsc, bool inserted)
{
    if (!enable_log) return;
    number_of_records++;

    llOperations op;
    if (dsc->owner->is_document())
        op = inserted ? LL_INSERT_DOC_INDEX : LL_DELETE_DOC_INDEX;
    else
        op = inserted ? LL_INSERT_COL_INDEX : LL_DELETE_COL_INDEX;

    std::string obj_path_str = dsc->object->toLRString();
    std::string key_path_str = dsc->key->toLRString();

    const char * doc_name = dsc->owner->get_name();
    // First character in doc_name stands for metadata cell type

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 6,
                 obj_path_str.c_str(),   obj_path_str.size() + 1,
                 key_path_str.c_str(),   key_path_str.size() + 1,
                 &dsc->keytype,          sizeof(dsc->keytype),
                 dsc->index_title,       strlen(dsc->index_title) + 1,
                 doc_name,               strlen(doc_name) + 1,
                 &dsc->backend_type,     sizeof(dsc->backend_type));
}

#ifdef SE_ENABLE_FTSEARCH

static void custom_put_str(const char *str, char *cust_buf, unsigned int *cust_size)
{
    size_t len;

    if (str == NULL)
    {
        U_ASSERT(*cust_size < sizeof(tr_globals::e_string_buf));
        cust_buf[*cust_size] = '\x0';
        (*cust_size)++;
    }
    else
    {
        len = strlen(str);
        U_ASSERT(*cust_size + len + 1 <= sizeof(tr_globals::e_string_buf));
        memcpy(cust_buf + *cust_size, str, len + 1);
        *cust_size += (len + 1);
    }
}
void ft_serialize_cust_tree(char *cust_buf, unsigned int *cust_size, ft_custom_tree_t *custom_tree)
{
	ft_custom_tree_t::sedna_rbtree_entry *tmp;
	tmp = custom_tree->rb_minimum(custom_tree->root);

	while (tmp != NULL)
	{
		U_ASSERT(*cust_size + sizeof(ft_index_type) <= sizeof(tr_globals::e_string_buf));
		memcpy(cust_buf + *cust_size, &tmp->obj->cm, sizeof(ft_index_type));
		*cust_size += sizeof(ft_index_type);

		if (tmp->obj->get_xmlns() == NULL)
		{
            custom_put_str(NULL, cust_buf, cust_size);
            custom_put_str(NULL, cust_buf, cust_size);
        }
		else
		{
            custom_put_str(tmp->obj->get_xmlns()->uri, cust_buf, cust_size);
            custom_put_str(tmp->obj->get_xmlns()->prefix, cust_buf, cust_size);
        }
        custom_put_str(tmp->obj->local, cust_buf, cust_size);

		tmp = custom_tree->rb_successor(tmp);
	}
}
ft_index_template_t* ft_rebuild_cust_tree(const char *custom_tree_buf, unsigned custom_tree_size)
{
	ft_index_template_t *res;
	const char *p = custom_tree_buf;

	if (custom_tree_size == 0)
		return NULL;

	res = se_new ft_index_template_t();
	while ((uintptr_t)(p - custom_tree_buf) < custom_tree_size)
	{
		ft_index_type ind_type;
		const char *ns_uri, *ns_pref, *name;
		U_ASSERT((uintptr_t)(p + sizeof(ft_index_type) - custom_tree_buf) < custom_tree_size);
		memcpy(&ind_type, p, sizeof(ind_type));
		p += sizeof(ft_index_type);
		ns_uri = p;
		while (*p)
		{
			++p;
			U_ASSERT((uintptr_t)(p - custom_tree_buf) < custom_tree_size);
		}
		++p;
		ns_pref = p;
		while (*p)
		{
			++p;
			U_ASSERT((uintptr_t)(p - custom_tree_buf) < custom_tree_size);
		}
		++p;
		name = p;
		while (*p)
		{
			++p;
			U_ASSERT((uintptr_t)(p - custom_tree_buf) < custom_tree_size);
		}
		++p;

		xmlns_ptr ns=NULL_XMLNS;
		if (*ns_pref)
		{
			ns = xmlns_touch(ns_pref, ns_uri);
		}

		res->push_back(ft_index_pair_t(xsd::QName::createNsCn(ns, name, true), ind_type));
	}
	return res;
}

void hl_logical_log_ft_index(xpath::PathExpression *object_path, ft_index_type itconst, const char * index_title,
                             const char* doc_name,const char* options,bool is_doc,ft_custom_tree_t * custom_tree,bool inserted)
{
    if (!enable_log) return;
    number_of_records++;

    unsigned int custom_tree_size = 0;
    char *custom_tree_buf = tr_globals::e_string_buf;

    if (custom_tree != NULL)
        ft_serialize_cust_tree(custom_tree_buf, &custom_tree_size, custom_tree);

    llOperations op;
    if (is_doc)
        op = inserted ? LL_INSERT_DOC_FTS_INDEX : LL_DELETE_DOC_FTS_INDEX;
    else
        op = inserted ? LL_INSERT_COL_FTS_INDEX : LL_DELETE_COL_FTS_INDEX;

    std::string obj_path_str = object_path->toLRString();

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 6, obj_path_str.c_str(), obj_path_str.size() + 1,
            &itconst, sizeof(ft_index_type), index_title, strlen(index_title) + 1,
            doc_name, strlen(doc_name) + 1, options, strlen(options) + 1, &custom_tree_size, sizeof(unsigned),
            custom_tree_buf, (size_t)custom_tree_size);
}
#endif

#ifdef SE_ENABLE_TRIGGERS
void hl_logical_log_trigger(trigger_time tr_time, trigger_event tr_event,
                            xpath::PathExpression *trigger_path, trigger_granularity tr_gran, trigger_action_cell* trac,
                            inserting_node insnode, xpath::PathExpression *path_to_parent, const char* trigger_title,
                            const char* doc_name, bool is_doc, bool inserted)
{
    if (!enable_log) return;
    number_of_records++;

    std::string tr_path_str = trigger_path->toLRString();
    std::string tr_path_par;

    if (path_to_parent)
        tr_path_par = path_to_parent->toLRString();

    unsigned int trac_len = 0;

    for (trigger_action_cell *tr_act = trac; tr_act != NULL; tr_act = tr_act->next)
        trac_len += strlen(tr_act->statement) + 1;

    char *tr_action_buf = new char[trac_len];
    unsigned int tr_action_buf_size = 0;
    unsigned int str_len = 0;

    for (trigger_action_cell *tr_act = trac; tr_act != NULL; tr_act = tr_act->next) {
        str_len = strlen(tr_act->statement);

        U_ASSERT(tr_action_buf_size + str_len + 1 <= trac_len);

        if (str_len) {
            memcpy(tr_action_buf + tr_action_buf_size, tr_act->statement, str_len + 1);
            tr_action_buf_size += str_len + 1;
        } else {
            tr_action_buf[tr_action_buf_size] = '\x0';
            tr_action_buf_size++;
        }
    }

    llOperations op;
    if (is_doc)
        op = inserted ? LL_INSERT_DOC_TRG : LL_DELETE_DOC_TRG;
    else
        op = inserted ? LL_INSERT_COL_TRG : LL_DELETE_COL_TRG;


    size_t innname_len = insnode.name ? strlen(insnode.name) + 1 : 1;

    llLogGeneral(TR_RECORD, tr_globals::trid, op, false, 11, &tr_time, sizeof(trigger_time), &tr_event, sizeof(trigger_event),
            tr_path_str.c_str(), tr_path_str.size() + 1, &tr_gran, sizeof(trigger_granularity),
            &tr_action_buf_size, sizeof(unsigned int), tr_action_buf, (size_t)tr_action_buf_size,
            insnode.name ? insnode.name : "", innname_len, &insnode.type, sizeof(t_item),
            tr_path_par.c_str(), tr_path_par.size() + 1, trigger_title,
            strlen(trigger_title) + 1, doc_name, strlen(doc_name) + 1);

    delete[] tr_action_buf;
}
#endif

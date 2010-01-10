/*
 * File:  PPBulkLoad.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPBulkLoad.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/crmutils/crmutils.h"
#include "tr/tr_globals.h"
#include "tr/locks/locks.h"
#include "tr/log/log.h"
#include "tr/auth/auc.h"

PPBulkLoad::PPBulkLoad(PPOpIn _filename_,
                       dynamic_context *_cxt1_,
                       PPOpIn _document_,
                       dynamic_context *_cxt2_,
                       PPOpIn _collection_,
                       dynamic_context *_cxt3_) :
                                                  cxt1(_cxt1_),
                                                  cxt2(_cxt2_),
                                                  cxt3(_cxt3_),
                                                  filename(_filename_),
                                                  document(_document_),
                                                  collection(_collection_)
{
}

PPBulkLoad::~PPBulkLoad()
{
    delete filename.op;
    filename.op = NULL;

    delete document.op;
    document.op = NULL;

    if (collection.op)
    {
        delete collection.op;
        collection.op = NULL;
    }

    delete cxt1;
    cxt1 = NULL;
    delete cxt2;
    cxt2 = NULL;
    delete cxt3;
    cxt3 = NULL;
}

void PPBulkLoad::open()
{
    local_lock_mrg->lock(lm_x);

    dynamic_context::global_variables_open();
    filename.op->open();
    document.op->open();
    if (collection.op) collection.op->open();
}

void PPBulkLoad::close()
{
    filename.op->close();
    document.op->close();
    if (collection.op) collection.op->close();
    dynamic_context::global_variables_close();
}

void PPBulkLoad::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    filename.op->accept(v);
    document.op->accept(v);
    if (collection.op) collection.op->accept(v);    
    v.pop();
}

void PPBulkLoad::execute()
{
    tuple_cell tc, tc_filename, tc_document, tc_collection;
    tuple t(1);

    filename.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = filename.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    filename.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
    tc_filename = tuple_cell::make_sure_light_atomic(tc);


    document.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = document.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    document.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
    tc_document = tuple_cell::make_sure_light_atomic(tc);


    int need_cp;
    xptr doc_root;
    std::vector<std::string> filenames(1, tc_filename.get_str_mem());
    std::vector<client_file> cf_vec(1);
    tr_globals::client->get_file_from_client(&filenames, &cf_vec);
    bool write_to_logical_log;

    if (!tr_globals::is_need_checkpoint_on_transaction_commit)
    {
       if (cf_vec[0].file_size >= MAX_FILE_SIZE_WITHOUT_CHECKPOINT)
       {
          write_to_logical_log = false;
          tr_globals::is_need_checkpoint_on_transaction_commit = true;
       }
       else
          write_to_logical_log = true;
    }
    else
          write_to_logical_log = false;

    // we cannot make checkpoint before commit now
    // in this case redo info woulb be lost
    // TODO: we can fix it later by releasing transaction concurrent semaphore a bit earlier
    //write_to_logical_log = true;
    // llNeedCheckpoint() takes responsibility for truncating logical log now
    //tr_globals::is_need_checkpoint_on_transaction_commit = false;

    bool boundary_space_strip = (cxt1->st_cxt->boundary_space == xq_boundary_space_strip);

    try {
   	    if (collection.op == NULL)
        {
            local_lock_mrg->put_lock_on_document(tc_document.get_str_mem());
            auth_for_load_document(tc_document.get_str_mem());

            if (!write_to_logical_log) hl_disable_log();

            doc_root = loadfile(cf_vec[0].f,
                                *tr_globals::client->get_se_ostream(),
                                tc_document.get_str_mem(),
                                boundary_space_strip,
                                need_cp,
                                tr_globals::client->is_print_progress());

            if (!write_to_logical_log) hl_enable_log();
            if (!write_to_logical_log) hl_logical_log_document(doc_root, tc_document.get_str_mem(), NULL, true);
        }
        else
        {
            collection.op->next(t);
            if (t.is_eos()) throw USER_EXCEPTION(SE1071);

            tc = collection.get(t);
            if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
                throw USER_EXCEPTION(SE1071);

            collection.op->next(t);
            if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
            tc_collection = tuple_cell::make_sure_light_atomic(tc);


            local_lock_mrg->put_lock_on_collection(tc_collection.get_str_mem());
            auth_for_load_document_collection(tc_document.get_str_mem(), tc_collection.get_str_mem());

            if (!write_to_logical_log) hl_disable_log();

            doc_root = loadfile(cf_vec[0].f,
                                *tr_globals::client->get_se_ostream(),
                                tc_document.get_str_mem(),
                                tc_collection.get_str_mem(),
                                boundary_space_strip,
                                need_cp,
                                tr_globals::client->is_print_progress());

            if (!write_to_logical_log) hl_enable_log();
            if (!write_to_logical_log) hl_logical_log_document(doc_root, tc_document.get_str_mem(), tc_collection.get_str_mem(), true);
        }
    } catch (ANY_SE_EXCEPTION) {
        tr_globals::client->close_file_from_client(cf_vec[0]);
        throw;
    }

    tr_globals::client->close_file_from_client(cf_vec[0]);
}




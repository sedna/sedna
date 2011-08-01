/*
 * File:  PPBulkLoad.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPBulkLoad.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/updates/updates.h"
#include "tr/tr_globals.h"
#include "tr/locks/locks.h"
#include "tr/log/log.h"
#include "tr/auth/auc.h"
#include "tr/updates/bulkload.h"

PPBulkLoad::PPBulkLoad(PPOpIn _filename_,
                       PPOpIn _document_,
                       PPOpIn _collection_,
                       dynamic_context *_cxt_) : PPUpdate("PPBulkLoad"),
                                                  cxt(_cxt_),
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

    delete cxt;
    cxt = NULL;
}

void PPBulkLoad::do_open()
{
    local_lock_mrg->lock(lm_x);

    cxt->global_variables_open();
    filename.op->open();
    document.op->open();
    if (collection.op) collection.op->open();
}

void PPBulkLoad::do_close()
{
    filename.op->close();
    document.op->close();
    if (collection.op) collection.op->close();
    cxt->global_variables_close();
}

void PPBulkLoad::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    filename.op->accept(v);
    document.op->accept(v);
    if (collection.op) collection.op->accept(v);
    v.pop();
}

void PPBulkLoad::do_execute()
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

    xptr doc_root;
    std::vector<std::string> filenames(1, tc_filename.get_str_mem());
    std::vector<client_file> cf_vec(1);
    tr_globals::client->get_file_from_client(&filenames, &cf_vec);
    bool write_to_logical_log  = !tr_globals::is_need_checkpoint_on_transaction_commit;

    /*
     * The following optimization commented out by me (AK)
     *
     * There is a problem with making checkpoints before the commit:
     *    1) We cannot release locks due to problems with recovery afterwards
     *       Case: release locks (t1) -> commit (t2) -> crash (before t1's checkpoint) :
     *             recovery won't find t1, but finds t2, which could reference t1's data
     *    2) If we don't release locks there could be deadlock between t1 and t2: t2 waits for lock,
     *       t1 waits for checkpoint, but checkpoint waits for t2 to end
     *
     * So, for now this leaves us only manual optimization (LOGLESS mode), which guarantees such
     * transaction to be exclusive.
     */

    /*
    if (write_to_logical_log && cf_vec[0].file_size >= MAX_FILE_SIZE_WITHOUT_CHECKPOINT)
    {
        write_to_logical_log = false;
        tr_globals::is_need_checkpoint_on_transaction_commit = true;
    }
    */

    bool boundary_space_strip = (cxt->get_static_context()->get_boundary_space() == xq_boundary_space_strip);

    try {
        BulkLoadFrontend bulkLoadManager;
        bulkLoadManager.options.stripBoundarySpaces = boundary_space_strip;

        if (cxt->get_static_context()->getLocalOption("bulk-load-cdata-section-preserve") == "yes") {
            bulkLoadManager.options.preserveCDataSection = true;
        } else {
            bulkLoadManager.options.preserveCDataSection = false;
        }

        bulkLoadManager.setSourceFile(cf_vec[0].f);

        if (collection.op == NULL)
        {
            local_lock_mrg->put_lock_on_document(tc_document.get_str_mem());
            auth_for_load_document(tc_document.get_str_mem());

            if (!write_to_logical_log) {
                hl_disable_log();
            }

            doc_root = bulkLoadManager.loadDocument(tc_document.get_str_mem()).getPtr();

            if (!write_to_logical_log) {
                hl_enable_log();
                hl_logical_log_document(doc_root, tc_document.get_str_mem(), NULL, true);
            }
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

            if (!write_to_logical_log) {
                hl_disable_log();
            }

            doc_root = bulkLoadManager.loadCollectionDocument(tc_collection.get_str_mem(), tc_document.get_str_mem()).getPtr();

            if (!write_to_logical_log) {
                hl_enable_log();
                hl_logical_log_document(doc_root, tc_document.get_str_mem(), tc_collection.get_str_mem(), true);
            }
        }
    } catch (ANY_SE_EXCEPTION) {
        tr_globals::client->close_file_from_client(cf_vec[0]);
        throw;
    }

    tr_globals::client->close_file_from_client(cf_vec[0]);
}

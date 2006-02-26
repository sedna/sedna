/*
 * File:  PPBulkLoad.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPBulkLoad.h"
#include "crmutils.h"
#include "client_core.h"
#include "tr_globals.h"
#include "locks.h"
#include "log.h"


extern client_core *client;

PPBulkLoad::PPBulkLoad(PPOpIn _filename_,
                       PPOpIn _document_,
                       PPOpIn _collection_,
                       crmostream& _s_,
                       bool _print_progress_) : filename(_filename_),
                                                document(_document_),
                                                collection(_collection_),
                                                s(_s_),
                                                print_progress(_print_progress_)
{
}

PPBulkLoad::~PPBulkLoad()
{
    delete filename.op;
    document.op = NULL;

    delete document.op;
    document.op = NULL;

    if (collection.op)
    {
        delete collection.op;
        collection.op = NULL;
    }
}

void PPBulkLoad::open()
{
    local_lock_mrg->lock(lm_x);

    filename.op->open();
    document.op->open();
    if (collection.op) collection.op->open();
}

void PPBulkLoad::close()
{
    filename.op->close();
    document.op->close();
    if (collection.op) collection.op->close();
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
    client_file cf = client->get_file_from_client(tc_filename.get_str_mem()); 
    bool boundary_space_strip = (tr_globals::st_ct.boundary_space == xq_boundary_space_strip);

    try {
   	    if (collection.op == NULL) 
        { 
            local_lock_mrg->put_lock_on_document(tc_document.get_str_mem());
            hl_disable_log();
            doc_root = loadfile(cf.f, tc_document.get_str_mem(), boundary_space_strip, need_cp, print_progress);
            hl_enable_log();
            hl_logical_log_document(doc_root, tc_document.get_str_mem(), NULL, true);
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
            hl_disable_log();
            doc_root = loadfile(cf.f, tc_document.get_str_mem(), tc_collection.get_str_mem(), boundary_space_strip, need_cp, print_progress);
            hl_enable_log();
            hl_logical_log_document(doc_root, tc_document.get_str_mem(), tc_collection.get_str_mem(), true);
        }
    } catch (...) {
        client->close_file_from_client(cf);
        
        throw;
    }

    client->close_file_from_client(cf);
}


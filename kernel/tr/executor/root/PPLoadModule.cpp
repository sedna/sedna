/*
 * File:  PPBulkLoad.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPLoadModule.h"
#include "crmutils.h"
#include "client_core.h"
#include "tr_globals.h"
#include "locks.h"
#include "log.h"
#include "pq.h"
#include "micro.h"
#include "metadata.h"
#include "d_printf.h"

extern client_core *client;

PPLoadModule::PPLoadModule(PPOpIn _filename_,
                         PPOpIn _modulename_,
                         bool _is_load_replace_, 
                         se_ostream& _s_) : filename(_filename_),
                                            modulename(_modulename_),
                                            is_load_replace(_is_load_replace_),
                                            s(_s_)
{
}

PPLoadModule::~PPLoadModule()
{
    delete filename.op;
    filename.op = NULL;

    delete modulename.op;
    modulename.op = NULL;
}

void PPLoadModule::open()
{
    filename.op->open();
    modulename.op->open();
}

void PPLoadModule::close()
{
    filename.op->close();
    modulename.op->close();
}

void PPLoadModule::execute()
{

    tuple_cell tc, tc_filename, tc_modulename;
    tuple t(1);

    filename.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = filename.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    filename.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);        
    tc_filename = tuple_cell::make_sure_light_atomic(tc);


    modulename.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = modulename.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    modulename.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);        
    tc_modulename = tuple_cell::make_sure_light_atomic(tc);

    client_file cf;

    try {
         cf = client->get_file_from_client(tc_filename.get_str_mem());
         //precompile input module
		 std::string module_name;
         std::string module_pc_text = prepare_module(cf.f, module_name/*out*/);


         local_lock_mrg->lock(lm_x);
         local_lock_mrg->put_lock_on_collection(MODULES_COLLECTION_NAME);

         xptr doc_root, elem_ptr;
         if (is_load_replace)
         {
            try{
               delete_document(MODULES_COLLECTION_NAME, module_name.c_str());
	    } catch(SednaUserException& e) {}
         }

         doc_root = insert_document_in_collection(MODULES_COLLECTION_NAME, module_name.c_str());

         elem_ptr = insert_element(XNULL, XNULL, doc_root, "module", xs_untyped, NULL, NULL);

//d_printf2("inserting module: %s\n", module_pc_text.c_str());
         insert_text(XNULL, XNULL, elem_ptr, module_pc_text.c_str(), module_pc_text.size());

    } catch (...) {
        client->close_file_from_client(cf);
        throw;
    }

    client->close_file_from_client(cf);
}


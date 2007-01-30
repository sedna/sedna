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
#include "auc.h"
#include <functional>
#include <algorithm>

extern client_core *client;

PPLoadModule::PPLoadModule(
    arr_of_PPOpIn   _filenames_,
    bool            _is_load_replace_)
    : filenames         (_filenames_)
    , is_load_replace   (_is_load_replace_)
{
}

struct Op_deletor:
    public std::unary_function<PPOpIn, void>
{
    void operator()(PPOpIn &filename)
    {
        delete filename.op;
        filename.op = NULL;
    }
};

struct Op_opener:
    public std::unary_function<PPOpIn, void>
{
    void operator()(PPOpIn &filename)
    {
        filename.op->open();
    }
};

struct Op_closer:
    public std::unary_function<PPOpIn, void>
{
    void operator()(PPOpIn &filename)
    {
        filename.op->close();
    }
};

PPLoadModule::~PPLoadModule()
{
    std::for_each(
        filenames.begin(), filenames.end(),
        Op_deletor()
        );
}

void PPLoadModule::open()
{
    std::for_each(
        filenames.begin(), filenames.end(),
        Op_opener()
        );
}

void PPLoadModule::close()
{
    std::for_each(
        filenames.begin(), filenames.end(),
        Op_closer()
        );
}

class Tc_filename_obtainer:
    public std::unary_function<PPOpIn, tuple_cell>
{
public:
    Tc_filename_obtainer()
        : t(1)
    {}
    tuple_cell operator()(const PPOpIn &filename)
    {
        filename.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION(SE1071);

        tc = filename.get(t);
        if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
            throw USER_EXCEPTION(SE1071);

        filename.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        return tuple_cell::make_sure_light_atomic(tc);
    }
private:
    tuple_cell  tc;
    tuple       t;
};

void PPLoadModule::execute()
{
    std::vector<tuple_cell> tc_filenames(filenames.size());
    std::transform(
        filenames.begin(), filenames.end(),
        tc_filenames.begin(),
        Tc_filename_obtainer()
        );

    client_file cf;

    try {
        std::string module_name1, module_name2, module_pc_text ;
        const int fnames_size = tc_filenames.size();
        for (int i = 0; i < fnames_size; ++i)
        {
            cf = client->get_file_from_client(tc_filenames[i].get_str_mem());
            //precompile input module
            module_pc_text += prepare_module(cf.f, module_name1/*out*/);
            client->close_file_from_client(cf);

            if (i && (module_name1 != module_name2))
                throw USER_EXCEPTION2(SE1072, (module_name1 + " and " + module_name2).c_str());

            module_name2=module_name1;
        }

        local_lock_mrg->lock(lm_x);
        local_lock_mrg->put_lock_on_collection(MODULES_COLLECTION_NAME);

        xptr doc_root, elem_ptr;
        if (is_load_replace)
        {
            try{
               delete_document(MODULES_COLLECTION_NAME, module_name1.c_str());
            } catch(SednaUserException& e) {}
        }

        doc_root = insert_document_in_collection(MODULES_COLLECTION_NAME, module_name1.c_str());

        elem_ptr = insert_element(XNULL, XNULL, doc_root, "module", xs_untyped, NULL, NULL);

//d_printf2("inserting module: %s\n", module_pc_text.c_str());
        insert_text(XNULL, XNULL, elem_ptr, module_pc_text.c_str(), module_pc_text.size());
        
        auth_for_load_module(module_name1.c_str());
    } catch (...) {
        client->close_file_from_client(cf);
        throw;
    }

    client->close_file_from_client(cf);
}


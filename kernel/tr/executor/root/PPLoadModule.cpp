/*
 * File:  PPLoadModule.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <functional>
#include <algorithm>

#include "common/sedna.h"
#include "common/errdbg/d_printf.h"

#include "tr/executor/root/PPLoadModule.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/tr_globals.h"
#include "tr/locks/locks.h"
#include "tr/log/log.h"
#include "tr/xqp/modules.h"
#include "tr/mo/mo.h"
#include "tr/structures/metadata.h"
#include "tr/auth/auc.h"


PPLoadModule::PPLoadModule(arr_of_PPOpIn   _filenames_,
                           bool            _is_load_replace_,
                           dynamic_context *_cxt_) : PPUpdate("PPLoadModule"),
                                                     filenames(_filenames_),
                                                     is_load_replace(_is_load_replace_),
                                                     cxt(_cxt_)
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

struct Op_acceptor:
    public std::unary_function<PPOpIn, void>
{
private:
    PPVisitor &v;

public:
    void operator()(PPOpIn &filename)
    {
        filename.op->accept(v);
    }

    Op_acceptor(PPVisitor &_v_): v(_v_) {}
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

    delete cxt;
    cxt = NULL;
}

void PPLoadModule::do_open()
{
    cxt->global_variables_open();
    std::for_each(
        filenames.begin(), filenames.end(),
        Op_opener()
        );
}

void PPLoadModule::do_close()
{
    std::for_each(
        filenames.begin(), filenames.end(),
        Op_closer()
        );

    cxt->global_variables_close();
}

void PPLoadModule::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    std::for_each(filenames.begin(), filenames.end(), Op_acceptor(v));
    v.pop();
}

class Tc_filename_obtainer:
    public std::unary_function<PPOpIn, std::string>
{
public:
    Tc_filename_obtainer()
        : t(1)
    {}
    std::string operator()(const PPOpIn &filename)
    {
        filename.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION(SE1071);

        tc = filename.get(t);
        if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
            throw USER_EXCEPTION(SE1071);

        filename.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        return std::string(
            tuple_cell::make_sure_light_atomic(tc).get_str_mem()
            );
    }
private:
    tuple_cell tc;
    xqp_tuple t;
};

struct Client_file_closer:
    public std::unary_function<client_file, void>
{
    void operator()(client_file &cf)
    {
        tr_globals::client->close_file_from_client(cf);
    }
};

void close_all_client_files(std::vector<client_file> &cf_vec)
{
    std::for_each(
        cf_vec.begin(), cf_vec.end(),
        Client_file_closer()
        );

}

void PPLoadModule::do_execute()
{
    const int                   fnames_size = filenames.size();
    std::vector<std::string>    tc_filenames(fnames_size);
    std::transform(
        filenames.begin(), filenames.end(),
        tc_filenames.begin(),
        Tc_filename_obtainer()
        );

    std::vector<client_file> cf_vec(fnames_size);

    try {
        std::string module_pc_text, module_name;

        tr_globals::client->get_file_from_client(&tc_filenames, &cf_vec);
        module_pc_text = prepare_modules(cf_vec, &module_name);
        close_all_client_files(cf_vec);

        local_lock_mrg->lock(lm_x);
        local_lock_mrg->put_lock_on_collection(MODULES_COLLECTION_NAME);

        xptr doc_root, elem_ptr;
        if (is_load_replace)
        {
            auth_for_drop_module(module_name.c_str());
            try
            {
                delete_document_from_collection(MODULES_COLLECTION_NAME, module_name.c_str());
            }
            catch(SednaUserException) // catch to avoid 'no such document errors'
            {
            }
        }

        try
        {
            doc_root = insert_document_into_collection(MODULES_COLLECTION_NAME, module_name.c_str());
        }
        catch(SednaUserException& e)
        {
            if(e.get_code() == SE2004)
            {
                throw USER_EXCEPTION2(SE1073, module_name.c_str());
            }
            if(e.get_code() == SE2008)
            {
                throw USER_EXCEPTION2(SE2008, (std::string("Invalid module URI '") + module_name + "'").c_str());
            }
            else
            {
                throw;
            }
        }

        elem_ptr = insert_element(XNULL, XNULL, doc_root, "module", xs_untyped, NULL_XMLNS);

        insert_text(XNULL, XNULL, elem_ptr, text_source_mem(module_pc_text.c_str(), module_pc_text.size()));

        auth_for_load_module(module_name.c_str());
    } catch (ANY_SE_EXCEPTION) {
        close_all_client_files(cf_vec);
        throw;
    }

    close_all_client_files(cf_vec);
}


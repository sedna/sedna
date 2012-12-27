/*
 * File:  ext.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/protocol/sedna_ef.h"

#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/por2qep/ext.h"
#include "tr/executor/xqops/PPExtFunCall.h"
#include "tr/tr_globals.h"
#include "tr/strings/e_string.h"

#ifdef _WIN32
#else
#include <dirent.h>
#include <strings.h>
#endif

///////////////////////////////////////////////////////////////////////////////
/// ExtFunction
///////////////////////////////////////////////////////////////////////////////

SEDNA_SEQUENCE_ITEM *ExtFunction::global_result_item = NULL;
char *ExtFunction::error_msg_buf = NULL;
int ExtFunction::instance_count = 0;

ExtFunction::ExtFunction(const std::string &fname, ULibrary lib, ExtFunction **_fn_ptr_)
{
    if (global_result_item == NULL) {
        global_result_item = (SEDNA_SEQUENCE_ITEM *)malloc(sizeof(SEDNA_SEQUENCE_ITEM));
    }

    if (error_msg_buf == NULL) {
        error_msg_buf = new char[SEDNA_ERROR_MSG_BUF_SIZE]; //FIXME - use malloc?
    }
    instance_count++;

    fcxt = new func_cxt();

    fcxt->func = (sedna_ext_func_t) uGetProcAddress(lib, fname.c_str());
    if (fcxt->func == NULL) {
            throw USER_EXCEPTION2(SE2203, (std::string("failed to load function '") + fname + "'").c_str());
    }
    //NULL is ok here
    fcxt->func_init = (sedna_ext_func_init_t) uGetProcAddress(lib, (fname + "_init").c_str());
    fcxt->func_deinit = (sedna_ext_func_deinit_t) uGetProcAddress(lib, (fname + "_deinit").c_str());

    fcxt->func_initialized = false;
    fcxt->ref_count = 1;

    fcxt->init.node_buf	= global_result_item;
    fcxt->init.sedna_free		= ::free;
    fcxt->init.sedna_malloc	= ::malloc;
    fcxt->init.ptr		= NULL;

    result = NULL;

    this->fn_ptr = _fn_ptr_;
    *(this->fn_ptr) = this;
}

ExtFunction::ExtFunction(func_cxt *_fcxt_, ExtFunction **_fn_ptr_) : fcxt(_fcxt_),
                                                                     fn_ptr(_fn_ptr_),
                                                                     result(NULL)
{
    instance_count++;
    fcxt->ref_count++;
}

ExtFunction::~ExtFunction()
{
    result_clear();

    fcxt->ref_count--;
    if (fcxt->ref_count == 0)
    {
        if (fcxt->func_initialized && (fcxt->func_deinit != NULL))
        {
            error_msg_buf[0] = 0;
            fcxt->func_deinit(&fcxt->init, error_msg_buf);
            error_msg_buf[SEDNA_ERROR_MSG_BUF_SIZE-1] = 0;
            /* FIXME - this exception causes pure virtual call in tr.cpp
            if (error_msg_buf[0] != 0
                    throw USER_EXCEPTION2(SE2202, error_msg_buf);
            */
        }
        delete fcxt;
        fcxt = NULL;
        *(this->fn_ptr) = NULL;
    }

    instance_count--;
    if (instance_count == 0)
    {
        if (error_msg_buf != NULL)
        {
            delete[] error_msg_buf; //FIXME - use free?
            error_msg_buf = NULL;
        }
        if (global_result_item != NULL)
        {
            free(global_result_item);
            global_result_item = NULL;
        }
    }

}

void ExtFunction::free_item(SEDNA_SEQUENCE_ITEM *item)
{
    if (item->data.type == SEDNATYPE_string) {
        free(item->data.val_string);
    }
    if (item != global_result_item) {
        free(item);
    }
}

ExtFunction *ExtFunction::copy()
{
    return new ExtFunction(fcxt, this->fn_ptr);
}

SEDNA_SEQUENCE_ITEM *ExtFunction::make_item(const tuple &t)
{
    if (t.size() != 1) {
        throw USER_EXCEPTION2(SE1003, "bad tuple_cells count (in external function's arguments)");
    }

    tuple_cell res = atomize(t.cells[0]);

    SEDNA_SEQUENCE_ITEM *node = (SEDNA_SEQUENCE_ITEM *)malloc(sizeof(SEDNA_SEQUENCE_ITEM));
    switch (res.get_atomic_type()) {
        case xs_integer:
                node->data.type = SEDNATYPE_integer;
                node->data.val_integer = res.get_xs_integer();
                break;
        case xs_float:
                node->data.type = SEDNATYPE_float;
                node->data.val_float = res.get_xs_float();
                break;
        case xs_double:
                node->data.type = SEDNATYPE_double;
                node->data.val_double = res.get_xs_double();
                break;
        default:
                res = cast(res, xs_string);
                node->data.type = SEDNATYPE_string;
                str_off_t len = res.get_strlen();
        //TODO: check len < MAX_WE_CAN_ALLOC
                U_ASSERT(len >= 0 && len < SIZE_MAX);
                node->data.val_string = (SEDNA_string)malloc((size_t)len+1);
                res.copy_string(node->data.val_string);
                break;
    }
    node->next = NULL;
    return node;
}

SEDNA_SEQUENCE_ITEM *ExtFunction::get_ef_seq(const PPOpIn &inp)
{
    tuple t(inp.ts);

    inp.op->next(t);
    if (t.is_eos()) {
        return NULL;
    } else {
        SEDNA_SEQUENCE_ITEM *first, *cur;
        first = make_item(t);
        cur = first;
        inp.op->next(t);
        while (!t.is_eos())
        {
            cur->next = make_item(t);
            cur = cur->next;
            inp.op->next(t);
        }
        return first;
    }
}

void ExtFunction::make_ef_args(SEDNA_EF_ARGS &args, const arr_of_PPOpIn &arr)
{
    args.length = arr.size();
    if (args.length < 1)
    {
            args.args = NULL;
            return;
    }
    args.args = (SEDNA_SEQUENCE_ITEM **)malloc(sizeof(SEDNA_SEQUENCE_ITEM *) * args.length);
    for (int i = 0; i < args.length; i++) {
            args.args[i] = get_ef_seq(arr[i]);
    }
}

void ExtFunction::clear_ef_args(SEDNA_EF_ARGS &args)
{
    if (args.length < 1) {
        return;
    }
    for (int i = 0; i < args.length; i++) {
        SEDNA_SEQUENCE_ITEM *item = args.args[i];
        while (item != NULL)
        {
            SEDNA_SEQUENCE_ITEM *next = item->next;
            free_item(item);
            item = next;
        }
    }
    free(args.args);
    args.length = 0;
}

void ExtFunction::invoke(const arr_of_PPOpIn &arr)
{
    result_clear();

    if (!fcxt->func_initialized)
    {
        if (fcxt->func_init != NULL)
        {
            error_msg_buf[0] = 0;
            fcxt->func_init(&fcxt->init, error_msg_buf);
            error_msg_buf[SEDNA_ERROR_MSG_BUF_SIZE-1] = 0;
            if (error_msg_buf[0] != 0) {
                throw USER_EXCEPTION2(SE2200, error_msg_buf);
            }
        }
        fcxt->func_initialized = true;
    }

    make_ef_args(args, arr);
    fcxt->init.node_buf->next = NULL;
    error_msg_buf[0] = 0;
    result = fcxt->func(&fcxt->init, &args, error_msg_buf);
    error_msg_buf[SEDNA_ERROR_MSG_BUF_SIZE-1] = 0;
    if (error_msg_buf[0] != 0) {
        throw USER_EXCEPTION2(SE2201, error_msg_buf);
    }
    clear_ef_args(args);
}

void ExtFunction::result_skip()
{
    if (result == NULL) {
        return;
    }
    SEDNA_SEQUENCE_ITEM *item = result;
    result = item->next;
    free_item(item);
}

void ExtFunction::result_clear()
{
    while (result != NULL) {
        result_skip();
    }
}
void ExtFunction::result_peek(tuple &t)
{
    if (result == NULL) {
        t.set_eos();
        return;
    }
    switch (result->data.type) {
        case SEDNATYPE_string:
                t.copy(tuple_cell::atomic_deep(xs_string, result->data.val_string));
                break;
        case SEDNATYPE_integer:
                t.copy(tuple_cell::atomic((int64_t)(result->data.val_integer)));
                break;
        case SEDNATYPE_float:
                t.copy(tuple_cell::atomic(result->data.val_float));
                break;
        case SEDNATYPE_double:
                t.copy(tuple_cell::atomic(result->data.val_double));
                break;
        default:
                t.set_eos();
                break;
    }
}
void ExtFunction::result_next(tuple &t)
{
    result_peek(t);
    result_skip();
}

///////////////////////////////////////////////////////////////////////////////
/// ExtFunctionManager
///////////////////////////////////////////////////////////////////////////////

ExtFunctionManager ext_function_manager = ExtFunctionManager();

void ExtFunctionManager::add_func_to_list(const char *fname, const ULibrary lib)
{
    std::string *func_name = new std::string(fname);
    func_list_t::const_iterator it = func_list.find(*func_name);
    if (it != func_list.end()) {
        return; //it's ok, assume that same function was found in another dll-file, FIXME: warning if files are in the same dir?
    }
    ext_function_desc *desc = new ext_function_desc(lib);
    func_list[*func_name] = desc;
}

void ExtFunctionManager::load_func_list_from_lib(const char *lib)
{
    ULibrary hLib = uLoadLibrary(lib);
    if (hLib == UDL_INVALID_LIBRARY) {
        return;
    }

    const char **ef_names = (const char **)uGetProcAddress(hLib, "ef_names");
    if (ef_names == NULL) {
        uFreeLibrary(hLib);
        return;
    }

    int i = 0;
    while (ef_names[i] != 0) {
        add_func_to_list(ef_names[i], hLib);
        i++;
    }
}

void ExtFunctionManager::load_func_list_one_dir(const std::string &path)
{
#ifdef _WIN32
    std::string search_path = path + "*.dll";
    HANDLE hFind;
    WIN32_FIND_DATA FindFileData;

    hFind = FindFirstFile(search_path.c_str(), &FindFileData);
    if (hFind == INVALID_HANDLE_VALUE) {
        return;
    } else {
        while (true) {
            std::string tmp_str = path + FindFileData.cFileName;
            load_func_list_from_lib(tmp_str.c_str());
            if (FindNextFile(hFind, &FindFileData) == 0) {
                break;
            }
        }
    }

#else // UNIX
    DIR *dir = opendir(path.c_str());
    struct dirent *ent;
    if (dir == NULL) {
        return;
    } 
    
    while ((ent = readdir(dir)) != NULL) {
        char *tmp = ent->d_name + strlen(ent->d_name) - 3;
        if (!strcmp(tmp, ".so")) {
            std::string tmp_str = path + ent->d_name;
            load_func_list_from_lib(tmp_str.c_str());
        }
    }

    closedir(dir);

#endif
}


void ExtFunctionManager::load_func_list()
{
    if (loaded_func_list) {
        return;
    }
    //if something fails, we don't want to try again next time
    loaded_func_list = true;

    load_func_list_one_dir(tr_globals::databaseOptions.dataFilePath + "/lib/");
    load_func_list_one_dir(tr_globals::sednaOptions.dataDirectory + "/lib/");
}


PPIterator *ExtFunctionManager::make_pp_ext_func(const char *name, dynamic_context *cxt, operation_info info, arr_of_PPOpIn arr)
{
    std::string name_str(name);
    load_func_list();
    ext_function_desc *fdesc = func_list[name_str];
    if (fdesc == NULL) {
            throw USER_EXCEPTION2(SE1003, (std::string("external function implementation not found: '") + name_str + "'").c_str());
    }  

    ExtFunction *fn;
    if (fdesc->fn == NULL) {
        fn = new ExtFunction(name_str, func_list[name_str]->lib, &fdesc->fn);
    } else {
        fn = fdesc->fn->copy();
    }

    return new PPExtFunCall(cxt, info, arr, fn, name_str);
}

ExtFunctionManager::~ExtFunctionManager() { }


/*
 * File:  ext.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _EXT_H
#define _EXT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "common/sedna_ef.h"
#include "common/u/udl.h"

//supported atomic types are defined in sedna_ef.h
//adding new types requires modification of:
//	enum sedna_atomic_type	in sedna_ef.h
//	struct sedna_atomic_value	in sedna_ef.h
//	ExtFunction::make_item		in ext.cpp
//	ExtFunction::free_item		in ext.cpp (if needed)
//	ExtFunction::result_peek	in ext.cpp
//	documentation			in ProgGuide.tex

typedef SEDNA_SEQUENCE_ITEM *(* sedna_ext_func_t)(SEDNA_EF_INIT *init, SEDNA_EF_ARGS *args, char * error_msg_buf);
typedef void (* sedna_ext_func_init_t)(SEDNA_EF_INIT *init, char * error_msg_buf);
typedef void (* sedna_ext_func_deinit_t)(SEDNA_EF_INIT *init, char * error_msg_buf);

class ExtFunction
{
private:
	static SEDNA_SEQUENCE_ITEM *global_result_item;
	static char *error_msg_buf;
	//counter for instances using global_result_item & error_msg_buf
	static int instance_count;


	SEDNA_EF_ARGS			args;
	struct func_cxt {
		SEDNA_EF_INIT			init;
		sedna_ext_func_t		func;
		sedna_ext_func_init_t	func_init;
		sedna_ext_func_deinit_t	func_deinit;
		bool func_initialized;
		int ref_count;
	};
	func_cxt *fcxt;
	ExtFunction **fn_ptr;
    std::string function_name;

	SEDNA_SEQUENCE_ITEM *result;
	SEDNA_SEQUENCE_ITEM *make_item(const xqp_tuple &t);
	SEDNA_SEQUENCE_ITEM *get_ef_seq(const PPOpIn &inp);
	void make_ef_args(SEDNA_EF_ARGS &args, const arr_of_PPOpIn &arr);
	void clear_ef_args(SEDNA_EF_ARGS &args);
	void free_item(SEDNA_SEQUENCE_ITEM *item);
public:
	ExtFunction(const std::string &fname, ULibrary lib, ExtFunction **_fn_ptr_);
	/* For PPExtFunCall copy call */
    ExtFunction(func_cxt *_fcxt_, ExtFunction **_fn_ptr_);
	~ExtFunction();
	ExtFunction *copy();

	void invoke(const arr_of_PPOpIn &arr);
	void result_skip();
	void result_clear();
	void result_peek(xqp_tuple &t);
	void result_next(xqp_tuple &t);
};

class ExtFunctionManager
{
private:
	struct ext_function_desc {
		ULibrary lib;
		ExtFunction *fn;
		ext_function_desc(ULibrary _lib_) : lib(_lib_), fn(NULL) {}
	};
	bool loaded_func_list;
	typedef std::map<std::string, ext_function_desc*> func_list_t;
	func_list_t func_list;

	void add_func_to_list(const char *fname, const ULibrary lib);
	void load_func_list_from_lib(const char *lib);
	void load_func_list_one_dir(const std::string &path);
	void load_func_list();
public:
	ExtFunctionManager() {}
	~ExtFunctionManager();
	PPIterator *make_pp_ext_func(const char *name, dynamic_context *cxt, operation_info info, arr_of_PPOpIn arr);
};

extern ExtFunctionManager ext_function_manager;

#endif //_EXT_H

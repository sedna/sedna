/*
 * File:  PPSQL.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSQL_H
#define __PPSQL_H

#include <vector>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPConstructors.h"

class IElementProducer;

enum sql_handle_type {
	SQLH_CONNECTION,
	SQLH_PREPARED_STMT
};

class SQLExecutor;

class SQLHandle
{
public:
	sql_handle_type type;

	SQLHandle(sql_handle_type _type_) : type(_type_) {}

	virtual SQLExecutor*	get_executor() = 0;
	virtual void			release_executor(SQLExecutor *executor) = 0;
	virtual SQLHandle*		prepare_stmt(const char *query, int query_len, PPOpIn *options) = 0;

	virtual void			close() = 0;
	virtual void			commit() = 0;
	virtual void			rollback() = 0;

	virtual ~SQLHandle() {};
};

class SQLExecutor
{
public:
	virtual void execute_query (const char *query, int query_len, PPOpIn *options) = 0;
	virtual void execute_prepared(arr_of_PPOpIn params) = 0;
	virtual void close_query() = 0;
	virtual void fetch(tuple &t, IElementProducer * producer) = 0;
	virtual int  update_row_count() = 0;

	virtual bool is_query_active() = 0;

	virtual ~SQLExecutor();
};

class SQLConnection : public SQLHandle
{
public:
	SQLConnection() : SQLHandle(SQLH_CONNECTION) {}

	virtual ~SQLConnection();
};

class SQLDriver
{
public:
	virtual const char*		prefix() const = 0;
	virtual const int		prefix_len() const = 0;
	virtual SQLConnection*	new_connection(const char *connect_str, int connect_str_len,
											  const char *uid, int uid_len,
											  const char *pass, int pass_len, PPOpIn *options) = 0;

	virtual ~SQLDriver();
};

extern int SQLOptionGetBool(const char *value);
extern bool SQLOptionParse(const tuple_cell &opt, char *&opt_name, char *&opt_value);

class SQLHandleManager
{
	std::vector<SQLHandle *> handles;

	SQLDriver *sql_odbc_driver;
public:
	SQLHandleManager();
	~SQLHandleManager();

	int		new_handle(SQLHandle *h);
	bool	delete_handle(int h);

	int		new_connection(const char *connect_str, int connect_str_len,
									const char *uid, int uid_len,
									const char *pass, int pass_len, PPOpIn *options);

	SQLHandle*	get_handle(int h);
};

class PPFnSQLBase : public PPConstructor
{
protected:
	static SQLHandleManager *sql_handle_manager;
	static bool firstBaseCons;
	static bool checkBaseInitial();

	bool handle_manager_carrier;

	PPFnSQLBase(dynamic_context *_cxt_,
                operation_info _info_);
};


class PPFnSQLConnect : public PPFnSQLBase
{
protected:

	arr_of_PPOpIn	arr;
	bool			first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnSQLConnect(dynamic_context *_cxt_,
                   operation_info _info_,
                   const arr_of_PPOpIn &arr);
    virtual ~PPFnSQLConnect();
};

class PPFnSQLExecute : public PPFnSQLBase
{
protected:
	arr_of_PPOpIn	arr;
	bool			exec_update;
	bool			first_time;
	SQLHandle		*handle;
	SQLExecutor		*executor;

	void release_executor();
	void get_executor();

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnSQLExecute(dynamic_context *_cxt_,
                   operation_info _info_,
                   const arr_of_PPOpIn &arr,
                   bool _exec_update_);
    virtual ~PPFnSQLExecute();
    inline bool is_update() const { return exec_update; }
};

class PPFnSQLPrepare : public PPFnSQLBase
{
private:
	bool	has_options;
	PPOpIn	connection;
	PPOpIn	statement;
	PPOpIn	options;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
	PPFnSQLPrepare(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _connection_,
                   PPOpIn _statement_,
                   PPOpIn _options_);

	PPFnSQLPrepare(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _connection_,
                   PPOpIn _statement_);
    virtual ~PPFnSQLPrepare();
};

class PPFnSQLClose : public PPFnSQLBase
{
private:
	PPOpIn	connection;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
	PPFnSQLClose(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _connection_);

    virtual ~PPFnSQLClose();
};

class PPFnSQLCommit : public PPFnSQLBase
{
private:
	PPOpIn	connection;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
	PPFnSQLCommit(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _connection_);
    virtual ~PPFnSQLCommit();
};

class PPFnSQLRollback : public PPFnSQLBase
{
private:
	PPOpIn	connection;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
	PPFnSQLRollback(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _connection_);
    virtual ~PPFnSQLRollback();
};


#endif

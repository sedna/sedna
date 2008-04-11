/*
 * File:  PPSQL.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPSQL.h"
#include "tr/executor/xqops/PPSQLODBC.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"

const char *sqlns_uri = "http://modis.ispras.ru/Sedna/SQL";

SQLHandleManager *PPFnSQLBase::sql_handle_manager = NULL;
bool PPFnSQLBase::firstBaseCons = true;

SQLExecutor::~SQLExecutor()
{
}

SQLConnection::~SQLConnection()
{
}

SQLDriver::~SQLDriver()
{
}

int SQLOptionGetBool(const char *value)
{
	if (!value)
		return -1;
	if (   (value[0] == '1' && value[1] == 0)
		|| ((value[0] == 'o' || value[0] == 'O') && 
		    (value[1] == 'n' || value[1] == 'N') && value[2] == 0)
		|| ((value[0] == 'y' || value[0] == 'Y') && 
			(value[1] == 'e' || value[1] == 'E') && 
			(value[2] == 's' || value[2] == 'S') && value[3] == 0))
			return 1;
	if (   (value[0] == '0' && value[1] == 0)
		|| ((value[0] == 'n' || value[0] == 'N') && 
		    (value[1] == 'o' || value[1] == 'O') && value[2] == 0)
		|| ((value[0] == 'o' || value[0] == 'O') && 
			(value[1] == 'f' || value[1] == 'F') && 
			(value[2] == 'f' || value[2] == 'F') && value[3] == 0))
			return 0;

	return -1;
}

static char *copy_attr_value(xptr attr)
{
    tuple_cell tc = dm_string_value(attr);
	char *value = se_new char[tc.get_strlen()+1];
    return tc.copy_string(value);
}

bool SQLOptionParse(const tuple_cell &opt, char *&opt_name, char *&opt_value)
{
	if (opt.is_node())
	{
		xptr node = opt.get_node();

		CHECKP(node);
		xml_ns *ns = GETSCHEMENODE(XADDR(node))->xmlns;

		if (!ns)
			return false;
		if (!ns->uri)
			return false;
		if (strcmp(ns->uri, sqlns_uri))
			return false;
		
		char *elem_name = GETSCHEMENODE(XADDR(node))->name;
		if (strcmp(elem_name, "option"))
			return false;


		opt_name = NULL;
		opt_value = NULL;
		//FIXME - CHECKP(node)???
		node = getFirstByOrderAttributeChild(node);
		while (node != XNULL)
		{
			char *atr_name;
			CHECKP(node);
			atr_name = GETSCHEMENODE(XADDR(node))->name;
			if (!strcmp(atr_name, "name"))
				opt_name = copy_attr_value(node);
			else if (!strcmp(atr_name, "value"))
				opt_value = copy_attr_value(node);
			else
			{
				if (opt_name)
					delete[] opt_name;
				if (opt_value)
					delete[] opt_value;
				return false;
			}

			node = getNextByOrderAttribute(node);
		}

		if (!opt_name)
		{
			if (opt_value)
				delete[] opt_value;
		}
		return true;
	}
	else
		return false;
}

SQLHandleManager::SQLHandleManager()
{
	sql_odbc_driver = se_new SQLODBCDriver();
}

SQLHandleManager::~SQLHandleManager()
{
	int h;

	for (h = handles.size()-1; h >= 0; h--)
	{
		SQLHandle *handle = handles[h];
		if (handle && handle->type == SQLH_CONNECTION)
		{
			delete handle;
			handles[h] = NULL;
		}
	}
	
	delete sql_odbc_driver;
}

int SQLHandleManager::new_handle(SQLHandle *h)
{
	if (h == NULL)
		return 0;

	handles.push_back(h);
	return handles.size();
}

bool SQLHandleManager::delete_handle(int h)
{
	if ((h < 1) || (h > handles.size()))
		return false;

	SQLHandle *handle = handles[h-1];
	delete handle;
	handles[h-1] = NULL;

	return true;
}

int SQLHandleManager::new_connection(char *connect_str, int connect_str_len, 
										char *uid, int uid_len,
										char *pass, int pass_len, PPOpIn *options)
{
	if (NULL == connect_str)
		return 0;

	SQLConnection *conn = NULL;

	if (connect_str_len >= sql_odbc_driver->prefix_len() &&
		!strncmp(connect_str, sql_odbc_driver->prefix(), sql_odbc_driver->prefix_len()))
	{
		int l = sql_odbc_driver->prefix_len();
		conn = sql_odbc_driver->new_connection(connect_str + l, connect_str_len - l, uid, uid_len, pass, pass_len, options);
	}

	if (NULL != conn)
		return new_handle(conn);

	return 0;
}

SQLHandle*	SQLHandleManager::get_handle(int h)
{
	if ((h < 1) || ((unsigned int)h > handles.size()))
		return NULL;

	return handles[h-1];
}

bool PPFnSQLBase::checkBaseInitial()
{
	if (firstBaseCons)
	{
		firstBaseCons = false;
		sql_handle_manager = se_new SQLHandleManager();
		return true;
	}
	else
		return false;
}

PPFnSQLBase::PPFnSQLBase(dynamic_context *_cxt_) : PPConstructor(_cxt_,false)
{
	handle_manager_carrier = false;
}

PPFnSQLConnect::PPFnSQLConnect(dynamic_context *_cxt_, const arr_of_PPOpIn &_arr_) : PPFnSQLBase(_cxt_), arr(_arr_)
{
}

PPFnSQLConnect::~PPFnSQLConnect()
{
	for (int i = 0; i < arr.size(); i++)
	{
		delete arr[i].op;
		arr[i].op = NULL;
	}
	if (handle_manager_carrier)
	{
		firstBaseCons = true;
		handle_manager_carrier = false;
		delete sql_handle_manager;
		sql_handle_manager = NULL;
	}
}


void PPFnSQLConnect::open ()
{
	handle_manager_carrier = checkBaseInitial();
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->open();

	first_time = true;
}

void PPFnSQLConnect::reopen ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->reopen();

	first_time = true;
}

void PPFnSQLConnect::close ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->close();
}

//FIXME
static op_str_buf str_val;
//copied from PPConstructors.cpp
static char *getStringParameter(PPOpIn content)
{
	str_val.clear();
	tuple value(content.ts);
	content.op->next(value);
	sequence at_vals(1);
	if (value.is_eos()) 
	{
	 	str_val.append(EMPTY_STRING_TC);
		return NULL;
	}
	else
	{
		
		at_vals.add(value);
		/*tuple_cell res=atomize(value.cells[0]);
		res=cast_to_xs_string(res);
		sbuf->append(res);*/
		content.op->next(value);

	}
//	int charsize=1;
	while (!(value.is_eos()))
	{
		if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPSQL");
		at_vals.add(value);		
	}
	sequence::iterator it=at_vals.begin();
	do
	{
		tuple_cell res=atomize((*it).cells[0]);
		res=cast(res, xs_string);
		res=tuple_cell::make_sure_light_atomic(res);
		if (it!=at_vals.begin())
		{
			str_val.append(" ");				
		}
		str_val.append(res);
		it++;
        
	}
	while (it!=at_vals.end());
	//str_val.push_to_memory();
	return str_val.c_str();
}

void PPFnSQLConnect::next(tuple &t)
{
	SET_CURRENT_PP(this);
	
	if (first_time)
	{
		first_time = false;
		char *tmp;

		char	*connect_str	= NULL;
		int		connect_str_len	= 0;
		char	*uid			= NULL;
		int		uid_len			= 0;
		char	*pass			= NULL;
		int		pass_len		= 0;

		tmp = getStringParameter(arr[0]);
		//connect_str		= tmp.get_str_mem();
		connect_str_len	= strlen(tmp);
		connect_str = se_new char[connect_str_len+1];
		strncpy(connect_str, tmp, connect_str_len);
		connect_str[connect_str_len] = 0;
		

		if (arr.size() >= 2)
		{
			tmp = getStringParameter(arr[1]);
			//uid		= tmp.get_str_mem();
			uid_len = strlen(tmp);
			uid = se_new char[uid_len+1];
			strncpy(uid, tmp, uid_len);
			uid[uid_len] = 0;
			
			if (arr.size() >= 3)
			{
				tmp = getStringParameter(arr[2]);
				pass		= tmp;
				pass_len	= strlen(tmp);
			}
		}
		PPOpIn *options = NULL;
		if (arr.size() >= 4)
			options = &arr[3];
		int h = sql_handle_manager->new_connection(connect_str, connect_str_len, 
			uid, uid_len, pass, pass_len, options);
		t.copy(tuple_cell::atomic((__int64)h));

		delete connect_str;
		if (uid != NULL)
			delete uid;
	}
	else
	{
		first_time = true;
		t.set_eos();
	}

	RESTORE_CURRENT_PP;
}

PPIterator* PPFnSQLConnect::copy(dynamic_context *_cxt_)
{
    PPFnSQLConnect *res = se_new PPFnSQLConnect(_cxt_, arr);

	for (int it = 0; it < arr.size(); it++)
		res->arr[it].op = arr[it].op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);

	return res;
}

PPFnSQLExecute::PPFnSQLExecute(dynamic_context *_cxt_, const arr_of_PPOpIn &_arr_, bool _exec_update_) : PPFnSQLBase(_cxt_), 
									arr(_arr_), handle(NULL), executor(NULL), exec_update(_exec_update_)
{
}

PPFnSQLExecute::~PPFnSQLExecute()
{
	for (int i = 0; i < arr.size(); i++)
	{
		delete arr[i].op;
		arr[i].op = NULL;
	}

	if (schema_carrier)
	{
		nid_delete(virt_root);
		root_schema->delete_scheme_node();
		firstCons=true; // XXX - !!! false !!! There was false before... (Andrey)
	}
	if (handle_manager_carrier)
	{
		firstBaseCons = true;
		handle_manager_carrier = false;
		delete sql_handle_manager;
		sql_handle_manager = NULL;
	}
}

void PPFnSQLExecute::release_executor()
{
	if (NULL == executor)
		return;

	handle->release_executor(executor);
	executor = NULL;
}

void PPFnSQLExecute::get_executor()
{
	if (NULL == handle)
		return;

	executor = handle->get_executor();
}


void PPFnSQLExecute::open ()
{
	handle_manager_carrier = checkBaseInitial();
    schema_carrier = checkInitial();

	for (unsigned int i = 0; i < arr.size(); i++)
	{
		arr[i].op->open();
	}

	release_executor();
	first_time = true;
}

void PPFnSQLExecute::reopen ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->reopen();

	release_executor();
	first_time = true;
}

void PPFnSQLExecute::close ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->close();
}

void PPFnSQLExecute::next(tuple &t)
{
	SET_CURRENT_PP(this);
	
	if (first_time)
	{
		first_time = false;
		tuple tmp(1);
		tuple_cell tmp_cell;

		arr[0].op->next(tmp);

		if (tmp.is_eos())
			throw XQUERY_EXCEPTION(XPTY0004);

		handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

		arr[0].op->next(tmp);
		if (!tmp.is_eos())
			throw XQUERY_EXCEPTION(XPTY0004);

		if (handle == NULL)
			throw XQUERY_EXCEPTION(SE2101);

		get_executor();
		if (NULL == executor)
			throw XQUERY_EXCEPTION(SE2102);

		//TODO - check wherer prepared stmt or not here
		switch (handle->type)
		{
			case SQLH_CONNECTION:
				//TODO - throw exception instead, this should never happen
				if (NULL == executor || arr.size() < 2)
				{
					t.set_eos();
					first_time = true;
					{RESTORE_CURRENT_PP; return;}
				}

				{
					const char *query	= getStringParameter(arr[1]);
					int	query_len = strlen(query);

					//TODO - options
					executor->execute_query(query, query_len, NULL);
				}
				
				break;
			case SQLH_PREPARED_STMT:
				executor->execute_prepared(arr);
				break;
			default:
				throw XQUERY_EXCEPTION(SE2103);
		}

		if (exec_update)
		{
			t.copy(tuple_cell::atomic((__int64)executor->update_row_count()));
			{RESTORE_CURRENT_PP; return;}
		}
	}

	if (exec_update)
	{
		t.set_eos();
		first_time = true;
	}
	else
	{
		executor->fetch(t, virt_root, last_elem);
		if (t.is_eos()) {
			first_time = true;
		}
	}

	RESTORE_CURRENT_PP;
}

PPIterator* PPFnSQLExecute::copy(dynamic_context *_cxt_)
{
	PPFnSQLExecute *res = se_new PPFnSQLExecute(_cxt_, arr, exec_update);

	for (int it = 0; it < arr.size(); it++)
		res->arr[it].op = arr[it].op->copy(_cxt_);

    return res;
}


//////////////////////////////////////////////////////////////////////////
// PPFnSQLPrepare

PPFnSQLPrepare::PPFnSQLPrepare(dynamic_context *_cxt_, PPOpIn _connection_, PPOpIn _statement_,
							   PPOpIn _options_) : PPFnSQLBase(_cxt_), has_options(true),
							   connection(_connection_), statement(_statement_), options(_options_)
{
}
PPFnSQLPrepare::PPFnSQLPrepare(dynamic_context *_cxt_, PPOpIn _connection_, PPOpIn _statement_)
								: PPFnSQLBase(_cxt_), has_options(false), connection(_connection_),
							   statement(_statement_)
{
}



PPFnSQLPrepare::~PPFnSQLPrepare()
{
	if (handle_manager_carrier)
	{
		firstBaseCons = true;
		handle_manager_carrier = false;
		delete sql_handle_manager;
		sql_handle_manager = NULL;
	}
}
void PPFnSQLPrepare::open ()
{
	handle_manager_carrier = checkBaseInitial();
	connection.op->open();
	statement.op->open();
	if (has_options)
		options.op->open();

	first_time = true;
}
void PPFnSQLPrepare::reopen ()
{
	connection.op->reopen();
	statement.op->reopen();
	if (has_options)
		options.op->reopen();

	first_time = true;
}


void PPFnSQLPrepare::close()
{
	connection.op->close();
	statement.op->close();
	if (has_options)
		options.op->close();
}
void PPFnSQLPrepare::next(tuple &t)
{
	SET_CURRENT_PP(this);
	
	if (first_time)
	{
		SQLHandle *handle;
		tuple tmp(1);
		//tuple_cell tmp_cell;

		first_time = false;

		connection.op->next(tmp);

		if (tmp.is_eos())
			throw XQUERY_EXCEPTION(XPTY0004);

		handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

		connection.op->next(tmp);
		if (!tmp.is_eos())
			throw XQUERY_EXCEPTION(XPTY0004);

		if (handle == NULL)
			throw XQUERY_EXCEPTION(SE2101);

		if (handle->type != SQLH_CONNECTION)
			throw XQUERY_EXCEPTION(SE2104);

		char *query	= NULL;
		int	query_len = 0;
		query		= getStringParameter(statement);
		query_len	= strlen(query);

		SQLHandle *stmt;
		if (has_options)
			stmt = handle->prepare_stmt(query, query_len, &options);
		else
			stmt = handle->prepare_stmt(query, query_len, NULL);

		t.copy(tuple_cell::atomic((__int64)sql_handle_manager->new_handle(stmt)));
	}
	else
	{
		first_time = true;
		t.set_eos();
	}

	RESTORE_CURRENT_PP;
}
PPIterator * PPFnSQLPrepare::copy(dynamic_context *_cxt_)
{
	PPFnSQLPrepare *res;
	if (has_options)
		res = se_new PPFnSQLPrepare(_cxt_, connection, statement, options);
	else
		res = se_new PPFnSQLPrepare(_cxt_, connection, statement);

	res->connection.op = connection.op->copy(_cxt_);
	res->statement.op = statement.op->copy(_cxt_);
	if (has_options)
		res->options.op = options.op->copy(_cxt_);

	return res;
}

//////////////////////////////////////////////////////////////////////////
// PPFnSQLClose
PPFnSQLClose::PPFnSQLClose(dynamic_context *_cxt_, PPOpIn _connection_) : PPFnSQLBase(_cxt_), connection(_connection_)
{
}
PPFnSQLClose::~PPFnSQLClose()
{
	if (handle_manager_carrier)
	{
		firstBaseCons = true;
		handle_manager_carrier = false;
		delete sql_handle_manager;
		sql_handle_manager = NULL;
	}
}
void PPFnSQLClose::open ()
{
	handle_manager_carrier = checkBaseInitial();
	connection.op->open();
}
void PPFnSQLClose::reopen ()
{
	connection.op->reopen();
}
void PPFnSQLClose::close()
{
	connection.op->close();
}
void PPFnSQLClose::next(tuple &t)
{
	SET_CURRENT_PP(this);
	
	int handle_id;
	SQLHandle *handle;
	tuple tmp(1);
	tuple_cell tmp_cell;

	connection.op->next(tmp);

	if (tmp.is_eos())
		throw XQUERY_EXCEPTION(XPTY0004);

	handle_id = tmp.cells[0].get_xs_integer();
	handle = sql_handle_manager->get_handle(handle_id);

	connection.op->next(tmp);
	if (!tmp.is_eos())
		throw XQUERY_EXCEPTION(XPTY0004);

	if (handle == NULL)
		throw XQUERY_EXCEPTION(SE2101);

	if (handle->type != SQLH_CONNECTION)
		throw XQUERY_EXCEPTION(SE2104);

	handle->close();
	//TODO - free handle? would require either releasing all executors 
	//or delaying connecton object destruction

	t.set_eos();

	RESTORE_CURRENT_PP;
}

PPIterator * PPFnSQLClose::copy(dynamic_context *_cxt_)
{
	PPFnSQLClose *res;
	res = se_new PPFnSQLClose(_cxt_, connection);

	res->connection.op = connection.op->copy(_cxt_);

	return res;
}

//////////////////////////////////////////////////////////////////////////
// PPFnSQLCommit
PPFnSQLCommit::PPFnSQLCommit(dynamic_context *_cxt_, PPOpIn _connection_) : PPFnSQLBase(_cxt_), connection(_connection_)
{
}
PPFnSQLCommit::~PPFnSQLCommit()
{
	if (handle_manager_carrier)
	{
		firstBaseCons = true;
		handle_manager_carrier = false;
		delete sql_handle_manager;
		sql_handle_manager = NULL;
	}
}
void PPFnSQLCommit::open ()
{
	handle_manager_carrier = checkBaseInitial();
	connection.op->open();
}
void PPFnSQLCommit::reopen ()
{
	connection.op->reopen();
}
void PPFnSQLCommit::close()
{
	connection.op->close();
}
void PPFnSQLCommit::next(tuple &t)
{
	SET_CURRENT_PP(this);
	
	SQLHandle *handle;
	tuple tmp(1);
	tuple_cell tmp_cell;

	connection.op->next(tmp);

	if (tmp.is_eos())
		throw XQUERY_EXCEPTION(XPTY0004);

	handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

	connection.op->next(tmp);
	if (!tmp.is_eos())
		throw XQUERY_EXCEPTION(XPTY0004);

	if (handle == NULL)
		throw XQUERY_EXCEPTION(SE2101);

	if (handle->type != SQLH_CONNECTION)
		throw XQUERY_EXCEPTION(SE2104);

	handle->commit();

	t.set_eos();

	RESTORE_CURRENT_PP;
}

PPIterator * PPFnSQLCommit::copy(dynamic_context *_cxt_)
{
	PPFnSQLCommit *res;
	res = se_new PPFnSQLCommit(_cxt_, connection);

	res->connection.op = connection.op->copy(_cxt_);

	return res;
}

//////////////////////////////////////////////////////////////////////////
// PPFnSQLRollback
PPFnSQLRollback::PPFnSQLRollback(dynamic_context *_cxt_, PPOpIn _connection_) : PPFnSQLBase(_cxt_), connection(_connection_)
{
}
PPFnSQLRollback::~PPFnSQLRollback()
{
	if (handle_manager_carrier)
	{
		firstBaseCons = true;
		handle_manager_carrier = false;
		delete sql_handle_manager;
		sql_handle_manager = NULL;
	}
}
void PPFnSQLRollback::open ()
{
	handle_manager_carrier = checkBaseInitial();
	connection.op->open();
}
void PPFnSQLRollback::reopen ()
{
	connection.op->reopen();
}
void PPFnSQLRollback::close()
{
	connection.op->close();
}
void PPFnSQLRollback::next(tuple &t)
{
	SET_CURRENT_PP(this);
	
	SQLHandle *handle;
	tuple tmp(1);
	tuple_cell tmp_cell;

	connection.op->next(tmp);

	if (tmp.is_eos())
		throw XQUERY_EXCEPTION(XPTY0004);

	handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

	connection.op->next(tmp);
	if (!tmp.is_eos())
		throw XQUERY_EXCEPTION(XPTY0004);

	if (handle == NULL)
		throw XQUERY_EXCEPTION(SE2101);

	if (handle->type != SQLH_CONNECTION)
		throw XQUERY_EXCEPTION(XPTY0004);

	handle->rollback();

	t.set_eos();

	RESTORE_CURRENT_PP;
}

PPIterator * PPFnSQLRollback::copy(dynamic_context *_cxt_)
{
	PPFnSQLRollback *res;
	res = se_new PPFnSQLRollback(_cxt_, connection);

	res->connection.op = connection.op->copy(_cxt_);

	return res;
}


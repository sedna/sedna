/*
 * File:  PPSQL.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPSQL.h"
#include "tr/executor/xqops/PPSQLODBC.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/visitor/PPVisitor.h"

#include "tr/structures/nodeutils.h"
#include "tr/executor/base/SCElementProducer.h"

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
        xmlns_ptr ns = getSchemaNode(node)->get_xmlns();

        if (!ns)
            return false;
        if (!ns->uri)
            return false;
        if (strcmp(ns->uri, sqlns_uri))
            return false;

        char *elem_name = getSchemaNode(node)->name;
        if (strcmp(elem_name, "option"))
            return false;


        opt_name = NULL;
        opt_value = NULL;
        //FIXME - CHECKP(node)???
        node = getFirstAttributeChild(node);
        while (node != XNULL)
        {
            char *atr_name;
            CHECKP(node);
            atr_name = getSchemaNode(node)->name;
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

            node = getNextAttribute(node);
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

int SQLHandleManager::new_connection(const char *connect_str, int connect_str_len,
                                        const char *uid, int uid_len,
                                        const char *pass, int pass_len, PPOpIn *options)
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

SQLHandle*  SQLHandleManager::get_handle(int h)
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

PPFnSQLBase::PPFnSQLBase(dynamic_context *_cxt_,
                         operation_info _info_) : PPConstructor(_cxt_,_info_,false)
{
    handle_manager_carrier = false;
}

///////////////////////////////////////////////////////////////////////////////
/// PPFnSQLConnect
///////////////////////////////////////////////////////////////////////////////
PPFnSQLConnect::PPFnSQLConnect(dynamic_context *_cxt_,
                               operation_info _info_,
                               const arr_of_PPOpIn &_arr_) : PPFnSQLBase(_cxt_, _info_),
                                                             arr(_arr_)
{
}

PPFnSQLConnect::~PPFnSQLConnect()
{
    for (arr_of_PPOpIn::size_type i = 0; i < arr.size(); i++)
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


void PPFnSQLConnect::do_open ()
{
    handle_manager_carrier = checkBaseInitial();
    for (unsigned int i = 0; i < arr.size(); i++)
        arr[i].op->open();

    first_time = true;
}

void PPFnSQLConnect::do_reopen()
{
    for (unsigned int i = 0; i < arr.size(); i++)
        arr[i].op->reopen();

    first_time = true;
}

void PPFnSQLConnect::do_close()
{
    for (unsigned int i = 0; i < arr.size(); i++)
        arr[i].op->close();
}

//copied from PPConstructors.cpp
static const char *getStringParameter(PPOpIn content, const char *err_pref)
{
    tuple value(content.ts);
    content.op->next(value);
    sequence at_vals(1);
    if (value.is_eos())
    {
		throw USER_EXCEPTION2(XPTY0004, (std::string(err_pref) + std::string(". Argument contains zero items.")).c_str());
    }
    while (!(value.is_eos()))
    {
        if (!(value.cells_number==1 )) throw USER_EXCEPTION2(SE1003, "in PPSQL");
        at_vals.add(value);
        content.op->next(value);
    }
    executor_globals::tmp_op_str_buf.clear();
    sequence::iterator it=at_vals.begin();
    do
    {
        tuple_cell res=atomize((*it).cells[0]);
        res=cast(res, xs_string);
        res=tuple_cell::make_sure_light_atomic(res);
        if (it!=at_vals.begin())
        {
			executor_globals::tmp_op_str_buf.append(" ");
        }
        executor_globals::tmp_op_str_buf.append(res);
        it++;
    }
    while (it!=at_vals.end());
    //str_val.push_to_memory();
    return executor_globals::tmp_op_str_buf.c_str();
}

void PPFnSQLConnect::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;
        const char *tmp;

        char    *connect_str    = NULL;
        int     connect_str_len = 0;
        char    *uid            = NULL;
        int     uid_len         = 0;
        const char *pass        = NULL;
        int     pass_len        = 0;

		tmp = getStringParameter(arr[0], "Bad 1st argument of sql:connect");
        connect_str_len = strlen(tmp);
        connect_str = se_new char[connect_str_len+1];
        strncpy(connect_str, tmp, connect_str_len);
        connect_str[connect_str_len] = 0;


        if (arr.size() >= 2)
        {
			tmp = getStringParameter(arr[1], "Bad 2nd argument of sql:connect");
            uid_len = strlen(tmp);
            uid = se_new char[uid_len+1];
            strncpy(uid, tmp, uid_len);
            uid[uid_len] = 0;

            if (arr.size() >= 3)
            {
				tmp = getStringParameter(arr[2], "Bad 3rd argument of sql:connect");
                pass        = tmp;
                pass_len    = strlen(tmp);
            }
        }
        PPOpIn *options = NULL;
        if (arr.size() >= 4)
            options = &arr[3];
        int h = sql_handle_manager->new_connection(connect_str, connect_str_len,
            uid, uid_len, pass, pass_len, options);
        t.copy(tuple_cell::atomic((int64_t)h));

        delete connect_str;
        if (uid != NULL)
            delete uid;
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnSQLConnect::do_copy(dynamic_context *_cxt_)
{
    PPFnSQLConnect *res = se_new PPFnSQLConnect(_cxt_, info, arr);

    for (arr_of_PPOpIn::size_type it = 0; it < arr.size(); it++)
        res->arr[it].op = arr[it].op->copy(_cxt_);

    return res;
}

void PPFnSQLConnect::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (arr_of_PPOpIn::size_type it = 0; it < arr.size(); it++)
        arr[it].op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
/// PPFnSQLExecute
///////////////////////////////////////////////////////////////////////////////
PPFnSQLExecute::PPFnSQLExecute(dynamic_context *_cxt_,
                               operation_info _info_,
                               const arr_of_PPOpIn &_arr_,
                               bool _exec_update_) : PPFnSQLBase(_cxt_, _info_),
                                                     arr(_arr_),
                                                     handle(NULL),
                                                     executor(NULL),
                                                     exec_update(_exec_update_)
{
}

PPFnSQLExecute::~PPFnSQLExecute()
{
    for (arr_of_PPOpIn::size_type i = 0; i < arr.size(); i++)
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


void PPFnSQLExecute::do_open ()
{
    handle_manager_carrier = checkBaseInitial();
    checkInitial();

    for (unsigned int i = 0; i < arr.size(); i++)
    {
        arr[i].op->open();
    }

    release_executor();
    first_time = true;
}

void PPFnSQLExecute::do_reopen()
{
    for (unsigned int i = 0; i < arr.size(); i++)
        arr[i].op->reopen();

    release_executor();
    first_time = true;
}

void PPFnSQLExecute::do_close()
{
    for (unsigned int i = 0; i < arr.size(); i++)
        arr[i].op->close();
}

void PPFnSQLExecute::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;
        tuple tmp(1);
        tuple_cell tmp_cell;

        arr[0].op->next(tmp);

        if (tmp.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "Empty $connection argument of sql:execute");

        handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

        arr[0].op->next(tmp);
        if (!tmp.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "Singleton sequence is expected in $connection argument of sql:execute");

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
                    return;
                }

                {
					const char *query	= getStringParameter(arr[1], "Bad 2nd argument of sql:execute");
                    int query_len = strlen(query);

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
            t.copy(tuple_cell::atomic((int64_t)executor->update_row_count()));
            return;
        }
    }

    if (exec_update)
    {
        t.set_eos();
        first_time = true;
    }
    else
    {
        executor->fetch(t, SCElementProducer::getVirtualRoot(XNULL));

        if (t.is_eos()) {
            first_time = true;
        }
    }
}

PPIterator* PPFnSQLExecute::do_copy(dynamic_context *_cxt_)
{
    PPFnSQLExecute *res = se_new PPFnSQLExecute(_cxt_, info, arr, exec_update);

    for (arr_of_PPOpIn::size_type it = 0; it < arr.size(); it++)
        res->arr[it].op = arr[it].op->copy(_cxt_);

    return res;
}

void PPFnSQLExecute::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (arr_of_PPOpIn::size_type it = 0; it < arr.size(); it++)
        arr[it].op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
/// PPFnSQLPrepare
///////////////////////////////////////////////////////////////////////////////
PPFnSQLPrepare::PPFnSQLPrepare(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _connection_,
                               PPOpIn _statement_,
                               PPOpIn _options_) : PPFnSQLBase(_cxt_, _info_),
                                                   has_options(true),
                                                   connection(_connection_),
                                                   statement(_statement_),
                                                   options(_options_)
{
}
PPFnSQLPrepare::PPFnSQLPrepare(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _connection_,
                               PPOpIn _statement_) : PPFnSQLBase(_cxt_, _info_),
                                                     has_options(false),
                                                     connection(_connection_),
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
void PPFnSQLPrepare::do_open ()
{
    handle_manager_carrier = checkBaseInitial();
    connection.op->open();
    statement.op->open();
    if (has_options)
        options.op->open();

    first_time = true;
}
void PPFnSQLPrepare::do_reopen()
{
    connection.op->reopen();
    statement.op->reopen();
    if (has_options)
        options.op->reopen();

    first_time = true;
}


void PPFnSQLPrepare::do_close()
{
    connection.op->close();
    statement.op->close();
    if (has_options)
        options.op->close();
}
void PPFnSQLPrepare::do_next(tuple &t)
{
    if (first_time)
    {
        SQLHandle *handle;
        tuple tmp(1);
        //tuple_cell tmp_cell;

        first_time = false;

        connection.op->next(tmp);

        if (tmp.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "Empty $connection argument of sql:prepare");

        handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

        connection.op->next(tmp);
        if (!tmp.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "Singleton sequence is expected in $connection argument of sql:prepare");

        if (handle == NULL)
            throw XQUERY_EXCEPTION(SE2101);

        if (handle->type != SQLH_CONNECTION)
            throw XQUERY_EXCEPTION(SE2104);

        const char *query = NULL;
        int query_len = 0;
		query		= getStringParameter(statement, "Bad $statement argument of sql:prepare");
        query_len   = strlen(query);

        SQLHandle *stmt;
        if (has_options)
            stmt = handle->prepare_stmt(query, query_len, &options);
        else
            stmt = handle->prepare_stmt(query, query_len, NULL);

        t.copy(tuple_cell::atomic((int64_t)sql_handle_manager->new_handle(stmt)));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}
PPIterator * PPFnSQLPrepare::do_copy(dynamic_context *_cxt_)
{
    PPFnSQLPrepare *res;
    if (has_options)
        res = se_new PPFnSQLPrepare(_cxt_, info, connection, statement, options);
    else
        res = se_new PPFnSQLPrepare(_cxt_, info, connection, statement);

    res->connection.op = connection.op->copy(_cxt_);
    res->statement.op = statement.op->copy(_cxt_);
    if (has_options)
        res->options.op = options.op->copy(_cxt_);

    return res;
}

void PPFnSQLPrepare::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    connection.op->accept(v);
    statement.op->accept(v);
    if (has_options)
        options.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
/// PPFnSQLClose
///////////////////////////////////////////////////////////////////////////////

PPFnSQLClose::PPFnSQLClose(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _connection_) : PPFnSQLBase(_cxt_, _info_),
                                                  connection(_connection_)
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
void PPFnSQLClose::do_open ()
{
    handle_manager_carrier = checkBaseInitial();
    connection.op->open();
}
void PPFnSQLClose::do_reopen()
{
    connection.op->reopen();
}
void PPFnSQLClose::do_close()
{
    connection.op->close();
}
void PPFnSQLClose::do_next(tuple &t)
{
    int handle_id;
    SQLHandle *handle;
    tuple tmp(1);
    tuple_cell tmp_cell;

    connection.op->next(tmp);

    if (tmp.is_eos())
		throw XQUERY_EXCEPTION2(XPTY0004, "Empty $connection argument of sql:close");

    handle_id = tmp.cells[0].get_xs_integer();
    handle = sql_handle_manager->get_handle(handle_id);

    connection.op->next(tmp);
    if (!tmp.is_eos())
		throw XQUERY_EXCEPTION2(XPTY0004, "Singleton sequence is expected in $connection argument of sql:close");

    if (handle == NULL)
        throw XQUERY_EXCEPTION(SE2101);

    if (handle->type != SQLH_CONNECTION)
        throw XQUERY_EXCEPTION(SE2104);

    handle->close();
    //TODO - free handle? would require either releasing all executors
    //or delaying connecton object destruction

    t.set_eos();
}

PPIterator * PPFnSQLClose::do_copy(dynamic_context *_cxt_)
{
    PPFnSQLClose *res;
    res = se_new PPFnSQLClose(_cxt_, info, connection);

    res->connection.op = connection.op->copy(_cxt_);

    return res;
}

void PPFnSQLClose::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    connection.op->accept(v);
    v.pop();
}

////////////////////////////////////////////////////////////////////////////////
/// PPFnSQLCommit
////////////////////////////////////////////////////////////////////////////////

PPFnSQLCommit::PPFnSQLCommit(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _connection_) : PPFnSQLBase(_cxt_, _info_),
                                                    connection(_connection_)
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
void PPFnSQLCommit::do_open ()
{
    handle_manager_carrier = checkBaseInitial();
    connection.op->open();
}
void PPFnSQLCommit::do_reopen()
{
    connection.op->reopen();
}
void PPFnSQLCommit::do_close()
{
    connection.op->close();
}
void PPFnSQLCommit::do_next(tuple &t)
{
    SQLHandle *handle;
    tuple tmp(1);
    tuple_cell tmp_cell;

    connection.op->next(tmp);

    if (tmp.is_eos())
		throw XQUERY_EXCEPTION2(XPTY0004, "Empty $connection argument of sql:commit");

    handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

    connection.op->next(tmp);
    if (!tmp.is_eos())
		throw XQUERY_EXCEPTION2(XPTY0004, "Singleton sequence is expected in $connection argument of sql:commit");

    if (handle == NULL)
        throw XQUERY_EXCEPTION(SE2101);

    if (handle->type != SQLH_CONNECTION)
        throw XQUERY_EXCEPTION(SE2104);

    handle->commit();

    t.set_eos();
}

PPIterator * PPFnSQLCommit::do_copy(dynamic_context *_cxt_)
{
    PPFnSQLCommit *res;
    res = se_new PPFnSQLCommit(_cxt_, info, connection);

    res->connection.op = connection.op->copy(_cxt_);

    return res;
}

void PPFnSQLCommit::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    connection.op->accept(v);
    v.pop();
}

////////////////////////////////////////////////////////////////////////////////
/// PPFnSQLRollback
////////////////////////////////////////////////////////////////////////////////

PPFnSQLRollback::PPFnSQLRollback(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _connection_) : PPFnSQLBase(_cxt_, _info_),
                                                        connection(_connection_)
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
void PPFnSQLRollback::do_open ()
{
    handle_manager_carrier = checkBaseInitial();
    connection.op->open();
}
void PPFnSQLRollback::do_reopen()
{
    connection.op->reopen();
}
void PPFnSQLRollback::do_close()
{
    connection.op->close();
}
void PPFnSQLRollback::do_next(tuple &t)
{
    SQLHandle *handle;
    tuple tmp(1);
    tuple_cell tmp_cell;

    connection.op->next(tmp);

    if (tmp.is_eos())
		throw XQUERY_EXCEPTION2(XPTY0004, "Empty $connection argument of sql:rollback");

    handle = sql_handle_manager->get_handle(tmp.cells[0].get_xs_integer());

    connection.op->next(tmp);
    if (!tmp.is_eos())
		throw XQUERY_EXCEPTION2(XPTY0004, "Singleton sequence is expected in $connection argument of sql:rollback");

    if (handle == NULL)
        throw XQUERY_EXCEPTION(SE2101);

    if (handle->type != SQLH_CONNECTION)
        throw XQUERY_EXCEPTION(XPTY0004);

    handle->rollback();

    t.set_eos();
}

PPIterator* PPFnSQLRollback::do_copy(dynamic_context *_cxt_)
{
    PPFnSQLRollback *res;
    res = se_new PPFnSQLRollback(_cxt_, info, connection);
    res->connection.op = connection.op->copy(_cxt_);
    return res;
}

void PPFnSQLRollback::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    connection.op->accept(v);
    v.pop();
}

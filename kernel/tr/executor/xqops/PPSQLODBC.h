/*
 * File:  PPSQLODBC.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSQLODBC_H
#define __PPSQLODBC_H

#define DEBUG_ODBC	1

#include "common/sedna.h"

#include <sql.h>
#include <sqltypes.h>
#include <sqlext.h>

#include "tr/executor/xqops/PPSQL.h"
#include "tr/executor/xqops/PPConstructors.h"
#include "tr/structures/producer.h"

//FIXME??
#ifndef SQL_API

#define SQL_API

#endif


class SQLODBCBase
{
	static bool loaded;
public:
	typedef SQLRETURN  (SQL_API * tSQLGetDiagRec)(SQLSMALLINT HandleType, SQLHANDLE Handle,
           SQLSMALLINT RecNumber, SQLCHAR *Sqlstate,
           SQLINTEGER *NativeError, SQLCHAR *MessageText,
           SQLSMALLINT BufferLength, SQLSMALLINT *TextLength);

	typedef SQLRETURN  (SQL_API * tSQLAllocHandle)(SQLSMALLINT HandleType,
           SQLHANDLE InputHandle, SQLHANDLE *OutputHandle);

	typedef SQLRETURN  (SQL_API * tSQLFreeHandle)(SQLSMALLINT HandleType, SQLHANDLE Handle);

	typedef SQLRETURN  (SQL_API * tSQLSetEnvAttr)(SQLHENV EnvironmentHandle,
           SQLINTEGER Attribute, SQLPOINTER Value,
           SQLINTEGER StringLength);

	typedef SQLRETURN (SQL_API * tSQLDriverConnect)(
				SQLHDBC            hdbc,
				SQLHWND            hwnd,
				SQLCHAR 		  *szConnStrIn,
				SQLSMALLINT        cbConnStrIn,
				SQLCHAR           *szConnStrOut,
				SQLSMALLINT        cbConnStrOutMax,
				SQLSMALLINT 	  *pcbConnStrOut,
				SQLUSMALLINT       fDriverCompletion);

	typedef SQLRETURN  (SQL_API * tSQLExecDirect)(SQLHSTMT StatementHandle,
           SQLCHAR *StatementText, SQLINTEGER TextLength);

	typedef SQLRETURN  (SQL_API * tSQLFetch)(SQLHSTMT StatementHandle);

	typedef SQLRETURN  (SQL_API * tSQLNumResultCols)(SQLHSTMT StatementHandle,
           SQLSMALLINT *ColumnCount);

	typedef SQLRETURN  (SQL_API * tSQLDescribeCol)(SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, SQLCHAR *ColumnName,
           SQLSMALLINT BufferLength, SQLSMALLINT *NameLength,
           SQLSMALLINT *DataType, SQLULEN *ColumnSize,
           SQLSMALLINT *DecimalDigits, SQLSMALLINT *Nullable);

	typedef SQLRETURN  (SQL_API * tSQLBindCol)(SQLHSTMT StatementHandle, 
		   SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType, 
		   SQLPOINTER TargetValue, SQLLEN BufferLength, 
	   	   SQLLEN *StrLen_or_Ind);

	typedef SQLRETURN  (SQL_API * tSQLGetData)(SQLHSTMT StatementHandle,
           SQLUSMALLINT ColumnNumber, SQLSMALLINT TargetType,
           SQLPOINTER TargetValue, SQLLEN BufferLength,
           SQLLEN *StrLen_or_Ind);

	typedef SQLRETURN  (SQL_API * tSQLCloseCursor)(SQLHSTMT StatementHandle);

	typedef SQLRETURN (SQL_API * tSQLBindParameter)(
	    SQLHSTMT           hstmt,
	    SQLUSMALLINT       ipar,
	    SQLSMALLINT        fParamType,
	    SQLSMALLINT        fCType,
	    SQLSMALLINT        fSqlType,
	    SQLULEN            cbColDef,
	    SQLSMALLINT        ibScale,
	    SQLPOINTER         rgbValue,
	    SQLLEN             cbValueMax,
	    SQLLEN     		   *pcbValue);

	typedef SQLRETURN (SQL_API * tSQLNumParams)(
		SQLHSTMT           hstmt,
		SQLSMALLINT 	  *pcpar);

	typedef SQLRETURN  (SQL_API * tSQLParamData)(SQLHSTMT StatementHandle,
           SQLPOINTER *Value);

	typedef SQLRETURN  (SQL_API * tSQLPrepare)(SQLHSTMT StatementHandle,
           SQLCHAR *StatementText, SQLINTEGER TextLength);

	typedef SQLRETURN  (SQL_API * tSQLPutData)(SQLHSTMT StatementHandle,
           SQLPOINTER Data, SQLLEN StrLen_or_Ind);

	typedef SQLRETURN  (SQL_API * tSQLExecute)(SQLHSTMT StatementHandle);

	typedef SQLRETURN  (SQL_API * tSQLDisconnect)(SQLHDBC ConnectionHandle);
	typedef SQLRETURN  (SQL_API * tSQLEndTran)(SQLSMALLINT HandleType, SQLHANDLE Handle,
           SQLSMALLINT CompletionType);

	typedef SQLRETURN  (SQL_API * tSQLRowCount)(SQLHSTMT StatementHandle, 
	   SQLLEN* RowCount);
	typedef SQLRETURN  (SQL_API * tSQLSetConnectAttr)(SQLHDBC ConnectionHandle,
           SQLINTEGER Attribute, SQLPOINTER Value,
           SQLINTEGER StringLength);

	static tSQLGetDiagRec		fSQLGetDiagRec;
	static tSQLAllocHandle		fSQLAllocHandle;
	static tSQLFreeHandle		fSQLFreeHandle;
	static tSQLSetEnvAttr		fSQLSetEnvAttr;
	static tSQLDriverConnect	fSQLDriverConnect;
	static tSQLFetch			fSQLFetch;
	static tSQLExecDirect		fSQLExecDirect;
	static tSQLNumResultCols	fSQLNumResultCols;
	static tSQLDescribeCol		fSQLDescribeCol;
	static tSQLBindCol			fSQLBindCol;
	static tSQLGetData			fSQLGetData;
	static tSQLCloseCursor		fSQLCloseCursor;
	static tSQLBindParameter	fSQLBindParameter;
	static tSQLNumParams		fSQLNumParams;
	static tSQLParamData		fSQLParamData;
	static tSQLPrepare			fSQLPrepare;
	static tSQLPutData			fSQLPutData;
	static tSQLExecute			fSQLExecute;
	static tSQLDisconnect		fSQLDisconnect;
	static tSQLEndTran			fSQLEndTran;
	static tSQLRowCount			fSQLRowCount;
	static tSQLSetConnectAttr	fSQLSetConnectAttr;


	static void load();
};

struct SQLODBCResult
{
	char		col_name[64];
	SQLSMALLINT	col_name_len;
};

class SQLODBCExecutor : public SQLExecutor
{
private:
	SQLHDBC hdbc;
	SQLHSTMT hstmt;

	bool active;
	SQLODBCResult *results;
	int	results_count;

	static char *res_buf;
	static int	res_buf_size;

	SQLLEN param_ind;
	std::vector<SQLSMALLINT> param_types;
	inline void set_param_type(int i, SQLSMALLINT vtype, SQLSMALLINT ptype, SQLUINTEGER colsize);

	void prepare_results();
public:
	SQLODBCExecutor(SQLHDBC _hdbc_, SQLHSTMT _hstmt_);

	static SQLODBCExecutor* create(SQLHDBC hdbc);
	void prepare(const char *query, int query_len, PPOpIn *options);

	virtual void execute_query (const char *query, int query_len, PPOpIn *options);
	virtual void execute_prepared(arr_of_PPOpIn params);
	virtual void close_query();
	virtual void fetch(xqp_tuple &t, IElementProducer * producer);
	virtual int  update_row_count();

	virtual bool is_query_active();
	//TODO destructor + add virual to parent (free hstmt in desctructor too!), del res_buf
};

class SQLODBCConnection : public SQLConnection
{
	SQLHDBC hdbc;
	bool	open;

	std::vector<SQLHandle *> prepared_stmts;
public:
	SQLODBCConnection(SQLHDBC _hdbc_);

	virtual SQLExecutor*	get_executor();
	virtual void			release_executor(SQLExecutor *executor);
	virtual SQLHandle*		prepare_stmt(const char *query, int query_len, PPOpIn *options);

	virtual void			close();
	virtual void			commit();
	virtual void			rollback();

	virtual ~SQLODBCConnection();
};

class SQLODBCPreparedStmt : public SQLHandle
{
	SQLODBCConnection	*connection;
	SQLODBCExecutor		*executor;
public:
	SQLODBCPreparedStmt(SQLODBCConnection *_connection_, SQLODBCExecutor *_executor_);

	virtual SQLExecutor*	get_executor();
	virtual void			release_executor(SQLExecutor *executor);
	virtual SQLHandle*		prepare_stmt(const char *query, int query_len, PPOpIn *options);

	virtual void			close();
	virtual void			commit();
	virtual void			rollback();

	virtual ~SQLODBCPreparedStmt();
};

class SQLODBCDriver : public SQLDriver
{
private:
	SQLHENV	henv;

	bool	alloc_henv();

	char	connection_str_buf[1024];
	char	*connection_str_end;

	void	append_connection_str(const char *opt, const char *val, int val_len, bool braces = false);
public:
	SQLODBCDriver();

	virtual const char*     prefix() const { return "odbc:"; }
	virtual int             prefix_len() const { return 5; }
	virtual SQLConnection*	new_connection(const char *connect_str, int connect_str_len,
											  const char *uid, int uid_len,
											  const char *pass, int pass_len, PPOpIn *options);

	virtual ~SQLODBCDriver();
};


#endif

/*
 * File:  PPSQLODBC.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/mo/mo.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "common/errdbg/d_printf.h"
#include "tr/strings/e_string.h"
#include "tr/executor/xqops/PPSQLODBC.h"

#ifndef WIN32
#include <dlfcn.h>
#include <sys/mman.h>
#endif

#include "tr/executor/base/SCElementProducer.h"

//#define LOCK_MEM

#ifdef LOCK_MEM

//#define DEBUG_MEM_LOCK
#ifdef DEBUG_MEM_LOCK

#define d_printf1_ml d_printf1
#define d_printf2_ml d_printf2
#define d_printf3_ml d_printf3
#define d_printf4_ml d_printf4

#else

#define d_printf1_ml(x1)
#define d_printf2_ml(x1,x2)
#define d_printf3_ml(x1,x2,x3)
#define d_printf4_ml(x1,x2,x3,x4)

#endif

#ifdef WIN32

static const SIZE_T	lock_mem_spot_size = 0x10000;

#else /* UNIX */

static const size_t	lock_mem_spot_size = 0x10000;

#endif
static const int lock_mem_start_block = 8*1024;

static int lock_mem_spots_count;

typedef std::vector<bool> lock_mem_result_t;

static lock_mem_result_t *lock_mem()
{
	lock_mem_result_t *r = se_new lock_mem_result_t(lock_mem_spots_count);

	if (r != NULL)
	{
		unsigned int spot = (unsigned int)LAYER_ADDRESS_SPACE_START_ADDR;
		unsigned int lock_end = spot + LAYER_ADDRESS_SPACE_SIZE;
		int spot_cnt = lock_mem_start_block;
#ifdef WIN32
		SIZE_T spot_size = lock_mem_spot_size * lock_mem_start_block;
#else
		size_t spot_size = lock_mem_spot_size * lock_mem_start_block;
#endif
		int i;

		for (i = 0; i < lock_mem_spots_count; i++)
			(*r)[i] = false;

		i = 0;


		d_printf3_ml("lock_mem() started, spot=%x, lock_end=%x\n", spot, lock_end);

		while (spot < lock_end)
		{
#ifdef WIN32
			LPVOID addr = NULL;
			if (spot+spot_size <= lock_end)
				addr = VirtualAlloc((LPVOID)spot, spot_size, MEM_RESERVE, PAGE_READWRITE);
			if (addr == (LPVOID)spot)
			{
				(*r)[i] = true;
				spot += spot_size;
				i += spot_cnt;
				while ((i & ((spot_cnt<<1)-1)) == 0 && spot_cnt < lock_mem_start_block)
				{
					spot_cnt = spot_cnt << 1;
					spot_size = spot_size << 1;
				}
			}
#else
			void *addr = MAP_FAILED;
			if (spot+spot_size <= lock_end)
				addr = mmap((void *)spot, spot_size, PROT_NONE, MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
			if (addr == (void *)spot)
			{
				d_printf4_ml("mmapped block at 0x%x, size=0x%x, cnt=%d\n", spot, spot_size, spot_cnt);
				int new_i = i + spot_cnt;
				while (i < new_i)
					(*r)[i++] = true;
				spot += spot_size;
				while ((i & ((spot_cnt<<1)-1)) == 0 && spot_cnt < lock_mem_start_block)
				{
					spot_cnt = spot_cnt << 1;
					spot_size = spot_size << 1;
				}
			}
#endif
			else
			{
#ifdef WIN32
				if (addr != NULL)
					throw SYSTEM_EXCEPTION("VirtualAlloc returned unexpected result");
#else
				if (addr != MAP_FAILED)
					throw SYSTEM_EXCEPTION("mmap returned unexpected result");
#endif
				if (spot_cnt > 1)
				{
					spot_size = spot_size >> 1;
					spot_cnt = spot_cnt >> 1;
				}
				else
				{
					spot += spot_size;
					i += spot_cnt;
					while ((i & ((spot_cnt<<1)-1)) == 0 && spot_cnt < lock_mem_start_block)
					{
						spot_cnt = spot_cnt << 1;
						spot_size = spot_size << 1;
					}
				}
			}

		}
	}

	d_printf1_ml("lock_mem() finished\n");
	return r;
}

static void unlock_mem(lock_mem_result_t *r)
{
	d_printf2_ml("unlock_mem(%p) started\n", r);
	if (r == NULL)
		return;

	unsigned int spot = (unsigned int)LAYER_ADDRESS_SPACE_START_ADDR;
	unsigned int lock_end = spot + LAYER_ADDRESS_SPACE_SIZE;
#ifdef WIN32
	for (int i = 0; spot < lock_end; spot += lock_mem_spot_size, i++)
		if ((*r)[i])
			VirtualFree((LPVOID)spot, 0, MEM_RELEASE);
#else
	int i = 0;
	size_t max_spot_size = lock_mem_spot_size * lock_mem_start_block;
	while (spot < lock_end)
	{
		if ((*r)[i])
		{
			void *spot_start = (void *)spot;
			size_t spot_len = 0;
			while (spot < lock_end && spot_len < max_spot_size && (*r)[i])
			{
				spot_len += lock_mem_spot_size;
				spot += lock_mem_spot_size;
				i++;
			}
			d_printf3_ml("munmap(%p, 0x%x)\n", spot_start, spot_len);
			munmap(spot_start, spot_len);
		}
		else
		{
			i++;
			spot += lock_mem_spot_size;
		}
	}
#endif

	delete r;
	d_printf1_ml("unlock_mem() finished\n");
}

#else

#define unlock_mem(x)

#endif //LOCK_MEM

static inline bool result_ok(SQLRETURN rc)
{
	return rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO;
}

static std::string getODBCDiag(SQLSMALLINT htype, SQLHANDLE handle)
{
	std::string str("");
	int i=0,rc;
	static SQLCHAR state[16], msgbuf[1024];
	SQLSMALLINT msglen = 0;
	SQLINTEGER NativeError;

	while (true) {
		rc = SQLODBCBase::fSQLGetDiagRec(htype, handle, ++i, state,
				&NativeError, msgbuf, sizeof(msgbuf)-1, &msglen);

		switch (rc) {
			case SQL_NO_DATA:
				return str;
			case SQL_SUCCESS:
			case SQL_SUCCESS_WITH_INFO:
				if (i > 1)
					str += ", ";
				msgbuf[msglen] = 0;
				str += (char *)msgbuf;
				break;
			default:
				if (i > 1)
					str += ", Failed to get ODBC diagnostic info";
				else
					str += "Failed to get ODBC diagnostic info";
				return str;
		}
	}
}


bool SQLODBCBase::loaded = false;

SQLODBCBase::tSQLGetDiagRec		SQLODBCBase::fSQLGetDiagRec = NULL;
SQLODBCBase::tSQLAllocHandle	SQLODBCBase::fSQLAllocHandle = NULL;
SQLODBCBase::tSQLFreeHandle		SQLODBCBase::fSQLFreeHandle = NULL;
SQLODBCBase::tSQLSetEnvAttr		SQLODBCBase::fSQLSetEnvAttr = NULL;
SQLODBCBase::tSQLDriverConnect	SQLODBCBase::fSQLDriverConnect = NULL;
SQLODBCBase::tSQLExecDirect		SQLODBCBase::fSQLExecDirect = NULL;
SQLODBCBase::tSQLFetch			SQLODBCBase::fSQLFetch = NULL;
SQLODBCBase::tSQLNumResultCols	SQLODBCBase::fSQLNumResultCols = NULL;
SQLODBCBase::tSQLDescribeCol	SQLODBCBase::fSQLDescribeCol = NULL;
SQLODBCBase::tSQLBindCol		SQLODBCBase::fSQLBindCol = NULL;
SQLODBCBase::tSQLGetData		SQLODBCBase::fSQLGetData = NULL;
SQLODBCBase::tSQLCloseCursor	SQLODBCBase::fSQLCloseCursor = NULL;
SQLODBCBase::tSQLBindParameter	SQLODBCBase::fSQLBindParameter = NULL;
SQLODBCBase::tSQLNumParams		SQLODBCBase::fSQLNumParams = NULL;
SQLODBCBase::tSQLParamData		SQLODBCBase::fSQLParamData = NULL;
SQLODBCBase::tSQLPrepare		SQLODBCBase::fSQLPrepare = NULL;
SQLODBCBase::tSQLPutData		SQLODBCBase::fSQLPutData = NULL;
SQLODBCBase::tSQLExecute		SQLODBCBase::fSQLExecute = NULL;
SQLODBCBase::tSQLDisconnect		SQLODBCBase::fSQLDisconnect = NULL;
SQLODBCBase::tSQLEndTran		SQLODBCBase::fSQLEndTran = NULL;
SQLODBCBase::tSQLRowCount		SQLODBCBase::fSQLRowCount = NULL;
SQLODBCBase::tSQLSetConnectAttr	SQLODBCBase::fSQLSetConnectAttr = NULL;


/*
void trymmap()
{
	void *addr = mmap((void*)0x60100000, 0x10000, PROT_READ, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
//	void *addr = mmap((void*)0x0, 0x10000, PROT_READ, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	d_printf2_ml("trymmap: addr = %p\n", addr);
	if (addr != MAP_FAILED)
		munmap(addr, 0x10000);
}
*/

void SQLODBCBase::load()
{
	if (loaded)
		return;

#ifdef LOCK_MEM
	lock_mem_spots_count = LAYER_ADDRESS_SPACE_SIZE / lock_mem_spot_size /*+1*/;
#endif

	//FIXME - change SE2111 to smth else

	{
#ifdef LIBODBC_STATIC
#define LOAD(x) f##x = (t##x) x;
#else
#ifdef LOCK_MEM
		lock_mem_result_t *lmr = lock_mem();
#endif
#ifdef WIN32
		HINSTANCE hLib = LoadLibrary("odbc32.dll");
		unlock_mem(lmr);

		if (hLib == NULL)
			throw USER_EXCEPTION2(SE2111, "failed to load odbc32.dll");
#define LOAD(x)	f##x = (t##x) GetProcAddress((HMODULE)hLib, #x); \
				if (f##x == NULL) throw USER_EXCEPTION2(SE2111, "failed to load function " #x);
#else
		void *hLib = dlopen("libodbc.so", RTLD_LAZY);
		unlock_mem(lmr);

		if (hLib == NULL)
			throw USER_EXCEPTION2(SE2111, "failed to load libodbc.so");
#define LOAD(x)	f##x = (t##x) dlsym(hLib, #x); \
				if (f##x == NULL) throw USER_EXCEPTION2(SE2111, "failed to load function " #x);
#endif
#endif //LIBODBC_STATIC
		LOAD(SQLGetDiagRec);
		LOAD(SQLAllocHandle);
		LOAD(SQLFreeHandle);
		LOAD(SQLSetEnvAttr);
		LOAD(SQLDriverConnect);
		LOAD(SQLExecDirect);
		LOAD(SQLFetch);
		LOAD(SQLNumResultCols);
		LOAD(SQLDescribeCol);
		LOAD(SQLBindCol);
		LOAD(SQLGetData);
		LOAD(SQLCloseCursor);
		LOAD(SQLBindParameter);
		LOAD(SQLNumParams);
		LOAD(SQLParamData);
		LOAD(SQLPrepare);
		LOAD(SQLPutData);
		LOAD(SQLExecute);
		LOAD(SQLDisconnect);
		LOAD(SQLEndTran);
		LOAD(SQLRowCount);
		LOAD(SQLSetConnectAttr);
#undef LOAD
	}

	loaded = true;
}

char *SQLODBCExecutor::res_buf = NULL;
int	SQLODBCExecutor::res_buf_size = 0;

SQLODBCExecutor::SQLODBCExecutor(SQLHDBC _hdbc_, SQLHSTMT _hstmt_) : hdbc(_hdbc_),
										hstmt(_hstmt_), active(false), results(NULL), results_count(0)
{
}

SQLODBCExecutor* SQLODBCExecutor::create(SQLHDBC hdbc)
{
	SQLHSTMT hstmt;
	SQLRETURN rc;

	rc = SQLODBCBase::fSQLAllocHandle(SQL_HANDLE_STMT, hdbc, &hstmt);
	if (!result_ok(rc))
		return NULL;

	return se_new SQLODBCExecutor(hdbc, hstmt);
}

inline void SQLODBCExecutor::set_param_type(int i, SQLSMALLINT vtype, SQLSMALLINT ptype, SQLUINTEGER colsize)
{
	if (param_types[i-1] != vtype || colsize > 128)
	{
		SQLRETURN rc;

		rc = SQLODBCBase::fSQLBindParameter(hstmt, i, SQL_PARAM_INPUT,
			vtype, ptype,
			colsize, 0, (SQLPOINTER)i,
			0, &param_ind);

		if (!result_ok(rc)) {
			std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
			throw XQUERY_EXCEPTION2(SE2106, ("BindParameter: " + diag).c_str());
		}
		param_types[i-1] = vtype;
	}
}

void SQLODBCExecutor::prepare(const char *query, int query_len, PPOpIn *options)
{
	SQLRETURN rc;
	SQLSMALLINT cnt;

	rc = SQLODBCBase::fSQLPrepare(hstmt, (SQLCHAR *)query, query_len);
	if ( !result_ok(rc)) {
		std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
		throw XQUERY_EXCEPTION2(SE2105, diag.c_str());
	}

	rc = SQLODBCBase::fSQLNumParams(hstmt, &cnt);
	if (!result_ok(rc)) {
		std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
		throw XQUERY_EXCEPTION2(SE2105, diag.c_str());
	}

	param_ind = SQL_LEN_DATA_AT_EXEC(0);

	param_types = std::vector<SQLSMALLINT>(cnt);

	for (int i = 1; i<=cnt; i++) {
		//XXX - changle SQL_NULL_DATA to smth better
		param_types[i-1] = SQL_NULL_DATA;
	}
}

void SQLODBCExecutor::prepare_results()
{
	SQLRETURN rc;
	SQLSMALLINT num_cols;
	//TODO - get info
	active = true;

	rc = SQLODBCBase::fSQLNumResultCols(hstmt, &num_cols);
	if (!result_ok(rc))
	{
		std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
		close_query();
		throw XQUERY_EXCEPTION2(SE2112, diag.c_str());
	}

	results_count = num_cols;
	results = se_new SQLODBCResult[results_count];

	for (SQLSMALLINT i = 0; i < num_cols; i++)
	{
		rc = SQLODBCBase::fSQLDescribeCol(hstmt, i+1, (SQLCHAR *)results[i].col_name, sizeof(results[i].col_name),
			&results[i].col_name_len, NULL, NULL, NULL, NULL);
		//TODO - use datatype and size
		//TODO - check rc

		if (results[i].col_name_len >= sizeof(results[i].col_name))
			results[i].col_name_len = sizeof(results[i].col_name)-1;

		results[i].col_name[results[i].col_name_len] = '\x0';
	}
	if (res_buf == NULL)
	{
		res_buf_size = 128;
		res_buf = se_new char[res_buf_size];
	}
}

void SQLODBCExecutor::execute_query (const char *query, int query_len, PPOpIn *options)
{
	SQLRETURN rc;

	if (SQL_NULL_HSTMT == hstmt)
		return;

	rc = SQLODBCBase::fSQLExecDirect(hstmt, (SQLCHAR *)query, query_len);
	//SQL_NO_DATA is (sometimes?) returned when an update stmt
	//updated 0 rows (ie delete from empty table)
	if (!result_ok(rc) && rc != SQL_NO_DATA)
	{
		std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
		throw XQUERY_EXCEPTION2(SE2112, diag.c_str());
	}

	prepare_results();
}

//TODO - this is mostly copy-pasted from getStringParameter in PPConstructors
//TODO - rename to smth like getAtomicOrNullParameter
tuple_cell getStringOrNullParameter(PPOpIn content)
{
	tuple value(content.ts);
	content.op->next(value);
	if (value.is_eos())
	{
		tuple_cell result=tuple_cell::eos();
		return result;
	}
	std::vector<tuple_cell> at_vals;
	int charsize=1;

	if (!(value.size()==1 )) throw USER_EXCEPTION2(SE1003, "in PPFnSQLExecute");
	tuple_cell res=atomize(value.cells[0]);
	content.op->next(value);
	if (value.is_eos())
	{
		switch (res.get_atomic_type())
		{
			case xs_double:
				return res;
			default:
				//do nothing here,
				//go to cancatenate strings part
				;
		}
	}
	res=cast(res, xs_string);
	//res=tuple_cell::make_sure_light_atomic(res);
	if (res.is_light_atomic())
		charsize+=res.get_strlen_mem()+1;
	else
		charsize+=res.get_strlen_vmm()+1;
	at_vals.push_back(res);

	while (!(value.is_eos()))
	{
		if (!(value.size()==1 )) throw USER_EXCEPTION2(SE1003, "in PPFnSQLExecute");
		res=atomize(value.cells[0]);

		res=cast(res, xs_string);
		//res=tuple_cell::make_sure_light_atomic(res);
		if (res.is_light_atomic())
			charsize+=res.get_strlen_mem()+1;
		else
			charsize+=res.get_strlen_vmm()+1;
		at_vals.push_back(res);
		content.op->next(value);
	}
	charsize--;
	char* tmp=se_new char[charsize];
	int shift=0;
    for (std::vector<tuple_cell>::size_type i=0;i<at_vals.size();i++)
	{
        at_vals[i].copy_string(tmp+shift);
        shift += at_vals[i].get_strlen();

		if (i<(at_vals.size()-1))
			tmp[shift++]=' ';
	}
	tmp[charsize-1]=0;
	return tuple_cell::atomic(xs_string,tmp);
}

void SQLODBCExecutor::execute_prepared(arr_of_PPOpIn params)
{
	SQLRETURN rc;

	if (params.size() != param_types.size()+1)
		throw XQUERY_EXCEPTION2(SE2106, "bad parameters count");

	std::vector<tuple_cell> values(param_types.size());
    for (arr_of_PPOpIn::size_type id = 1; id < params.size(); id++)
	{
		tuple_cell tmp = getStringOrNullParameter(params[id]);
		values[id-1] = tmp;
		if (tmp.is_eos())
		{
			if (param_types[id-1] == SQL_NULL_DATA) //XXX - SQL_NULL_DATA
			{
				//XXX - we pass 128 as MAX_LEN here...
				set_param_type(id, SQL_C_CHAR, SQL_CHAR, 128);
			}
		}
		else if (tmp.is_atomic())
		{
			switch (tmp.get_atomic_type())
			{
				case xs_double:
					set_param_type(id, SQL_C_DOUBLE, SQL_DOUBLE, 0);
					break;
				case xs_string:
				{
   					//XXX - (int) not safe here !!!
					int len = (int)tmp.get_strlen() + 1;
					set_param_type(id, SQL_C_CHAR, SQL_CHAR, len > 128 ? len : 128);
					break;
				}
				default:
					throw XQUERY_EXCEPTION2(SE2106, "bad parameter type");
			}
		}
		else
			throw XQUERY_EXCEPTION2(SE2106, "bad parameter");

	}

	rc = SQLODBCBase::fSQLExecute(hstmt);
	if (rc == SQL_NEED_DATA) {
		SQLPOINTER data_ptr;
		int ind;

		while (true)
		{
			rc = SQLODBCBase::fSQLParamData(hstmt, &data_ptr);
			ind = (int)(ptrdiff_t)data_ptr; //XXX: we convert int to pointer in SQLBindParameter and pointer to int here
			if (rc != SQL_NEED_DATA)
				break;

			tuple_cell tmp;
			if (ind <= 0 || ind >= (int)params.size())
				throw XQUERY_EXCEPTION(SE2106);

			tmp = values[ind-1];
			if (tmp.is_eos())
				rc = SQLODBCBase::fSQLPutData(hstmt, NULL, SQL_NULL_DATA);
			else if (tmp.is_atomic())
			{
				switch (tmp.get_atomic_type())
				{
					case xs_double:
						{
							double x = tmp.get_xs_double();
							rc = SQLODBCBase::fSQLPutData(hstmt, (SQLPOINTER)&x, sizeof(x));
							break;
						}
					case xs_string:
						//XXX - SQL_NTS - is ok?? or use get_strlen_mem?
						rc = SQLODBCBase::fSQLPutData(hstmt, (SQLCHAR *)tmp.get_str_mem(), SQL_NTS);
						break;
					default:
						throw XQUERY_EXCEPTION2(SE2106, "bad parameter type");
				}
			}
			else
				throw XQUERY_EXCEPTION2(SE2106, "bad parameter");
			if (!result_ok(rc))
			{
				std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
				throw XQUERY_EXCEPTION2(SE2106, diag.c_str());
			}
		}
	}
	//see execute_query for comments about SQL_NO_DATA
	if (!result_ok(rc) && rc != SQL_NO_DATA) {
		std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
		throw XQUERY_EXCEPTION2(SE2106, diag.c_str());
	}

	prepare_results();
}

void SQLODBCExecutor::close_query()
{
	if (active)
	{
		if (results_count > 0)
		{
			if (results != NULL)
				delete results;
			results = NULL;
			results_count = 0;
		}
		SQLODBCBase::fSQLCloseCursor(hstmt); //FIXME - use SQLEndTran too?

		active = false;
	}
}

//TODO - this is not good to limit strings from SQL connection this way
#define MAX_ATTR_LENGTH 65535

void SQLODBCExecutor::fetch(tuple &t, IElementProducer * producer)
{
	SQLRETURN rc;
	rc = SQLODBCBase::fSQLFetch(hstmt);

	if (!result_ok(rc))
	{
		//TODO - throw expection if we're here because of error
		t.set_eos();
		return;
	}

    scoped_ptr<IElementProducer> element = producer->addElement(xsd::QName::createNsN(NULL_XMLNS, "tuple"), xs_untyped);

	for (int i = 0; i < results_count; i++)
	{
		SQLLEN res_len;
		rc = SQLODBCBase::fSQLGetData(hstmt, (SQLUSMALLINT)(i+1), SQL_C_CHAR, res_buf, res_buf_size, &res_len);
		if (!result_ok(rc))
		{
			std::string diag = getODBCDiag(SQL_HANDLE_STMT, hstmt);
			throw XQUERY_EXCEPTION2(SE2112, diag.c_str());
		}

		int offset = 0;
		while (rc == SQL_SUCCESS_WITH_INFO && (res_len > res_buf_size || res_len == SQL_NO_TOTAL))
		{
			if (res_buf_size >= MAX_ATTR_LENGTH)
				break; //give up

			//last char in buf returned by SQLGetData is 0 (i.e. string is truncated)
			offset = res_buf_size-1;

			//increase ref_buf_size
			int new_buf_size = res_buf_size << 1;
			//we need res_len+1 chars, if res_len is known (otherwise it's negative so all's ok)
			while (new_buf_size <= (int)res_len && new_buf_size < MAX_ATTR_LENGTH)
				new_buf_size <<= 1;
			if (new_buf_size > MAX_ATTR_LENGTH)
				new_buf_size = MAX_ATTR_LENGTH;
			//realloc buffer
			char *new_buf = se_new char[new_buf_size];
			//XXX - use stl??
			for (int tmp = 0; tmp < res_buf_size; tmp++)
				new_buf[tmp] = res_buf[tmp];
			delete[] res_buf;
			res_buf = new_buf;
			res_buf_size = new_buf_size;

			rc = SQLODBCBase::fSQLGetData(hstmt, (SQLUSMALLINT)(i+1), SQL_C_CHAR, &res_buf[offset], res_buf_size-offset, &res_len);
			res_len += offset; //SQLGetData returns length of the remaining string
		}
		if (res_len >= 0)
		{
			if (res_len >= res_buf_size)
				res_len = res_buf_size-1;
			res_buf[res_len] = 0;

			//old SQLODBCResult::get -
			//elem = insert_element(left, XNULL, parent, (char *)col_name, xs_untyped, NULL);
			//insert_text(XNULL, XNULL, elem, (char *)res_buf, res_len);
			//return elem;

            element->addAttribute(xsd::QName::createNsN(NULL_XMLNS, (char *) results[i].col_name), text_source_mem(res_buf, res_len), xs_untypedAtomic);
		}
		//if both if's failed assume that res_len == SQL_NULL_DATA
		// thus we don't need to do anything else here
	}

	t.copy(element->close());
}

int  SQLODBCExecutor::update_row_count()
{
	SQLRETURN rc;
	SQLLEN cnt;

	rc = SQLODBCBase::fSQLRowCount(hstmt, &cnt);
	return cnt;
}


bool SQLODBCExecutor::is_query_active()
{
	return active;
}




SQLODBCConnection::SQLODBCConnection(SQLHDBC _hdbc_) : SQLConnection(), hdbc(_hdbc_), open(true)
{
}

//TODO - make sure it never returns NULL (but throws exceptions)
SQLExecutor* SQLODBCConnection::get_executor()
{
	if (SQL_NULL_HDBC == hdbc)
		return NULL;

	return SQLODBCExecutor::create(hdbc);
}

void SQLODBCConnection::release_executor(SQLExecutor *executor)
{
	delete executor;
}

SQLHandle* SQLODBCConnection::prepare_stmt(const char *query, int query_len, PPOpIn *options)
{
	SQLODBCExecutor *executor;

	if (hdbc == SQL_NULL_HDBC)
		throw XQUERY_EXCEPTION2(SE2105, "Connection closed");

	executor = (SQLODBCExecutor *)get_executor();

	executor->prepare(query, query_len, options);

	SQLODBCPreparedStmt *stmt = se_new SQLODBCPreparedStmt(this, executor);
	prepared_stmts.push_back(stmt);

	return stmt;
}

void SQLODBCConnection::close()
{
	SQLRETURN rc;
	//XXX - this suxx if some prepared_stmts deleted
    for (std::vector<SQLHandle *>::size_type i = 0; i < prepared_stmts.size(); i++)
		prepared_stmts[i]->close();
	//TODO - try to release all executors first?
	rc = SQLODBCBase::fSQLDisconnect(hdbc);
	if (!result_ok(rc))
	{
		std::string diag = getODBCDiag(SQL_HANDLE_DBC, hdbc);
		throw XQUERY_EXCEPTION2(SE2108, diag.c_str());
	}
	SQLODBCBase::fSQLFreeHandle(SQL_HANDLE_DBC, hdbc);
	hdbc = SQL_NULL_HDBC;
}
void SQLODBCConnection::commit()
{
	SQLRETURN rc;
	rc = SQLODBCBase::fSQLEndTran(SQL_HANDLE_DBC, hdbc, SQL_COMMIT);
	if (!result_ok(rc))
	{
		std::string diag = getODBCDiag(SQL_HANDLE_DBC, hdbc);
		throw XQUERY_EXCEPTION2(SE2109, diag.c_str());
	}
}
void SQLODBCConnection::rollback()
{
	SQLRETURN rc;
	rc = SQLODBCBase::fSQLEndTran(SQL_HANDLE_DBC, hdbc, SQL_ROLLBACK);
	if (!result_ok(rc))
	{
		std::string diag = getODBCDiag(SQL_HANDLE_DBC, hdbc);
		throw XQUERY_EXCEPTION2(SE2110, diag.c_str());
	}
}

SQLODBCConnection::~SQLODBCConnection()
{
	if (SQL_NULL_HDBC != hdbc)
		close();
}


/////////////////////////////////////////////////
// SQLODBCPreparedStmt

SQLODBCPreparedStmt::SQLODBCPreparedStmt(SQLODBCConnection *_connection_, SQLODBCExecutor *_executor_)
							: SQLHandle(SQLH_PREPARED_STMT), connection(_connection_), executor(_executor_)
{
}

SQLExecutor* SQLODBCPreparedStmt::get_executor()
{
	if (executor == NULL)
		throw XQUERY_EXCEPTION2(SE2106, "Prepared statement not available anymore");
	return executor;
}
void SQLODBCPreparedStmt::release_executor(SQLExecutor *executor)
{
	//do nothing
}
SQLHandle* SQLODBCPreparedStmt::prepare_stmt(const char *query, int query_len, PPOpIn *options)
{
	return connection->prepare_stmt(query, query_len, options);
}

void SQLODBCPreparedStmt::close()
{
	if (executor)
	{
		connection->release_executor(executor);
		executor = NULL;
	}
}
void SQLODBCPreparedStmt::commit()
{
	connection->commit();
}
void SQLODBCPreparedStmt::rollback()
{
	connection->rollback();
}

SQLODBCPreparedStmt::~SQLODBCPreparedStmt()
{
	close();
}

///////////////////////////////////////////
// SQLODBCDriver

extern tuple_cell dm_node_name(xptr node);
SQLConnection*	SQLODBCDriver::new_connection(const char *connect_str, int connect_str_len,
											  const char *uid, int uid_len,
											  const char *pass, int pass_len, PPOpIn *options)
{
	int i;
	int connection_str_len;
	const char *driver = NULL;
	int	driver_len = 0;
	const char *server = NULL;
	int server_len = 0;
	const char *db = NULL;
	int	db_len = 0;
	SQLRETURN rc;
	SQLHDBC hdbc = SQL_NULL_HDBC;
	SQLSMALLINT resp_len;

	SQLODBCBase::load();

	if (!alloc_henv())
		return NULL;

	for (i=0; i < connect_str_len; i++)
		if (':' == connect_str[i])
			break;

	if (i < connect_str_len)
	{
		driver = connect_str;
		driver_len = i;
		i++;
	}
	else
		i = 0;

	if ((connect_str_len-i) > 2 && '/' == connect_str[i] && '/' == connect_str[i+1])
	{
		int j;
		i += 2;

		for (j = i; j < connect_str_len; j++)
			if ('/' == connect_str[j] || ';' == connect_str[j])
				break;

		server = &connect_str[i];
		server_len = j-i;

		if (j < connect_str_len)
		{
			i = j+1;
			if ('/' == connect_str[j])
			{
				for (j = i; j < connect_str_len; j++)
					if (';' == connect_str[j])
						break;
				db = &connect_str[i];
				db_len = j - i;
				if (j < connect_str_len)
					j++;
				i = j;
			}
		}
	}

	connection_str_end = connection_str_buf;

	append_connection_str("DRIVER", driver, driver_len, true);
	append_connection_str("Server", server, server_len);
	append_connection_str("DATABASE", db, db_len);
	append_connection_str("UID", uid, uid_len);
	append_connection_str("PWD", pass, pass_len);
	append_connection_str(NULL, &connect_str[i], connect_str_len - i);

	connection_str_len = connection_str_end - connection_str_buf;

	rc = SQLODBCBase::fSQLAllocHandle(SQL_HANDLE_DBC, henv, &hdbc);
	if (!result_ok(rc) || hdbc == SQL_NULL_HDBC)
	{
		return NULL;
	}

	//XXX - store completed str in separate buffer?
#ifdef LOCK_MEM
	lock_mem_result_t *lmr = lock_mem();
#endif
	//FIXME! - check length
	connection_str_buf[connection_str_len] = 0;
	rc = SQLODBCBase::fSQLDriverConnect(hdbc, NULL, (SQLCHAR *)connection_str_buf, connection_str_len,
		(SQLCHAR *)connection_str_buf, sizeof(connection_str_buf), &resp_len, SQL_DRIVER_NOPROMPT);
	unlock_mem(lmr);

	if (!result_ok(rc))
	{
		std::string diag = getODBCDiag(SQL_HANDLE_DBC, hdbc);
		SQLODBCBase::fSQLFreeHandle(SQL_HANDLE_DBC, hdbc);
		throw XQUERY_EXCEPTION2(SE2111, diag.c_str());
	}

	if (options != NULL)
	{
		tuple t(1);
		//TODO - later there may be some options which should be set before connnecting
		while (1)
		{
			char *name, *value;
			options->op->next(t);
			if (t.is_eos())
				break;

			if (t.size() != 1)
				throw XQUERY_EXCEPTION(SE2100);

			tuple_cell opt = options->get(t);

			if (!SQLOptionParse(opt, name, value))
				throw XQUERY_EXCEPTION(SE2100);

			if (!strcmp(name, "manual-commit"))
			{
				int x;
				if (!value)
				{
					delete[] name;
					throw XQUERY_EXCEPTION(SE2100);
				}
				x = SQLOptionGetBool(value);
				if (x == -1)
				{
					delete[] name;
					delete[] value;
					throw XQUERY_EXCEPTION(SE2100);
				}
				//TODO - set  SQL_AUTOCOMMIT_ON if !x
				if (x)
				{
					rc = SQLODBCBase::fSQLSetConnectAttr(hdbc,
						SQL_ATTR_AUTOCOMMIT,
						SQL_AUTOCOMMIT_OFF,
						SQL_IS_INTEGER);
					if (!result_ok(rc))
					{
						std::string diag = getODBCDiag(SQL_HANDLE_DBC, hdbc);
						SQLODBCBase::fSQLFreeHandle(SQL_HANDLE_DBC, hdbc);

						delete[] name;
						delete[] value;
						//TODO - make another exception type for this
						throw XQUERY_EXCEPTION2(SE2111, diag.c_str());
					}
				}
			}
			else {
				delete[] name;
				if (value)
					delete[] value;
				throw XQUERY_EXCEPTION(SE2100);
			}
			delete[] name;
			if (value)
				delete[] value;
		}
	}

	return se_new SQLODBCConnection(hdbc);
}


void	SQLODBCDriver::append_connection_str(const char *opt, const char *val, int val_len, bool braces)
{
	if (val_len < 1)
		return;
	int left = sizeof(connection_str_buf)-(connection_str_end-connection_str_buf);

	if (connection_str_end > connection_str_buf)
	{
		if (left <= 1)
			return; //TODO - resize
		*connection_str_end = ';';
		connection_str_end++;
		left--;
	}
	if (opt)
	{
		int opt_len = strlen(opt);
		if (left <= opt_len+1)
			return; //TODO - resize
		strncpy(connection_str_end, opt, opt_len);
		connection_str_end += opt_len;
		left -= opt_len;

		*connection_str_end = '=';
		connection_str_end++;
		left--;
	}

	if (braces)
	{
		if (left <= 1)
			return; //TODO - resize
		*connection_str_end = '{';
		connection_str_end++;
		left--;
	}
	if (left <= val_len)
		return; //TODO - resize
	strncpy(connection_str_end, val, val_len);
	connection_str_end += val_len;
	left -= val_len;
	if (braces)
	{
		if (left <= 1)
			return; //TODO - resize
		*connection_str_end = '}';
		connection_str_end++;
		left--;
	}
}
bool SQLODBCDriver::alloc_henv()
{
	if (henv != SQL_NULL_HENV)
		return true;

	SQLRETURN rc;

	rc = SQLODBCBase::fSQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv);
	if (!result_ok(rc))
	{
		henv = SQL_NULL_HENV;
		return false;
	}

	rc = SQLODBCBase::fSQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER)SQL_OV_ODBC3, 0);
	if (!result_ok(rc))
	{
		SQLODBCBase::fSQLFreeHandle(SQL_HANDLE_ENV, henv);
		henv = SQL_NULL_HENV;
		return false;
	}

	return henv != SQL_NULL_HENV;
}

SQLODBCDriver::~SQLODBCDriver()
{
	if (henv != SQL_NULL_HENV)
		SQLODBCBase::fSQLFreeHandle(SQL_HANDLE_ENV, henv);
}


SQLODBCDriver::SQLODBCDriver() : henv(SQL_NULL_HENV)
{
}


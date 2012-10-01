/*
 * File:  FTindex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FTINDEX_H
#define _FTINDEX_H

#include <algorithm>

#include "common/sedna.h"

#include "tr/ft/FTsearch.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/rcv/logican.h"
#include "u/uhdd.h"

#define FTLOG_HEADER		0x010
#define FTLOG_CREATE_BEGIN	0x0001
#define FTLOG_CREATE_END	0x0002
#define FTLOG_CLEAR_BEGIN	0x0003
#define FTLOG_CLEAR_END		0x0004
#define FTLOG_UPDATE_START	0x0005
#define FTLOG_UPDATE_END	0x0006
#define FTLOG_INSERT_START	0x0007
#define FTLOG_INSERT_END	0x0008
#define FTLOG_DELETE_START	0x0009
#define FTLOG_DELETE_END	0x000A
#define FTLOG_COMMIT_START	0x000B
#define FTLOG_ROLLBACK_START	0x000C

struct ftlog_record
{
	int pred_lsn;
	short rec_type;
};

struct ftlog_file
{
	typedef int lsn_t;
	static const lsn_t invalid_lsn = -1;
	UFile file;
	lsn_t last_lsn;
	lsn_t next_lsn;
	char buf[1024];
	int buf_pos;
	ftlog_file() : buf_pos(0), next_lsn(0), last_lsn(invalid_lsn)
	{
	}
	void close_and_delete_file(const char *index_name);
	void start_new_record(short rec_type)
	{
		ftlog_record rec = {last_lsn, rec_type};
		last_lsn = next_lsn;
		write_data(&rec, sizeof(ftlog_record));
	}
	inline void flush()
	{
		write_to_file(buf, buf_pos);
		buf_pos = 0;
	}
	inline void write_data(void *data, int size)
	{
		if ((int)sizeof(buf) - buf_pos < size)
		{
			const int left = sizeof(buf) - buf_pos;
			if (left > 0)
			{
				memcpy(buf + buf_pos, data, left);
				buf_pos += left;
				size -= left;
				next_lsn += left;
				data = (char*)data + left;
			}
			flush();
			if (sizeof(buf) < size)
			{
				next_lsn += size;
				write_to_file(data, size);
				return;
			}
		}
		U_ASSERT((int)sizeof(buf) - buf_pos >= size);
		memcpy(buf + buf_pos, data, size);
		next_lsn += size;
		buf_pos += size;
	}
	void write_xptr_sequence(xptr_sequence* seq);
	//returns non-zero on success
	int read_update_xptr_sequence(update_history *h, update_history::update_type ut);
	//returns non-zero on success
	//buf_pos must be 0
	int seek(lsn_t pos)
	{
		int64_t res_pos;
		int res;
		U_ASSERT(buf_pos == 0);
		res = uSetFilePointer(
                   file,
                   pos,
                   &res_pos,
                   U_FILE_BEGIN,
		   __sys_call_error);
		next_lsn = (lsn_t)res_pos; //FIXME: is it ok to set next_lsn in seek funcs?
		if (res == 0 || res_pos != pos)
			return 0;
		else
			return 1;
	}
	void seek_start()
	{
		if (seek(0) == 0)
			   throw SYSTEM_EXCEPTION("SE4046");
	}
	//returns 0 if failed, 1 if operation is successful
	int read_data(void *data, int size)
	{
		int res;
		unsigned int nread = 0;
		res = uReadFile(file, data, size, &nread, __sys_call_error);
		next_lsn += nread;
		if (res == 0 || nread != size)
			return 0;
		else
			return 1;
	}

	//returns 0 if failed, 1 if operation is successful
	int read_header(LSN *_trans_first_lsn_)
	{
		ftlog_record lrec;
		LSN trans_first_lsn;

		int res = this->read_data(&lrec, sizeof(ftlog_record));
		if (res == 0)
			return 0;

		if (lrec.rec_type != FTLOG_HEADER)
			return 0;

		res = this->read_data(&trans_first_lsn, sizeof(trans_first_lsn));
		if (res == 0)
			return 0;

		*_trans_first_lsn_ = trans_first_lsn;
		return 1;
	}
private:
	inline void write_to_file(void *buf, int size)
	{
		int res;
		unsigned int written;
		res = uWriteFile(file, buf, size, &written, __sys_call_error);
		if (res == 0 || buf_pos != written)
			throw USER_EXCEPTION2(SE4045, "failed to write to full-text index log file");
	}
};

class SednaIndexJob {
     public:
           //SednaIndexJob(PPOpIn* _seq_);
		   SednaIndexJob(ft_index_cell_object * _ft_idx_, bool no_log = false);
		   ~SednaIndexJob();
		   void set_index_name(tuple_cell& request);
		   void create_index(std::vector<xptr> *first_nodes);
		   static int clear_index(const char *index_name);
		   void clear_index();
		   void update_index(xptr_sequence* upserted);
		   void insert_into_index(xptr_sequence* upserted);
		   void delete_from_index(xptr_sequence* deleted);

		   ftlog_file *log_file;
		   static std::map<std::string, ftlog_file*> log_files_map;
		   static UFile create_log(const char *index_name);
		   static ftlog_file *get_log_file(const char *index_name);
		   static void start_commit();
		   static void fix_commit();
		   static void rollback();
		   static void recover_db(trn_cell_analysis_redo *redo_list, bool is_hb);

	  private:
		  SednaDataSource *ds;
		  dtsIndexJob dtind_job;

		  void execute();

		  PPOpIn* seq;
		  const ft_index_cell_object *ft_idx;
		  static void rollback_index(ftlog_file *log_file, const char *index_name);
		  static void recover_db_file(const char *fname, trn_cell_analysis_redo *redo_list);
	   	  static void rebuild_all_ftph();
		  static void rebuild_index(const char *index_name);
		  
     };
#endif

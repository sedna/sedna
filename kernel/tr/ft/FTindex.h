/*
 * File:  FTindex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_INDEX_H
#define _FT_INDEX_H

#include "sedna.h"

#include "FTsearch.h"
#include "xptr_sequence.h"
#include "llmgr_core.h"

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
		if (sizeof(buf) - buf_pos < size)
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
		U_ASSERT(sizeof(buf) - buf_pos >= size);
		memcpy(buf + buf_pos, data, size);
		next_lsn += size;
		buf_pos += size;
	}
	void write_xptr_sequence(xptr_sequence* seq);
	//returns non-zero on success
	int skip_xptr_sequence();
	xptr_sequence *read_xptr_sequence();
	//returns non-zero on success
	int seek(lsn_t pos)
	{
		__int64 res_pos;
		int res;
		U_ASSERT(buf_pos == 0);
		res = uSetFilePointer(
                   file,
                   0,
                   &res_pos,
                   U_FILE_BEGIN,
		   __sys_call_error);
		next_lsn = res_pos;
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
	//returns 0 if failed, 1 if opeation is successful
	int read_data(void *data, int size)
	{
		int res, nread = 0;
		res = uReadFile(file, data, size, &nread, __sys_call_error);
		next_lsn += nread;
		if (res == 0 || nread != size)
			return 0;
		else
			return 1;
	}
private:
	inline void write_to_file(void *buf, int size)
	{
		int res, written;
		res = uWriteFile(file, buf, size, &written, __sys_call_error);
		if (res == 0 || buf_pos != written)
			throw USER_EXCEPTION2(SE4045, "failed to write to full-text index log file");
	}
};

class SednaIndexJob : public dtSearch::DIndexJob {
     public:
           
           //SednaIndexJob(PPOpIn* _seq_);
		   SednaIndexJob(ft_index_cell* _ft_idx_);
		   void set_index_name(tuple_cell& request);
		   void create_index(std::vector<xptr> *first_nodes);
		   static int clear_index(const char *index_name);
		   void clear_index();
		   void update_index(xptr_sequence* upserted);
		   void insert_into_index(xptr_sequence* upserted);
		   void delete_from_index(xptr_sequence* deleted);
		   virtual void OnError(long, const char *, const char *, const char *);

		   ftlog_file *log_file;
		   static std::map<std::string, ftlog_file*> log_files_map;
		   static UFile create_log(const char *index_name);
		   static ftlog_file *get_log_file(const char *index_name);
		   static void start_commit();
		   static void fix_commit();
		   static void rollback();
		   static void recover_db(trns_analysis_map& undo_redo_trns_map);

	  private:
		  PPOpIn* seq;
		  const ft_index_cell *ft_idx;
		  static void rollback_index(ftlog_file *log_file, const char *index_name);
		  static void recover_db_file(const char *fname, trns_analysis_map& undo_redo_trns_map);
		  static void rebuild_index(const char *index_name);
		  
     };
#endif

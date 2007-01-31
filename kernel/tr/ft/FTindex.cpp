/*
 * File:  FTindex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/ft/FTindex.h"
#include "tr/tr_globals.h"
#include "dtsearch/include/dtsearch.h"
#include "common/u/uhdd.h"
#include "tr/log/log.h"
//using namespace dtSearch;

#ifndef _WIN32
#include <sys/types.h>
#include <dirent.h>
#endif

void ftlog_file::write_xptr_sequence(xptr_sequence* seq)
{
	int count = seq->size();
	this->write_data(&count, sizeof(count));
	xptr_sequence::iterator it=seq->begin();
	while (it!=seq->end())
	{
		xptr ptr = *it;
		it++;
		this->write_data(&ptr, sizeof(ptr));
	}
}
int ftlog_file::skip_xptr_sequence()
{
	int count, res;
	res = this->read_data(&count, sizeof(count));
	if (res == 0)
		return 0;
	res = this->seek(this->next_lsn + sizeof(xptr) * count);
	return res;
}

xptr_sequence *ftlog_file::read_xptr_sequence()
{
	xptr_sequence *seq;
	int count, res;
	res = this->read_data(&count, sizeof(count));
	if (res == 0)
		return NULL;

	seq = new xptr_sequence();
	while (count-- > 0)
	{
		xptr ptr;
		res = this->read_data(&ptr, sizeof(xptr));
		if (res == 0)
		{
			delete seq;
			return NULL;
		}
		seq->add(ptr);
	}
	return seq;
}

void ftlog_file::close_and_delete_file(const char *index_name)
{
	if (uCloseFile(file, __sys_call_error) == 0)
		throw USER_EXCEPTION(SE4043);

	char fn[32];
	sprintf(fn, "trn%d_%s.log", trid, index_name);
	std::string log_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	std::string log_path = log_path1 + std::string(fn);

	if (uDeleteFile(log_path.c_str(), __sys_call_error) == 0)
		throw USER_EXCEPTION(SE4041);
}

SednaIndexJob::SednaIndexJob(ft_index_cell* _ft_idx_) : ft_idx(_ft_idx_)
{
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	std::string index_path = index_path1 + std::string(ft_idx->index_title);
	uMkDir(index_path1.c_str(),NULL, __sys_call_error);
	uMkDir(index_path.c_str(),NULL, __sys_call_error);
	this->IndexPath.set(index_path.c_str());

	this->SuppressMessagePump();
	//Create file with trid if already exists abort
	log_file = get_log_file(ft_idx->index_title);

	//FIXME
	dtsOptions opts;
	short result;
	dtssGetOptions(opts, result);
	opts.fieldFlags = dtsoFfSkipFilenameField | dtsoFfXmlSkipAttributes;
	std::string stemming_file = std::string(SEDNA_DATA) + std::string("/data/")
	                + std::string(db_name) + std::string("_files/dtsearch/stemming.dat");
					
	strcpy(opts.stemmingRulesFile, stemming_file.c_str());
	dtssSetOptions(opts, result);
}
void SednaIndexJob::set_index_name(tuple_cell& request)
{
	this->IndexName.set(op_str_buf(request).c_str());
}
void SednaIndexJob::create_index(std::vector<xptr> *first_nodes)
{
	this->SetActionAdd();
	this->SetActionCreate();
	AttachDataSource(new CreationSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, first_nodes), true);
	log_file->start_new_record(FTLOG_CREATE_BEGIN);
	log_file->flush();
	this->Execute();
	log_file->start_new_record(FTLOG_CREATE_END);
	log_file->flush();
}
void SednaIndexJob::OnError(long a, const char * b, const char * c, const char *d)
{
	//d_printf2("error %s\n", c);
}
int SednaIndexJob::clear_index(const char *index_name)
{
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	std::string index_path = index_path1 + std::string(index_name);

	short result;

	dtssDeleteIndex((char *)index_path.c_str(), result);

	return result;
}
void SednaIndexJob::clear_index()
{
	log_file->start_new_record(FTLOG_CLEAR_BEGIN);
	log_file->flush();
	clear_index(ft_idx->index_title);
	log_file->start_new_record(FTLOG_CLEAR_END);
	log_file->flush();
}
void SednaIndexJob::update_index(xptr_sequence* upserted)
{
	this->SetActionAdd();
	this->SetIndexingFlags(dtsAlwaysAdd);
	AttachDataSource(new UpdateSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, upserted), true);
	log_file->start_new_record(FTLOG_UPDATE_START);
	log_file->write_xptr_sequence(upserted);
	log_file->flush();
	this->Execute();
	log_file->start_new_record(FTLOG_UPDATE_END);
	log_file->flush();
}
void SednaIndexJob::insert_into_index(xptr_sequence* upserted)
{
	this->SetActionAdd();
	this->SetIndexingFlags(dtsAlwaysAdd);
	AttachDataSource(new UpdateSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, upserted), true);
	log_file->start_new_record(FTLOG_INSERT_START);
	log_file->write_xptr_sequence(upserted);
	log_file->flush();
	this->Execute();
	log_file->start_new_record(FTLOG_INSERT_END);
	log_file->flush();
}
void SednaIndexJob::delete_from_index(xptr_sequence* deleted)
{
	std::string list_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	std::string list_path = list_path1 + std::string(ft_idx->index_title) + 
		std::string("/remove_list");

	UFile f = uCreateFile(list_path.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH, NULL, __sys_call_error);

	xptr_sequence::iterator it = deleted->begin();
	char tmp_buf[32];
	while (it != deleted->end())
	{
		int wr;
		SednaDataSource::recordToFilename(tmp_buf, *it);
		uWriteFile(f, tmp_buf, strlen(tmp_buf), &wr, __sys_call_error);
		tmp_buf[0] = '\n';
		uWriteFile(f, tmp_buf, 1, &wr, __sys_call_error);

		it++;
	}
	uCloseFile(f, __sys_call_error);
	this->ToRemoveListName.setU8(list_path.c_str());
	this->SetActionRemoveListed(true);
	log_file->start_new_record(FTLOG_DELETE_START);
	log_file->write_xptr_sequence(deleted);
	log_file->flush();
	this->Execute();
	uDeleteFile(list_path.c_str(), __sys_call_error);
	log_file->start_new_record(FTLOG_DELETE_END);
	log_file->flush();
}
UFile SednaIndexJob::create_log(const char *index_name)
{
	char fn[32];
	sprintf(fn, "trn%d_%s.log", trid, index_name);

	std::string log_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	std::string log_path = log_path1 + std::string(fn);

	USECURITY_ATTRIBUTES *sa;
	if(uCreateSA(&sa,U_ALL_ACCESS, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);
	UFile log_file = uCreateFile(log_path.c_str(), 0, U_READ_WRITE, U_WRITE_THROUGH, sa, __sys_call_error);
	if(uReleaseSA(sa, __sys_call_error)!=0) throw USER_EXCEPTION(SE3063);

	if (log_file == U_INVALID_FD)
		throw USER_EXCEPTION2(SE4040, "failed to create log for full-text index operations");
	
	return log_file;
}
std::map<std::string, ftlog_file*> SednaIndexJob::log_files_map;
ftlog_file *SednaIndexJob::get_log_file(const char *index_name)
{
	std::string index_name_str(index_name);
	std::map<std::string, ftlog_file*>::const_iterator it = log_files_map.find(index_name_str);
	if (it == log_files_map.end())
	{
		ftlog_file *lf = new ftlog_file();
		lf->file = create_log(index_name);
		lf->last_lsn = 0;
		log_files_map[index_name_str] = lf;
		lf->start_new_record(FTLOG_HEADER);
		LONG_LSN tran_first_lsn = get_lsn_of_first_record_in_logical_log();
		U_ASSERT(tran_first_lsn != NULL_LSN);
		lf->write_data(&tran_first_lsn, sizeof(tran_first_lsn));
		return lf;
	}
	else
		return it->second;
}

void SednaIndexJob::start_commit()
{
	std::map<std::string, ftlog_file*>::const_iterator it = log_files_map.begin();
	while (it != log_files_map.end())
	{
		it->second->start_new_record(FTLOG_COMMIT_START);
		it++;
	}
}
void SednaIndexJob::fix_commit()
{
	std::map<std::string, ftlog_file*>::const_iterator it = log_files_map.begin();
	while (it != log_files_map.end())
	{
		it->second->close_and_delete_file(it->first.c_str());
		delete it->second;
		it++;
	}
	log_files_map.clear();
}

void SednaIndexJob::rebuild_index(const char *index_name)
{
	clear_index(index_name);
	ft_index_cell* ft_idx = ft_index_cell::find_index(index_name);

	//FIXME: move this to ft_index_data.cpp?
	t_scmnodes sobj = execute_abs_path_expr(ft_idx->schemaroot, ft_idx->object);
	//III. For each schema node found (sn_obj)
	std::vector<xptr> start_nodes;
	for (int i = 0; i < sobj.size(); i++)
	{	
		xptr blk= sobj[i]->bblk;
		CHECKP(blk);
		start_nodes.push_back((GETBLOCKFIRSTDESCRIPTORABSOLUTE(((node_blk_hdr*)XADDR(blk)))));
	}
	
	SednaIndexJob sij(ft_idx);
	sij.create_index(&start_nodes);
}

//pre: log_file must be positioned right after FTLOG_HEADER record
void SednaIndexJob::rollback_index(ftlog_file *log_file, const char *index_name)
{
	ftlog_record lrec;
	ftlog_file::lsn_t cur_lsn;
	ftlog_file::lsn_t last_lsn = ftlog_file::invalid_lsn;
	bool ftindex_is_consistent = true;
	cur_lsn = log_file->next_lsn;
	while (log_file->read_data(&lrec, sizeof(ftlog_record)))
	{
		switch (lrec.rec_type)
		{
		case FTLOG_CREATE_BEGIN:
			clear_index(index_name);
			return;
		case FTLOG_CLEAR_BEGIN:
		case FTLOG_ROLLBACK_START:
			rebuild_index(index_name);
			return;
		case FTLOG_UPDATE_START:
		case FTLOG_INSERT_START:
		case FTLOG_DELETE_START:
			if (log_file->skip_xptr_sequence() == 0)
			{
				rebuild_index(index_name);
				return;
			}
			ftindex_is_consistent = false;
			break;
		case FTLOG_UPDATE_END:
		case FTLOG_INSERT_END:
		case FTLOG_DELETE_END:
		case FTLOG_COMMIT_START:
			ftindex_is_consistent = true;
			break;
		case FTLOG_CREATE_END: //impossible
		case FTLOG_CLEAR_END: //impossible
		default:
			throw SYSTEM_EXCEPTION("bad record in log");
		}
		last_lsn = cur_lsn;
		cur_lsn = log_file->next_lsn;
	}


	if (!ftindex_is_consistent)
	{
		rebuild_index(index_name);
		return;
	}

	if (log_file->seek(cur_lsn) == 0)
		throw SYSTEM_EXCEPTION("failed to seek in ft-log");

	log_file->start_new_record(FTLOG_ROLLBACK_START);
	log_file->flush();

	ft_index_cell* ft_idx = ft_index_cell::find_index(index_name);
	SednaIndexJob sij(ft_idx);
	xptr_sequence *seq;

	while (last_lsn != ftlog_file::invalid_lsn)
	{
		int res;
		if (log_file->seek(last_lsn) == 0)
			throw SYSTEM_EXCEPTION("failed to seek in ft-log");
		res = log_file->read_data(&lrec, sizeof(ftlog_record));
		if (res == 0)
			throw SYSTEM_EXCEPTION("failed to read log");
		switch (lrec.rec_type)
		{
		case FTLOG_UPDATE_START:
		case FTLOG_DELETE_START:
			seq = log_file->read_xptr_sequence();
			if (seq == NULL)
				throw SYSTEM_EXCEPTION("failed to read from ft-log");
			sij.update_index(seq);
			delete seq;
			break;
		case FTLOG_INSERT_START:
			seq = log_file->read_xptr_sequence();
			if (seq == NULL)
				throw SYSTEM_EXCEPTION("failed to read from ft-log");
			sij.delete_from_index(seq);
			delete seq;
			break;
		default:
			break;
		}
		last_lsn = lrec.pred_lsn;
	}
}
void SednaIndexJob::rollback()
{
	std::map<std::string, ftlog_file*>::const_iterator it = log_files_map.begin();
	while (it != log_files_map.end())
	{
		ftlog_file *log_file = it->second;
		log_file->flush();
		log_file->seek_start();
		LONG_LSN tran_first_lsn;
		if (log_file->read_header(&tran_first_lsn))
			rollback_index(log_file, it->first.c_str());
		log_file->close_and_delete_file(it->first.c_str());
		delete log_file;
		it++;
	//1. read the logindex file and crate the ordered list of index operations
	//2. if 0x0001 or 0x0002 operations are existed ->3 else 8
	//3. if the first operation is 0x0001 find index by title in persistent heap
	}
	log_files_map.clear();
}
void SednaIndexJob::recover_db_file(const char *fname, const trns_undo_analysis_list& undo_list, const trns_redo_analysis_list& redo_list, const LONG_LSN& checkpoint_lsn)
{
	char *index_name = new char[strlen(fname)];
	transaction_id trid;
	if (sscanf(fname, "trn%d_%s", &trid, index_name) != 2)
		throw SYSTEM_EXCEPTION("strange file in data/dtsearch folder");
	int l = strlen(index_name);
	if (l <= 4 || strcmp(index_name + l - 4, ".log"))
		throw SYSTEM_EXCEPTION("strange file in data/dtsearch folder");
	index_name[l-4] = 0;

	std::string log_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	std::string log_path = log_path1 + std::string(fname);

	UFile log_ufile = uOpenFile(log_path.c_str(), 0, U_READ_WRITE, U_WRITE_THROUGH, __sys_call_error);

	if (log_ufile == U_INVALID_FD)
		throw SYSTEM_EXCEPTION("failed to open log for full-text index operations");
	
	ftlog_file log_file;
	log_file.file = log_ufile;
	LONG_LSN trans_first_lsn;
	int res = log_file.read_header(&trans_first_lsn);
	bool rollback;
	if (trans_first_lsn > checkpoint_lsn)
	{
		bool f = false;
		trns_redo_analysis_list::const_iterator redo_it = redo_list.begin();
		while (redo_it != redo_list.end())
		{
			if (redo_it->trid == trid && redo_it->trn_end_lsn > trans_first_lsn)
			{
				rollback = false;
				f = true;
				break;
			}

			redo_it++;
		}
		if (!f)
		{
			rollback = true; //it's either nowhere or in undo_list
		}
	}
	else
	{
		bool f = false;
		trns_undo_analysis_list::const_iterator undo_it = undo_list.begin();
		while (undo_it != undo_list.end())
		{
			if (undo_it->trid == trid)
			{
				rollback = true;
				f = true;
				break;
			}
			undo_it++;
		}
		if (!f)
		{
			rollback = false;
		}
	}

	if (rollback)
	{
		rollback_index(&log_file, index_name);
		log_file.close_and_delete_file(index_name);
	}
	else
		log_file.close_and_delete_file(index_name);

	delete[] index_name;
}
void SednaIndexJob::recover_db(const trns_undo_analysis_list& undo_list, const trns_redo_analysis_list& redo_list, const LONG_LSN& checkpoint_lsn)
{
#ifdef _WIN32

    WIN32_FIND_DATA find_data;
    struct file_struct fs;
    UFile fhanldle;
    char buf[20];

	std::string log_path = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/trn*.log");

	fhanldle = FindFirstFile(log_path.c_str(), &find_data);
    if (fhanldle != U_INVALID_FD)
    {
        int found = 1;
        while (found)
        {
			recover_db_file(find_data.cFileName, undo_list, redo_list, checkpoint_lsn);
            found = FindNextFile(fhanldle, &find_data);
        }
        if (FindClose(fhanldle) == 0)
			throw USER_EXCEPTION(SE4043); //FIXME: exception code
    }
#else
	DIR *dir;
	struct dirent *dent;
	std::string log_path = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(db_name) + std::string("_files/dtsearch/");
	dir = opendir(log_path.c_str());
	if (dir != NULL)
		while (NULL != (dent = readdir(dir)))
		{
			int l = strlen(dent->d_name);
			if (l > 4 && !strcmp((char*)dent->d_name + l - 4, ".log"))
					recover_db_file(dent->d_name, undo_list, redo_list, checkpoint_lsn);
		}
	if (0 != closedir(dir))
			throw USER_EXCEPTION(SE4043); //FIXME: exception code
	
#endif
	//1. create list of all files to process
	// 2. cycle on file list
	//3. for each file search the map in order to verify iether the correspondin transaction was commited or not
	//4. in the case of the uncommited transaction start rollback procedure on index
	//5 . end of cycle
	//6. delete all files
}

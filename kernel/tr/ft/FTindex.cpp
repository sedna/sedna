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
#include "tr/cat/catenum.h"
#include "tr/structures/nodeutils.h"

#ifndef _WIN32
#include <sys/types.h>
#include <dirent.h>
#endif

static bool isRecovery = false;

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

int ftlog_file::read_update_xptr_sequence(update_history *h, update_history::update_type ut)
{
	int count, res;
	res = this->read_data(&count, sizeof(count));
	if (res == 0)
		return 0;

	while (count-- > 0)
	{
		xptr ptr;
		res = this->read_data(&ptr, sizeof(xptr));
		if (res == 0)
		{
			return 0;
		}
		h->add_node(ut, ptr);
	}
	return 1;
}

void ftlog_file::close_and_delete_file(const char *index_name)
{
	if (uCloseFile(file, __sys_call_error) == 0)
		throw USER_EXCEPTION(SE4043);

	char fn_part[32];
	sprintf(fn_part, "trn%d_", tr_globals::trid);
	std::string log_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	std::string log_path = log_path1 + std::string(fn_part) + std::string(index_name) + std::string(".log");

	if (uDeleteFile(log_path.c_str(), __sys_call_error) == 0)
		throw USER_EXCEPTION(SE4041);
}

SednaIndexJob::SednaIndexJob(ft_index_cell_object* _ft_idx_, bool no_log) : ft_idx(_ft_idx_), ds(NULL)
{
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	std::string index_path = index_path1 + std::string(ft_idx->index_title);
	uMkDir(index_path1.c_str(),NULL, __sys_call_error);
	uMkDir(index_path.c_str(),NULL, __sys_call_error);

	dtind_job.clear();
	dtind_job.createFlags.relativePaths = true;
	dtind_job.allowConcurrentAccess = true;

	strncpy(dtind_job.indexPath, index_path.c_str(), sizeof(dtind_job.indexPath));

	//Create file with trid if already exists abort
	if (isRecovery || no_log)
		log_file = NULL;
	else
		log_file = get_log_file(ft_idx->index_title);

	//FIXME
	dtsOptions opts;
	short result;
	dtssGetOptions(opts, result);
	if (ft_idx->ftype == ft_xml_hl)
		opts.fieldFlags = dtsoFfXmlHideFieldNames | dtsoFfSkipFilenameField | dtsoFfXmlSkipAttributes;
	else
		opts.fieldFlags = dtsoFfSkipFilenameField | dtsoFfXmlSkipAttributes;
	//FIXME: mb check that length fits into dtsearch options?
	std::string stemming_file = std::string(SEDNA_DATA) + std::string("/data/")
	                + std::string(tr_globals::db_name) + std::string("_files/dtsearch/stemming.dat");

	strcpy(opts.stemmingRulesFile, stemming_file.c_str());
	std::string noisewords_file = std::string(SEDNA_DATA) + std::string("/data/")
	                + std::string(tr_globals::db_name) + std::string("_files/dtsearch/noisewords.dat");

	strcpy(opts.noiseWordFile, noisewords_file.c_str());
	dtssSetOptions(opts, result);
}
SednaIndexJob::~SednaIndexJob()
{
	if (ds)
		delete ds;
}
void SednaIndexJob::set_index_name(tuple_cell& request)
{
	//FIXME: use copy_to_buf in op_str_buf when available
	op_str_buf buf(request);
	strncpy(dtind_job.indexName, buf.c_str(), sizeof(dtind_job.indexName));
}
void SednaIndexJob::execute()
{
	if (!ds)
        dtind_job.dataSourceToIndex = 0;
    else
        dtind_job.dataSourceToIndex = ds->getInterface();

    short result;
    dtssDoIndexJob(dtind_job, result);

}
void SednaIndexJob::create_index(std::vector<xptr> *first_nodes)
{
	dtind_job.action.add = 1;
	dtind_job.action.create = 1;

	if (ds)
		delete ds;
	ds = se_new CreationSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, first_nodes);
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_CREATE_BEGIN);
		log_file->flush();
	}

	this->execute();

	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_CREATE_END);
		log_file->flush();
	}
}
int SednaIndexJob::clear_index(const char *index_name)
{
	std::string index_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	std::string index_path = index_path1 + std::string(index_name);

	short result;

	dtssDeleteIndex((char *)index_path.c_str(), result);

	return result;
}
void SednaIndexJob::clear_index()
{
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_CLEAR_BEGIN);
		log_file->flush();
	}
	clear_index(ft_idx->index_title);
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_CLEAR_END);
		log_file->flush();
	}
}
void SednaIndexJob::update_index(xptr_sequence* upserted)
{
	dtind_job.action.add = 1;
	dtind_job.indexingFlags = dtsAlwaysAdd;
	if (ds)
		delete ds;
	ds = se_new UpdateSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, upserted);
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_UPDATE_START);
		log_file->write_xptr_sequence(upserted);
		log_file->flush();
	}
	this->execute();
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_UPDATE_END);
		log_file->flush();
	}
}
void SednaIndexJob::insert_into_index(xptr_sequence* upserted)
{
	dtind_job.action.add = 1;
	dtind_job.indexingFlags = dtsAlwaysAdd;
	if (ds)
		delete ds;
	ds = se_new UpdateSednaDataSource(ft_idx->ftype, ft_idx->custom_tree, upserted);
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_INSERT_START);
		log_file->write_xptr_sequence(upserted);
		log_file->flush();
	}
	this->execute();
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_INSERT_END);
		log_file->flush();
	}
}
void SednaIndexJob::delete_from_index(xptr_sequence* deleted)
{
	std::string list_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	std::string list_path = list_path1 + std::string(ft_idx->index_title) +
		std::string("/remove_list");

	UFile f = uCreateFile(list_path.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH, NULL, __sys_call_error);

	xptr_sequence::iterator it = deleted->begin();
	char tmp_buf[32];
	while (it != deleted->end())
	{
		unsigned int wr;
		SednaDataSource::recordToFilename(tmp_buf, *it);
		uWriteFile(f, tmp_buf, strlen(tmp_buf), &wr, __sys_call_error);
		tmp_buf[0] = '\n';
		uWriteFile(f, tmp_buf, 1, &wr, __sys_call_error);

		it++;
	}
	uCloseFile(f, __sys_call_error);

	strncpy(dtind_job.toRemoveList, list_path.c_str(), sizeof(dtind_job.toRemoveList));

	dtind_job.action.remove = 1;

	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_DELETE_START);
		log_file->write_xptr_sequence(deleted);
		log_file->flush();
	}
	this->execute();
	uDeleteFile(list_path.c_str(), __sys_call_error);
	if (log_file != NULL)
	{
		log_file->start_new_record(FTLOG_DELETE_END);
		log_file->flush();
	}
}
UFile SednaIndexJob::create_log(const char *index_name)
{
	char fn_part[32];
	sprintf(fn_part, "trn%d_", tr_globals::trid);

	std::string log_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	std::string log_path = log_path1 + std::string(fn_part) + std::string(index_name) + std::string(".log");

	USECURITY_ATTRIBUTES *sa;
	if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0)
        throw USER_EXCEPTION(SE3060);

    UFile log_file = uCreateFile(log_path.c_str(), 0, U_READ_WRITE, U_WRITE_THROUGH, sa, __sys_call_error);

	if(uReleaseSA(sa, __sys_call_error)!=0)
        throw USER_EXCEPTION(SE3063);

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
		ftlog_file *lf = se_new ftlog_file();
		lf->file = create_log(index_name);
		lf->last_lsn = 0;
		log_files_map[index_name_str] = lf;
		lf->start_new_record(FTLOG_HEADER);
		LSN tran_first_lsn = get_lsn_of_first_record_in_logical_log();
		U_ASSERT(tran_first_lsn != LFS_INVALID_LSN);
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
	ftc_index_t ftc_idx;
	ft_index_cell_cptr ft_idx = find_ft_index(index_name, &ftc_idx);
	//TODO:check NULL

	//FIXME: move this to ft_index_data.cpp?
	t_scmnodes sobj;
    executePathExpression(schema_node_cptr(ft_idx->schemaroot), *ft_idx->object, &sobj, NULL, NULL);
	//III. For each schema node found (sn_obj)
	std::vector<xptr> start_nodes;
	for (unsigned int i = 0; i < sobj.size(); i++)
	{
		xptr blk= getNonemptyBlockLookFore(sobj[i]->bblk);
		if (blk != XNULL) {
			start_nodes.push_back(getFirstBlockNode(blk));
		}
	}

	SednaIndexJob sij(&*ft_idx, true);
	sij.create_index(&start_nodes);
}

//pre: log_file must be positioned right after FTLOG_HEADER record
void SednaIndexJob::rollback_index(ftlog_file *log_file, const char *index_name)
{
	ftlog_record lrec;
	ftlog_file::lsn_t cur_lsn;
	update_history uh;
	bool ftindex_is_consistent = true;
	cur_lsn = log_file->next_lsn;
	RECOVERY_CRASH;
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
			if (!log_file->read_update_xptr_sequence(&uh, update_history::ut_update))
			{
				rebuild_index(index_name);
				return;
			}
			ftindex_is_consistent = false;
			break;
		case FTLOG_DELETE_START:
			if (!log_file->read_update_xptr_sequence(&uh, update_history::ut_delete))
			{
				rebuild_index(index_name);
				return;
			}
			ftindex_is_consistent = false;
			break;
		case FTLOG_INSERT_START:
			if (!log_file->read_update_xptr_sequence(&uh, update_history::ut_insert))
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

	ftc_index_t ftc_idx;
	ft_index_cell_cptr ft_idx = find_ft_index(index_name, &ftc_idx);
	//TODO:check NULL

	{
		xptr_sequence *inserted, *updated, *deleted;
		uh.get_update_sequences(&inserted, &updated, &deleted);

		SednaIndexJob sij(&*ft_idx, true);

		if (updated->size() > 0)
			sij.update_index(updated);
		if (deleted->size() > 0)
			sij.update_index(deleted);
		if (inserted->size() > 0)
			sij.delete_from_index(inserted);

		uh.free_update_sequences(inserted, updated, deleted);
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
		LSN tran_first_lsn;
		if (log_file->read_header(&tran_first_lsn)) //FIXME: why errors are ignored here?
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
void SednaIndexJob::recover_db_file(const char *fname, trn_cell_analysis_redo *redo_list)
{
	char *index_name = se_new char[strlen(fname)];
	transaction_id trid;
	if (sscanf(fname, "trn%d_%s", &trid, index_name) != 2)
		throw SYSTEM_EXCEPTION("strange file in data/dtsearch folder");
	int l = strlen(index_name);
	if (l <= 4 || strcmp(index_name + l - 4, ".log"))
		throw SYSTEM_EXCEPTION("strange file in data/dtsearch folder");
	index_name[l-4] = 0;

	std::string log_path1 = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	std::string log_path = log_path1 + std::string(fname);

	UFile log_ufile = uOpenFile(log_path.c_str(), 0, U_READ_WRITE, U_WRITE_THROUGH, __sys_call_error);

	if (log_ufile == U_INVALID_FD)
		throw SYSTEM_EXCEPTION("failed to open log for full-text index operations");

	ftlog_file log_file;
	log_file.file = log_ufile;
	LSN trans_first_lsn;
	int res = log_file.read_header(&trans_first_lsn);
	bool rollback = false;

	trn_cell_analysis_redo *it = redo_list;

	while (it != NULL)
	{
		if (it->trid == trid && it->end_lsn > trans_first_lsn)
			break;

		it = it->next;
	}

	if (it == NULL)
		rollback = true;

	if (rollback)
	{
		rollback_index(&log_file, index_name);
		log_file.close_and_delete_file(index_name);
	}
	else
		log_file.close_and_delete_file(index_name);

	delete[] index_name;
}
void SednaIndexJob::rebuild_all_ftph()
{
    catalog_iterator it(catobj_ft_indicies, false);

	while (it.next())
    {
		rebuild_index(it.get_name());
    }
}

void SednaIndexJob::recover_db(trn_cell_analysis_redo *redo_list, bool is_hb)
{
	if (is_hb) // recovery process from hot-backup copy - need to rebuild all ft-indexes from ph
	{
		rebuild_all_ftph();
		return;
	}

	isRecovery = true;

#ifdef _WIN32

    WIN32_FIND_DATA find_data;
    UFile fhanldle;

	std::string log_path = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/trn*.log");

	fhanldle = FindFirstFile(log_path.c_str(), &find_data);
    if (fhanldle != U_INVALID_FD)
    {
        int found = 1;
        while (found)
        {
			recover_db_file(find_data.cFileName, redo_list);
            found = FindNextFile(fhanldle, &find_data);
        }
        if (FindClose(fhanldle) == 0)
			throw USER_EXCEPTION(SE4043); //FIXME: exception code
    }
#else
	DIR *dir;
	struct dirent *dent;
	std::string log_path = std::string(SEDNA_DATA) + std::string("/data/")
		+ std::string(tr_globals::db_name) + std::string("_files/dtsearch/");
	dir = opendir(log_path.c_str());
	if (dir != NULL)
	{
		while (NULL != (dent = readdir(dir)))
		{
			int l = strlen(dent->d_name);
			if (l > 4 && !strcmp((char*)dent->d_name + l - 4, ".log"))
					recover_db_file(dent->d_name, redo_list);
		}

		if (0 != closedir(dir))
			throw USER_EXCEPTION(SE4043); //FIXME: exception code
	}

#endif
	//1. create list of all files to process
	// 2. cycle on file list
	//3. for each file search the map in order to verify iether the correspondin transaction was commited or not
	//4. in the case of the uncommited transaction start rollback procedure on index
	//5 . end of cycle
	//6. delete all files
}

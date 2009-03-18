/*
 * File:  physrcv.cpp - Physical recovery on sm
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * This functions should be used to recover physical state of a database
 *
 */


#include "sm/llsm/llMain.h"
#include "sm/sm_globals.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/bm_rcv.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "common/base.h"
#include "common/xptr.h"
#include "common/lfsGlobals.h"
#include "common/wutypes.h"
#include "common/sm_vmm_data.h"
#include "sm/wu/wu.h"
#include "common/u/uutils.h"

#include <string>

#ifdef _WIN32
#include <io.h>
#else
#include <sys/types.h>
#include <dirent.h>
#endif

using namespace std;

static int sector_size = 512;
static void *ctrl_blk = NULL;

// Recovery functions for physical records
static void llRcvCheckpoint(LSN lsn, void *RecBuf)
{
	char *offs;
	int is_garbage, count;
	WuVersionEntry *blocks_info;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN); // skip operation code and next-chain-lsn

	is_garbage = *((int *)offs);
	offs += sizeof(int);

	count = *((size_t *)offs);
	offs += sizeof(size_t);

	blocks_info = (WuVersionEntry *)offs;
	for (int i = 0; i < count; i++)
	{
		if (is_garbage)
			push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), WuExternaliseXptr(blocks_info[i].xptr));
		else
		{
			bm_rcv_read_block(WuExternaliseXptr(blocks_info[i].xptr), ctrl_blk);
			bm_rcv_change(WuExternaliseXptr(blocks_info[i].lxptr), ctrl_blk, PAGE_SIZE);
			push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), WuExternaliseXptr(blocks_info[i].xptr));
		}
	}

	RECOVERY_CRASH;
}

static void llRcvFreeBlock(LSN lsn, void *RecBuf)
{
    char *offs;
	int free_blk_info_size;
	void *free_blk_info;
	xptr free_blk_info_xptr;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN);

	free_blk_info_size = *((int *)offs);
	offs += sizeof(int);

	free_blk_info_xptr = *((xptr *)offs);
	offs += sizeof(xptr);

	free_blk_info = offs;

	memcpy(ctrl_blk, free_blk_info, free_blk_info_size); // we need block in sector-aligned buffer here
	bm_rcv_change(free_blk_info_xptr, ctrl_blk, free_blk_info_size);

	RECOVERY_CRASH;
}

static void llRcvPersSnpAdd(LSN lsn, void *RecBuf)
{
    char *offs;
	TIMESTAMP ts;
	WuVersionEntry *blocks_info;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN); // skip operation code and next-chain-lsn

	ts = *((TIMESTAMP *)offs);
	offs += sizeof(TIMESTAMP);

	blocks_info = (WuVersionEntry *)offs;

	bm_rcv_read_block(WuExternaliseXptr(blocks_info->lxptr), ctrl_blk); // read "last" block

	// we must recover block only if it was moved to "blocks_info->xptr" place
	if (ts != ((vmm_sm_blk_hdr *)ctrl_blk)->versionsHeader.creatorTs[0])
	{
		bm_rcv_read_block(WuExternaliseXptr(blocks_info->xptr), ctrl_blk);

		U_ASSERT(
			((vmm_sm_blk_hdr *)ctrl_blk)->versionsHeader.creatorTs[0] == ts &&
			((vmm_sm_blk_hdr *)ctrl_blk)->p == WuExternaliseXptr(blocks_info->lxptr) &&
			((vmm_sm_blk_hdr *)ctrl_blk)->versionsHeader.xptr[0] == blocks_info->lxptr
		);

		bm_rcv_change(WuExternaliseXptr(blocks_info->lxptr), ctrl_blk, PAGE_SIZE);
	}
}

static void llRcvDecrease(LSN lsn, void *RecBuf)
{
	uint64_t old_size;
	char *offs;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN);

	old_size = *((uint64_t *)offs);
	bm_rcv_decrease(old_size);
}

static void llRcvBlock(LSN lsn, void *RecBuf)
{
    char *offs;
	int blk_info_size;
	void *blk_info;
	xptr blk_info_xptr;

	if (!llInfo->hotbackup_needed) return;

	offs = (char *)RecBuf + sizeof(char) + sizeof(LSN);

	blk_info_size = *((int *)offs);
	offs += sizeof(int);

	blk_info_xptr = *((xptr *)offs);
	offs += sizeof(xptr);

	blk_info = offs;

	memcpy(ctrl_blk, blk_info, blk_info_size); // we need block in sector-aligned buffer here
	bm_rcv_change(blk_info_xptr, ctrl_blk, blk_info_size);

	RECOVERY_CRASH;
}

// returns next lsn in physical chain
static LSN llRcvGetNextPhysLsn(LSN curr_lsn, void *RecBuf)
{
	return *((LSN *)((char *)RecBuf + sizeof(char)));
}

// Main structure for physical recovery
static
struct llRecInfo llRcvPhysRecsInfo[] =
{
	{LL_CHECKPOINT,         llRcvCheckpoint},
	{LL_FREE_BLOCKS,        llRcvFreeBlock},
	{LL_PERS_SNAPSHOT_ADD,  llRcvPersSnpAdd},
	{LL_DECREASE,           llRcvDecrease},
	{LL_HBBLOCK,            llRcvBlock},
};
static int llRcvPhysRecsInfoLen = sizeof(llRcvPhysRecsInfo) / sizeof(llRecInfo);

// This function restores persistent snapshot and free blocks information
LSN llRecoverPhysicalState()
{
	char *rec, *offs;
	LSN lsn;
	size_t count;
	void *ctrl_blk_buf;

	lsn = llInfo->checkpoint_lsn;

	if (lsn == LFS_INVALID_LSN)
	{
		// we are in big trouble here :)
		return LFS_INVALID_LSN;
	}

	// recover info from first checkpoint record
	rec = (char *)llGetRecordFromDisc(&lsn);

	if (rec[0] != LL_CHECKPOINT)
		throw USER_EXCEPTION(SE4153);

	offs = rec + sizeof(char) + sizeof(LSN) + sizeof(int);
	count = *((size_t *)offs);
	offs += sizeof(size_t) + sizeof(WuVersionEntry) * count;

	// recover master block
	bm_rcv_master_block(offs);
    offs += sizeof(bm_masterblock);

	// recover minimal lsn for logical recovery
	llInfo->min_rcv_lsn = *((LSN *)offs);

	lsn = llInfo->last_chain_lsn;

	if ((ctrl_blk_buf = malloc(2 * PAGE_SIZE)) == NULL)
		throw SYSTEM_EXCEPTION("Cannot allocate memory");
	// sector alligned buffer
	ctrl_blk = (void *)((uint32_t)((char *)ctrl_blk_buf + sector_size - 1) & ~(sector_size - 1));

	if (uGetDiskSectorSize(&sector_size, sm_globals::db_files_path, __sys_call_error) == 0)
		throw USER_EXCEPTION(SE4051);

	// recover physical state by scaning all physical records
	llScanRecords(llRcvPhysRecsInfo, llRcvPhysRecsInfoLen, lsn, llRcvGetNextPhysLsn, NULL);

	free(ctrl_blk_buf);
	ctrl_blk = NULL;

	// recover temporary file (moved to sm since we restore .setmp even on usual start)
	//bm_rcv_tmp_file();

	// return lsn for logical recovery
	return (llInfo->min_rcv_lsn == LFS_INVALID_LSN) ? llInfo->checkpoint_lsn : llInfo->min_rcv_lsn;
}

void llRcvRestorePh()
{
  uint64_t ph_counter = llInfo->ts;

  char buf3[20];

  string ph_bu_file_name = string(sm_globals::db_files_path) + 
                           string(sm_globals::db_name) + "." +
                           string(u_ui64toa(ph_counter, buf3, 10)) + ".seph";

  string ph_bu_file_name_wo_path = string(sm_globals::db_name) + "." + 
                                   string(u_ui64toa(ph_counter, buf3, 10)) + ".seph";

  string ph_cur_file_name = string(sm_globals::db_files_path) + 
                            string(sm_globals::db_name) + ".seph";

  // delete all other ph files
  string ph_name;
  int64_t number;

  UFile descr;

#ifdef _WIN32

  char buf[4096];
  char buf2[4096];
  char *cur_dir;
  cur_dir  = uGetCurrentWorkingDirectory(buf, 4096, __sys_call_error);

  if (uChangeWorkingDirectory(sm_globals::db_files_path, __sys_call_error) != 0 )
     throw USER_EXCEPTION(SE4604);

  struct _finddata_t ph_file;
  long dsc;

  //find first ph file

  if ( (dsc = _findfirst("*.seph", &ph_file)) == -1L)
     throw USER_EXCEPTION2(SE4044, "persistent heap file");

  do
  {
     if (strcmp(ph_file.name, ph_bu_file_name_wo_path.c_str()))
     	if (uDeleteFile(ph_file.name, __sys_call_error) == 0)
        	throw USER_EXCEPTION(SE4041);

	 RECOVERY_CRASH;

  } while(_findnext(dsc, &ph_file) == 0);


  _findclose(dsc);

  if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
     throw USER_EXCEPTION(SE4604);
#else
  DIR *dir;
  struct dirent* dent;

  dir = opendir(sm_globals::db_files_path);

  if (dir == NULL)
     throw USER_EXCEPTION(SE4604);

  string ext;
  string is_seph;

  while ( NULL != (dent = readdir (dir)) )
  {
     const char *p = NULL;
     ph_name = dent->d_name;

     p = strrchr(ph_name.c_str(),'.');
     if (p == NULL || 0!=strcmp(p,".seph") ) continue;

     if (strcmp(dent->d_name, ph_bu_file_name_wo_path.c_str()))
     	if (uDeleteFile((string(sm_globals::db_files_path) + dent->d_name).c_str(), __sys_call_error) == 0)
        	throw USER_EXCEPTION(SE4041);

	 RECOVERY_CRASH;
  }

  if (0 != closedir(dir))
     throw USER_EXCEPTION2(SE4054, sm_globals::db_files_path);
#endif

  if (uCopyFile(ph_bu_file_name.c_str(), ph_cur_file_name.c_str(), false, __sys_call_error) == 0)
      throw USER_EXCEPTION(SE4306);
}

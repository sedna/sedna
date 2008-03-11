#include <string>

#include <string.h>
#include "sm_llmgr.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/bm_rcv.h"
#include "sm/bufmgr/blk_mngmt.h"

#ifdef _WIN32
#include <io.h>
#else
#include <sys/types.h>
#include <dirent.h>
#endif

#include "common/u/uutils.h"

using namespace std;

// this function restores persistent snapshot and free blocks information
// it returns lsn for logical recovery
//!!! offset correction needed in case of format change
// TODO: check for order of free_blocks_info and blocks_recovery
LONG_LSN sm_llmgr::recover_db_by_phys_records(/*const LONG_LSN& last_cp_lsn,*/ bool sync)
{
  const char *rec;
  const char *body_beg;

  __int64 file_size, old_size;
  
  void *free_blk_info, *ctrl_blk, *ctrl_blk_buf;
  int free_blk_info_size;
  xptr free_blk_info_xptr;

  xptr ctrl_blk_pxptr, ctrl_blk_lxptr;

  ll_log_lock(sync);
  
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

//  DebugBreak();

  logical_log_file_head file_head =
                  read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  LONG_LSN last_checkpoint_lsn = file_head.last_checkpoint_lsn, first_checkpoint_lsn;
  LONG_LSN lsn = last_checkpoint_lsn;


  int state; // state of checkpoint record (stored in the record itself)

//  TIMESTAMP pers_snapshot_ts; // timestamp of persistent snapshot (stored in the record itself)
  size_t num, count;
  int isGarbage;

  char *block_ofs; // pointer to the info about blocks of persistent snapshot 
  LONG_LSN rcvLSN = NULL_LSN; // first LSN from which to start redo analysis (it is also used as an offset in checkpoint record)
  WuVersionEntry *blocks_info; // info about blocks of persistent snapshot
//  VersionsCreateVersionParams ver_info; // used in persistent snapshot recovery
//  std::vector<SnapshotsVersionInfo> blocks_from_checkpoint; // info about blocks from checkppoint record

  if (last_checkpoint_lsn == NULL_LSN) 
  {
    ll_log_unlock(sync);
  	return NULL_LSN;
  }

  do
  {
    set_file_pointer(lsn);
    if( uGetFileSize(ll_curr_file_dsc, &file_size, __sys_call_error) == 0)
       throw SYSTEM_EXCEPTION("Can't get file size");

	int rmndr = lsn % LOG_FILE_PORTION_SIZE;
	
	if (rmndr == file_size)//here we must reinit lsn
      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);
    else if (rmndr == 0)
      lsn += sizeof(logical_log_file_head);

//    if ((lsn%LOG_FILE_PORTION_SIZE) == file_size)//here we must reinit lsn
//      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);

    rec = get_record_from_disk(lsn);
    body_beg = rec + sizeof(logical_log_head);

    if (body_beg[0] != LL_CHECKPOINT)
       throw USER_EXCEPTION(SE4153);
 
    state = *((int *)(body_beg + sizeof(char)));
    block_ofs = const_cast<char *>(body_beg) + sizeof(char) + sizeof(int);

    if (state == 0)
    {
    	bm_rcv_master_block(block_ofs);
    	read_master_block(); // to restore indirecion_table_free_entry

    	block_ofs += sizeof(bm_masterblock);
//    	pers_snapshot_ts = *((TIMESTAMP *)block_ofs);
//    	block_ofs += sizeof(TIMESTAMP);
    	
//    	num = *((size_t *)block_ofs);
//    	block_ofs += sizeof(size_t);

		mem_head->min_rcv_lsn = *((LONG_LSN *)block_ofs);
		block_ofs += sizeof(LONG_LSN);
	}
  	 
    block_ofs += sizeof(int); // isGarbage field
    
    count = *((size_t *)block_ofs);
    block_ofs += sizeof(size_t);

/*    blocks_info = (SnapshotsVersionInfo *)block_ofs;
    for (int i = 0; i < count; i++)
    {
		blocks_from_checkpoint.push_back(blocks_info[i]);    	
    }
*/
    lsn = *((LONG_LSN *)(block_ofs + count * sizeof(WuVersionEntry)));

    RECOVERY_CRASH;

  } while (state != 0);
  
  lsn = file_head.last_chain_lsn;
  
//  ver_info.creationTs = 0;
//  ver_info.alsoUsageSize = 0;
//  ver_info.alsoUsage = NULL;

  char *lsn_offs; // address of the prevLSN field in current physical record

  int sector_size;

  int res = uGetDiskSectorSize(&sector_size, db_files_path.c_str(), __sys_call_error);  
  if (res == 0) 
     throw USER_EXCEPTION(SE4051);
  
  ctrl_blk_buf = se_new_cxt(TopMemoryContext) char[2*PAGE_SIZE];
  
  // sector alligned buffer
  ctrl_blk = (void *)((__uint32) ((char *)ctrl_blk_buf + sector_size - 1) & ~(sector_size - 1));

  while (lsn != NULL_LSN)
  {
    set_file_pointer(lsn);
    if( uGetFileSize(ll_curr_file_dsc, &file_size, __sys_call_error) == 0)
       throw SYSTEM_EXCEPTION("Can't get file size");

	int rmndr = lsn % LOG_FILE_PORTION_SIZE;
	
	if (rmndr == file_size)//here we must reinit lsn
      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);
    else if (rmndr == 0)
      lsn += sizeof(logical_log_file_head);

//    if ((lsn%LOG_FILE_PORTION_SIZE) == file_size)//here we must reinit lsn
//      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);

    rec = get_record_from_disk(lsn);
    body_beg = rec + sizeof(logical_log_head);
    lsn_offs = const_cast<char *>(body_beg);

    if (body_beg[0] == LL_FREE_BLOCKS)
    {
    	free_blk_info_size = *((int *)(body_beg + sizeof(char)));
    	free_blk_info = (void *)(body_beg + sizeof(char) + sizeof(int) + sizeof(xptr));
    	free_blk_info_xptr = *((xptr *)(body_beg + sizeof(char) + sizeof(int)));

    	memcpy(ctrl_blk, free_blk_info, free_blk_info_size);
    	bm_rcv_change(free_blk_info_xptr, ctrl_blk, free_blk_info_size);
        lsn_offs += sizeof(char) + sizeof(int) + sizeof(xptr) + free_blk_info_size;
    }
    else
    if (body_beg[0] == LL_PERS_SNAPSHOT_ADD)
    {
    	TIMESTAMP ts = *((TIMESTAMP *)(body_beg + sizeof(char) + sizeof(WuVersionEntry)));
    	blocks_info = (WuVersionEntry *)(body_beg + sizeof(char));
    	
//    	ver_info.lxptr = blocks_info->lxptr;
//    	ver_info.lastCommitedXptr = blocks_info->xptr;

//    	VeRevertBlock(&ver_info);

//        ctrl_blk = malloc(PAGE_SIZE);
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

        lsn_offs += sizeof(char) + sizeof(WuVersionEntry) + sizeof(TIMESTAMP) + sizeof(int);
    }
    else
    if (body_beg[0] == LL_DECREASE)
    {
    	old_size = *((__int64 *)(body_beg + sizeof(char)));
    	bm_rcv_decrease(old_size);

        lsn_offs += sizeof(char) + sizeof(__int64);
    }
  	else
  	if (body_beg[0] == LL_CHECKPOINT)
  	{
    	state = *((int *)(body_beg + sizeof(char)));
    	lsn_offs = const_cast<char *>(body_beg) + sizeof(char) + sizeof(int);

    	if (state == 0)
    		lsn_offs += sizeof(bm_masterblock) + sizeof(LONG_LSN);
  	 
    	isGarbage = *((int *)lsn_offs);
    	lsn_offs += sizeof(int);

    	count = *((size_t *)lsn_offs);
    	lsn_offs += sizeof(size_t);

    	blocks_info = (WuVersionEntry *)lsn_offs;
    	for (int i = 0; i < count; i++)
    	{
	    	if (isGarbage)
	    		push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), WuExternaliseXptr(blocks_info[i].xptr));
	    	else
	    	{
/*		    	ver_info.lxptr = blocks_info[i].lxptr;
    			ver_info.lastCommitedXptr = blocks_info[i].xptr;

    			VeRevertBlock(&ver_info);*/
		        ctrl_blk = malloc(PAGE_SIZE);
        		bm_rcv_read_block(WuExternaliseXptr(blocks_info[i].xptr), ctrl_blk);
		    	//TODO: change phys_xptr to log_xptr for this block
    			bm_rcv_change(WuExternaliseXptr(blocks_info[i].lxptr), ctrl_blk, PAGE_SIZE);
    			push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), WuExternaliseXptr(blocks_info[i].xptr));
    		}
    	}

    	lsn_offs += count * sizeof(WuVersionEntry);

	    RECOVERY_CRASH;
  	}
  	else
  		throw USER_EXCEPTION(SE4154);

    lsn = *((LONG_LSN *)lsn_offs);
  }
/*
  // recover checkpoint blocks
  for (int i = 0; i < blocks_from_checkpoint.size(); i++)
  {
    	if (blocks_from_checkpoint[i].isGarbage)
    		push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), blocks_from_checkpoint[i].xptr);
    	else
    	{
    		ver_info.lxptr = blocks_from_checkpoint[i].lxptr;
    		ver_info.lastCommitedXptr = blocks_from_checkpoint[i].xptr;

    		VeRevertBlock(&ver_info);
    	}
  }*/
  
  free(ctrl_blk_buf);

  bm_rcv_tmp_file();

  ll_log_unlock(sync);

//  return rcvLSN; // return LSN for recovery by logical log
	return last_checkpoint_lsn; // return lsn for logical recovery
}

void sm_llmgr::restorePh()
{
//  logical_log_file_head file_head =
//                  read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  __uint64 ph_counter = mem_head->ts;

  char buf3[20];

  string ph_bu_file_name = string(db_files_path) + string(db_name) + "." +
  							string(u_ui64toa(ph_counter, buf3, 10)) + ".seph";
  string ph_bu_file_name_wo_path = string(db_name) + "." + string(u_ui64toa(ph_counter, buf3, 10)) + ".seph";


  string ph_cur_file_name = string(db_files_path) + string(db_name) + ".seph";

  // delete all other ph files	
  string ph_name;
  __int64 number;
 
  UFile descr;

#ifdef _WIN32

  char buf[4096];
  char buf2[4096];
  char *cur_dir;
  cur_dir  = uGetCurrentWorkingDirectory(buf, 4096, __sys_call_error);

  if (uChangeWorkingDirectory(db_files_path.c_str(), __sys_call_error) != 0 )
     throw USER_EXCEPTION(SE4604); 

  struct _finddata_t ph_file;
  long dsc;

  //find first ph file

  if ( (dsc = _findfirst("*seph", &ph_file)) == -1L)
     throw USER_EXCEPTION2(SE4044, "persistent heap file");

  do 
  {
//     ph_number = ph_file.name;
//     ph_number = ph_number.erase(0, db_name.size() + 1);
//     ph_number.erase(ph_number.end() - 5, ph_number.end());
//     number = atoi(ph_number.c_str());
//     d_printf3("ph_number_recovered=%s, %d\n", ph_number.c_str(), number);

//     if (number != ph_counter && number != (ph_counter + 1))
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

  dir = opendir(db_files_path.c_str());

  if (dir == NULL)
     throw USER_EXCEPTION(SE4604); 

  string ext;
  string is_seph;

  while ( NULL != (dent = readdir (dir)) )
  {
     const char *p = NULL;
     ph_name = dent->d_name;
//     if (is_seph.size() < 7) continue;
//d_printf2("IS_LLOG=%s\n", is_llog.c_str());

     p = strrchr(ph_name.c_str(),'.');
     if ( p && 0!=strcmp(p,".seph") ) continue;

//     ph_number = ph_number.erase(0, db_name.size() + 1);
//d_printf2("7log_number =%s\n", log_number.c_str());
//     ph_number.erase(ph_number.end() - 5, ph_number.end());
//d_printf2("8log_number=%s\n", log_number.c_str());
//     number = atoi(ph_number.c_str());
//     d_printf3("log_number=%s, %d\n", log_number.c_str(), number);

//     if (number != ph_counter && number != (ph_counter + 1))
     if (strcmp(dent->d_name, ph_bu_file_name_wo_path.c_str()))
     	if (uDeleteFile((db_files_path + dent->d_name).c_str(), __sys_call_error) == 0)
        	throw USER_EXCEPTION(SE4041);
	 
	 RECOVERY_CRASH;
  }

  if (0 != closedir(dir))
     throw USER_EXCEPTION2(SE4054, db_files_path.c_str());
#endif  

  if (uCopyFile(ph_bu_file_name.c_str(), ph_cur_file_name.c_str(), false, __sys_call_error) == 0)
      throw USER_EXCEPTION(SE4306);

//  this->last_checkpoint_ph_counter = ph_counter;  
//  this->ph_file_counter = ph_counter + 1;
}


//!!! offset correction needed in case of format change
void sm_llmgr::ll_log_checkpoint(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage)
//void llmgr_core::ll_log_checkpoint(void *userData, SnapshotsVersionInfo *buf, size_t count)
{
  char *tmp_rec;  
  int rec_len;
  char op = LL_CHECKPOINT;
//  int num = CHARISMA_MAX_TRNS_NUMBER;
  int offs = 0;
  LONG_LSN ret_lsn;
  WuEnumerateVersionsParams *snp_info = params;
  
//  ll_log_lock(sync);  

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  int rec_state = 1;

  if (snp_info->persVersionsSent == 0 && snp_info->garbageVersionsSent == 0)
  	rec_state = 0;

  if (rec_state == 0)
  {
//  	mem_head->number_of_cp_records = 1;
  	mem_head->ts = snp_info->persSnapshotTs;

  	rec_len = sizeof(char) + sizeof(int) + sizeof(bm_masterblock) + sizeof(LONG_LSN) + sizeof(int) + sizeof(size_t) + 
  		sizeof(WuVersionEntry) * count + sizeof(LONG_LSN);
	tmp_rec = ll_log_malloc(rec_len);
  
    inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
    inc_mem_copy(tmp_rec, offs, &rec_state, sizeof(int));
    inc_mem_copy(tmp_rec, offs, mb, sizeof(bm_masterblock));
//    inc_mem_copy(tmp_rec, offs, &(snp_info->persistentSnapshotTs), sizeof(TIMESTAMP));
    inc_mem_copy(tmp_rec, offs, &(mem_head->min_rcv_lsn), sizeof(LONG_LSN));

    inc_mem_copy(tmp_rec, offs, &isGarbage, sizeof(int));
    inc_mem_copy(tmp_rec, offs, &count, sizeof(size_t));

    for (int i = 0; i < count; i++)
	    inc_mem_copy(tmp_rec, offs, &buf[i], sizeof(WuVersionEntry));
  }
  else
  {	
//	mem_head->number_of_cp_records++;

  	rec_len = sizeof(char) + sizeof(int) + sizeof(int) + sizeof(size_t) + 
  		sizeof(WuVersionEntry) * count + sizeof(LONG_LSN);
	tmp_rec = ll_log_malloc(rec_len);

    inc_mem_copy(tmp_rec, offs, &op, sizeof(char));
    inc_mem_copy(tmp_rec, offs, &rec_state, sizeof(int));
    inc_mem_copy(tmp_rec, offs, &isGarbage, sizeof(int));
    inc_mem_copy(tmp_rec, offs, &count, sizeof(size_t));

    for (int i = 0; i < count; i++)
	    inc_mem_copy(tmp_rec, offs, &buf[i], sizeof(WuVersionEntry));
  }

  if (rec_state == 0) mem_head->last_chain_lsn = NULL_LSN;
  
  inc_mem_copy(tmp_rec, offs, &(mem_head->last_chain_lsn), sizeof(LONG_LSN));
  
  //check that log file will not be overflowed
  if (LOG_FILE_PORTION_SIZE - (mem_head->next_lsn - (mem_head->base_addr + (mem_head->ll_files_num -1)*LOG_FILE_PORTION_SIZE)) <
	  (sizeof(logical_log_head) + rec_len))
  {//current log must be flushed and new file created
     ll_log_flush(false);
     extend_logical_log(false);
  }

  ret_lsn = mem_head->next_lsn;

  //insert record in shared memory
  logical_log_head log_head;

  log_head.prev_trn_offs = NULL_OFFS;
  log_head.body_len = rec_len;

  //insert log record into shared memory
  writeSharedMemoryWithCheck(&log_head, tmp_rec);
//  writeSharedMemory(&log_head, sizeof(logical_log_head));
//  writeSharedMemory(tmp_rec, rec_len);

  mem_head->next_lsn += sizeof(logical_log_head) + rec_len;
  
  mem_head->last_lsn = ret_lsn;
  mem_head->last_checkpoint_lsn = ret_lsn;
  mem_head->last_chain_lsn = ret_lsn;

//  ll_log_unlock(sync);

  //std::cout << "ll_log_checkpoint ret_lsn=" << ret_lsn << endl;;

//  return ret_lsn;
} 
